defmodule PhoenixCluster.Distribution do
  use GenServer
  require Logger

  alias PhoenixCluster.Items.DistCache, as: DistItemsCache

  @release_name    Mix.Project.config |> Keyword.get(:app)
  @update_wait_ms  30_000

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    cache_nodes = nodes_from_env()

    cache_nodes
    |> Enum.each(&Node.connect/1)

    :timer.send_interval(@update_wait_ms, __MODULE__, :check_update)

    initial_state = %{
      cache_nodes: cache_nodes,
      inactive_nodes: MapSet.new(),
      last_change_map: %{},
      reload_requests: %{}
    }
    {:ok, initial_state}
  end

  def set_inactive(node, last_change) do
    if node, do: GenServer.cast(__MODULE__, {:inactive, node, last_change})
  end

  def remote_reloaded(node, request_id) do
    GenServer.cast(__MODULE__, {:remote_reloaded, node, request_id})
  end

  @impl true
  def handle_cast({:inactive, inactive_node, last_change}, state) do
    %{
      inactive_nodes: inactive_nodes,
      last_change_map: last_change_map
    } = state

    new_last_change_map =
      last_change_map
      |> update_last_change(inactive_node, last_change)

    state_change = %{
      inactive_nodes: MapSet.put(inactive_nodes, inactive_node),
      last_change_map: new_last_change_map
    }

    {:noreply, Map.merge(state, state_change)}
  end

  @impl true
  def handle_info(:check_update, state) do
    %{
      cache_nodes: cache_nodes,
      inactive_nodes: inactive_nodes,
      last_change_map: last_change_map,
      reload_requests: reload_requests
    } = state
    # refresh list
    cache_nodes_updated = nodes_from_env()

    # disconnect removed nodes
    disconnect_list = cache_nodes -- cache_nodes_updated

    clean_last_change_map = Enum.reduce(disconnect_list, last_change_map,
      fn node, map ->
        Node.disconnect(node)
        Map.delete(map, node)
      end)

    if [] != disconnect_list, do: Logger.info("[#{__MODULE__}] disconnecting from #{inspect disconnect_list}")

    # connect new nodes
    connect_list = cache_nodes_updated -- cache_nodes

    if [] != connect_list do
      Logger.info("[#{__MODULE__}] connecting to #{inspect connect_list}")
      Enum.each(connect_list, &Node.connect/1)
    end

    # currently connected nodes
    connected_nodes = Node.list()
    Logger.info("[#{__MODULE__}] cluster_nodes: #{inspect cache_nodes_updated}")
    Logger.info("[#{__MODULE__}] nodes_down: #{inspect cache_nodes_updated -- connected_nodes}")

    DistItemsCache.set_active(connected_nodes)

    # notify nodes that were down to reload cache
    connected_set = MapSet.new(connected_nodes)
    previous_pending_set =
      MapSet.new(reload_requests)
      |> MapSet.intersection(connected_set)
    new_pending_set =
      connected_set
      |> MapSet.intersection(inactive_nodes)

    # tracks reload requests
    new_reload_requests =
      MapSet.union(previous_pending_set, new_pending_set)
      |> MapSet.to_list()
      |> reload_remote_caches(last_change_map)

    state_change = %{
      cache_nodes: cache_nodes_updated,
      inactive_nodes: MapSet.new(),
      last_change_map: clean_last_change_map,
      reload_requests: new_reload_requests
    }

    {:noreply, Map.merge(state, state_change)}
  end

  @impl true
  def handle_info({:EXIT, pid, :noconnection}, state) do
    %{
      reload_requests: reload_requests,
      inactive_nodes: inactive_set
    } = state

    inactive_node =
      Map.keys(reload_requests)
      |> Enum.find(
          fn node ->
            request_pid =
              reload_requests
              |> Map.get(node, {nil, nil})
              |> elem(0)

            request_pid == pid
          end)

    {:noreply, %{state | inactive_node: MapSet.put(inactive_set, inactive_node)}}
  end

  @impl true
  def handle_info({:remote_reloaded, node, request_id}, state) do
    %{reload_requests: reload_requests} = state
    requested_id =
      reload_requests
      |> Map.get(node, {nil, nil})
      |> elem(1)

    new_reload_requests =
      if requested_id == request_id do
        Map.delete(reload_requests, node)
      else
        reload_requests
      end
    {:noreply, %{state | reload_requests: new_reload_requests}}
  end

  #
  # Internal
  #
  defp reload_remote_caches(new_connection_nodes, last_change_map) do
    new_connection_nodes
    |> Enum.reduce(Map.new(), fn node, map ->
        last_change = Map.get(last_change_map, node)

        if last_change do
          request_id = Ecto.UUID.bingenerate()
          pid = Node.spawn_link(node, DistItemsCache, :local_load, [last_change, request_id])
          Map.put(map, node, {pid, request_id})
        else
          map
        end
      end)
  end

  defp update_last_change(last_change_map, node, last_change) do
    max_last_change = Map.get(last_change_map, node)

    if nil != max_last_change or NaiveDateTime.compare(last_change, max_last_change) == :gt do
      Map.put(last_change_map, node, last_change)
    else
      last_change_map
    end
  end

  defp nodes_from_env() do
    System.get_env("BEAM_DIST_CACHE_NODES", "#{@release_name}@127.0.0.1")
    |> String.split(",")
    |> Enum.map(&String.to_atom/1)
  end
end
