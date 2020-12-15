defmodule PhoenixCluster.Items.DistCache do
  use GenServer
  require Logger

  alias PhoenixCluster.Distribution
  alias PhoenixCluster.Items.LocalCache

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    Process.flag(:trap_exit, true)

    initial_state = %{
      active_nodes: [],
      cache_requests: %{},
      last_change: nil,
      last_load: nil,
    }

    {:ok, initial_state}
  end

  #
  # Cache API
  #
  # Functions put/1 and delete/1 in mutual exclusion to load_local/0 and load_local/1
  # in order to avoid overriding changes in progress with old database data.
  #
  def put(item) do
    GenServer.cast(__MODULE__, {:put, item})
  end

  def delete(item_id) do
    GenServer.cast(__MODULE__, {:delete, item_id})
  end

  def load_local() do
    GenServer.cast(__MODULE__, :load_local)
  end

  def load_local(from_node, request_id, remote_last_change) do
    GenServer.cast(__MODULE__, {:load_local, from_node, request_id, remote_last_change})
  end

  #
  # Distribution API
  #
  def set_active(connected_nodes) do
    GenServer.cast(__MODULE__, {:set_active, connected_nodes})
  end

  @impl true
  def handle_cast({:set_active, active_nodes}, state) do
    {:noreply, %{state | active_nodes: active_nodes}}
  end

  #
  # Cache Server
  #
  @impl true
  def handle_cast({operation, item_or_id}, state) when operation in [:put, :delete] do
    %{
      active_nodes: active_nodes,
      cache_requests: requests
    } = state
    # this will execute before or after loading
    apply(LocalCache, operation, [item_or_id])

    node_pid_map =
      for node <- active_nodes, into: %{} do
        # spawn remote process
        pid = Node.spawn_link(node, LocalCache, operation, [item_or_id])
        {node, pid}
      end

    state_change = %{
      cache_requests: Map.merge(requests, node_pid_map),
      last_change: NaiveDateTime.utc_now()
    }

    {:noreply, Map.merge(state, state_change)}
  end

  @impl true
  def handle_cast(:load_local, state) do
    do_load_cache()
    {:noreply, %{state | last_load: NaiveDateTime.utc_now()}}
  end

  @impl true
  def handle_cast({:load_local, from_node, request_id, remote_last_change}, state) do
    %{last_load: last_load} = state

    new_last_load =
      if is_load_needed?(remote_last_change, last_load) do
        do_load_cache()
        NaiveDateTime.utc_now()
      else
        last_load
      end

    Node.spawn(from_node, Distribution, :remote_reloaded, [Node.self(), request_id])

    {:noreply, %{state | last_load: new_last_load}}
  end

  @impl true
  def handle_info({:EXIT, pid, :noconnection}, state) do
    %{
      active_nodes: active_nodes,
      cache_requests: cache_requests,
      last_change: last_change
    } = state

    inactive_node =
      cache_requests
      |> Map.to_list()
      |> Enum.find_value(fn {node, request_pid} ->
          if request_pid == pid, do: node
        end)

    Distribution.set_inactive(inactive_node, last_change)

    {:noreply, %{state | active_nodes: active_nodes -- [inactive_node]}}
  end

  #
  # Internal
  #
  defp is_load_needed?(remote_last_change, last_load) do
    nil == remote_last_change or
    (nil != last_load and NaiveDateTime.compare(remote_last_change, last_load) == :gt)
  end

  defp do_load_cache() do
    # track load time
    {time_microsecs, _val} = :timer.tc(fn ->
      # to optmize might load only updated after disconnection
      PhoenixCluster.Items.list_items()
      |> LocalCache.load()
    end)

    Logger.info("Items cache loaded in #{div(time_microsecs, 1000)} ms")
  end
end
