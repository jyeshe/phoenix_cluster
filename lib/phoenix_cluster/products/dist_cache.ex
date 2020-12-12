defmodule PhoenixCluster.Products.DistCache do
  use GenServer
  require Logger

  alias PhoenixCluster.Distribution
  alias PhoenixCluster.Products.LocalCache

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    Process.flag(:trap_exit, true)

    initial_state = %{
      active_nodes: [],
      last_change: nil,
      last_load: nil
    }

    {:ok, initial_state}
  end

  #
  # Cache API
  #
  # Functions put/1 and delete/1 in mutual exclusion to load_local/0 and load_local/1
  # in order to avoid overriding changes in progress with old database data.
  #
  def put(product) do
    GenServer.cast(__MODULE__, {:put, product})
  end

  def delete(product_id) do
    GenServer.cast(__MODULE__, {:delete, product_id})
  end

  def load_local() do
    GenServer.cast(__MODULE__, :load_local)
  end

  def load_local(remote_last_change) do
    GenServer.cast(__MODULE__, {:load_local, remote_last_change})
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
  def handle_cast({:put, product}, %{active_nodes: active_nodes} = state) do
    # this will execute before or after loading
    LocalCache.put(product)

    node_pid_map =
      for node <- active_nodes, into: %{} do
        # spawn remote process
        pid = Node.spawn_link(node, LocalCache, :put, [product])
        {node, pid}
      end
    {:noreply, new_change_state(state, node_pid_map)}
  end

  @impl true
  def handle_cast({:delete, product_id}, %{active_nodes: active_nodes} = state) do
    # this will execute before or after loading
    LocalCache.delete(product_id)

    node_pid_map =
      for node <- active_nodes, into: %{} do
        # spawn remote process
        pid = Node.spawn_link(node, LocalCache, :delete, [product_id])
        {node, pid}
      end

    {:noreply, new_change_state(state, node_pid_map)}
  end


  @impl true
  def handle_cast(:load_local, state) do
    do_load_cache()
    {:noreply, %{state | last_load: NaiveDateTime.utc_now()}}
  end

  @impl true
  def handle_cast({:load_local, remote_last_change}, %{last_load: last_load} = state) do
    new_last_load =
      if is_load_needed?(remote_last_change, last_load) do
        do_load_cache()
        NaiveDateTime.utc_now()
      else
        last_load
      end

    {:noreply, %{state | last_load: new_last_load}}
  end

  @impl true
  def handle_info({:EXIT, pid, :noconnection}, state) do
    %{active_nodes: active_nodes, last_change: last_change} = state
    inactive_node = Enum.find(active_nodes, fn node -> Map.get(state, node) == pid end)
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
      PhoenixCluster.Products.list_products()
      |> LocalCache.load()
    end)

    Logger.info("Products cache loaded in #{div(time_microsecs, 1000)} ms")

  end

  defp new_change_state(state, node_pid_map) do
    state
    |> Map.merge(node_pid_map)
    |> Map.put(:last_change, NaiveDateTime.utc_now())
  end
end
