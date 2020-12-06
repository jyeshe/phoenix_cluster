defmodule PhoenixCluster.Distribution do
  use GenServer
  require Logger

  @modname         "distribution"
  @release_name    Mix.Project.config |> Keyword.get(:app)
  @update_wait_ms  30_000

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    main_nodes = nodes_from_env()

    Enum.each(main_nodes, fn node -> Node.connect(node) end)

    :timer.send_interval(@update_wait_ms, __MODULE__, :check_update)

    {:ok, main_nodes}
  end

  def handle_info(:check_update, main_nodes) do
    old_list = main_nodes
    new_list = nodes_from_env()

    # nodes removed
    disconnect_list = old_list -- new_list

    if [] != disconnect_list do
      Logger.info("[#{@modname}] disconnecting from #{inspect disconnect_list}")
      Enum.each(disconnect_list, fn node -> Node.disconnect(node) end)
    end

    # nodes added
    connect_list = old_list -- new_list

    if [] != connect_list do
      Logger.info("[#{@modname}] connecting to #{inspect connect_list}")
      Enum.each(connect_list, fn node -> Node.connect(node) end)
    end

    Logger.info("[#{@modname}] connected to #{inspect Node.list()}")

    {:noreply, main_nodes}
  end

  defp nodes_from_env() do
    System.get_env("BEAM_DIST_MAIN_IPS", "127.0.0.1")
    |> String.split(",")
    |> Enum.map(fn ip -> String.to_atom("#{@release_name}@#{ip}") end)
  end
end
