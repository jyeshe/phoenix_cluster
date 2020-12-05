defmodule PhoenixCluster.Distribution do
  use GenServer

  @release_name Mix.Project.config |> Keyword.get(:app)

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end
  def init(:ok) do
    main_nodes =
      System.get_env("PHOENIX_CLUSTER_IPS", "127.0.0.1")
      |> String.split(",")
      |> Enum.map(fn ip -> String.to_atom("#{@release_name}@#{ip}") end)

    Enum.each(main_nodes, fn node -> Node.connect(node) end)

    :timer.send_interval(10_000, __MODULE__, :check_connections)

    {:ok, main_nodes}
  end

  def handle_info(:check_connections, main_nodes) do
    IO.inspect Node.list

    Enum.each(main_nodes, fn node -> Node.connect(node) end)

    {:noreply, main_nodes}
  end
end
