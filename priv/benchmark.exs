defmodule Batch do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok), do: {:ok, []}

  def add(product) do
    GenServer.cast(__MODULE__, {:add, product})
  end

  def consume() do
    GenServer.call(__MODULE__, :consume)
  end

  def handle_cast({:add, product}, state) do
    # if order does not mater invert to [product | state]
    {:noreply, state ++ [product]}
  end

  def handle_call(:consume, _from, state) do
    {:reply, state, []}
  end
end

Batch.start_link()

alias PhoenixCluster.Products.{Cache, Product}

# local test:
#
# 1. start server1 phoenix with:
#   $ export RELEASE_COOKIE="ZIHU3OYHBBZPWDYQODVOXENLRDONHKRY6I6M23QPU6SIZTJPBBBQ===" RELEASE_NODE='server1@127.0.0.1'
#   $ export PHX_PORT=4001 BEAM_PORT=4371;
#   $ bin/phoenix_cluster start
# 2. start server2 phoenix with:
#   $ export RELEASE_COOKIE="ZIHU3OYHBBZPWDYQODVOXENLRDONHKRY6I6M23QPU6SIZTJPBBBQ===" RELEASE_NODE='server2@127.0.0.1'
#   $ export PHX_PORT=4002 BEAM_PORT=4372;
#   $ bin/phoenix_cluster start
# 3. start this iex with: $ elixir --name iex@127.0.0.1 --cookie ZIHU3OYHBBZPWDYQODVOXENLRDONHKRY6I6M23QPU6SIZTJPBBBQ=== -S mix run priv/benchmark.exs

make_new_list = fn ->
  for i <- 1..10000 do
    %Product{
      id: Ecto.UUID.generate,
      name: "product#{i}",
      inserted_at: NaiveDateTime.utc_now,
      updated_at: NaiveDateTime.utc_now
    }
  end
end

list1 = make_new_list.()
chunks1 = make_new_list.() |> Enum.chunk_every(100)
list2 = make_new_list.()
chunks2 = make_new_list.() |> Enum.chunk_every(100)

Node.connect(:"server1@127.0.0.1")
Node.connect(:"server2@127.0.0.1")
nodes = Node.list()
[node1, node2] = nodes
IO.inspect nodes

Benchee.run(%{
  "one_by_one - 1 replica" => fn -> [node1] |> Enum.each(fn node -> Enum.each(list1, &Node.spawn(node, Cache, :put, [&1])) end) end,
  "chuncks - 1 replica" => fn ->
    Enum.each(chunks1, fn chunk ->
      Enum.each(chunk, fn product -> Batch.add(product) end)
      Node.spawn(node2, Cache, :load, [Batch.consume()])
    end)
  end,
  "one_by_one - 2 replicas" => fn -> nodes |> Enum.each(fn node -> Enum.each(list2, &Node.spawn(node, Cache, :put, [&1])) end) end,
  "chuncks - 2 replicas" => fn ->
    Enum.each(nodes, fn node ->
      Enum.each(chunks2, fn chunk ->
        Enum.each(chunk, fn product -> Batch.add(product) end)
        Node.spawn(node, Cache, :load, [Batch.consume()])
      end)
    end)
  end
})
