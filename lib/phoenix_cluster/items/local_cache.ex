defmodule PhoenixCluster.Items.LocalCache do

  @table :items_ets

  alias PhoenixCluster.Items.Item

  @spec init :: :ets.tid()
  def init(), do: :ets.new(@table, [:named_table, :public, :set])

  @spec load(list(Item.t)) :: true
  def load(item_list) do
    rows = Enum.map(item_list, fn %Item{} = item -> {item.id, item} end)
    :ets.insert(@table, rows)
  end

  @spec list() :: list(Item.t)
  def list() do
    :ets.match(@table, {:_, :'$1'})
    |> List.flatten()
  end

  @spec put(Item.t) :: true
  def put(%Item{} = item), do: :ets.insert(@table, {item.id, item})

  @spec get(Ecto.UUID.t) :: Item | nil
  def get(item_id) do
    rows = :ets.match(@table, {item_id, :'$1'})

    if rows != [] do
      rows |> hd() |> hd()
    end
  end

  @spec delete(Ecto.UUID.t) :: true
  def delete(item_id) do
    :ets.delete(@table, item_id)
  end
end
