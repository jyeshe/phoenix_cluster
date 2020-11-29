defmodule PhoenixCluster.Products.Cache do

  @table :products_ets

  alias PhoenixCluster.Products.Product

  @spec init :: :ets.tid()
  def init(), do: :ets.new(@table, [:named_table, :public, :set])

  @spec load(list(Product.t)) :: true
  def load(product_list) do
    rows = Enum.map(product_list, fn %Product{} = product -> {product.id, product} end)
    :ets.insert(@table, rows)
  end

  @spec list() :: list(Product.t)
  def list() do
    :ets.match(@table, {:_, :'$1'})
    |> List.flatten()
  end

  @spec put(Product.t) :: true
  def put(%Product{} = product), do: :ets.insert(@table, {product.id, product})

  @spec get(Ecto.UUID.t) :: Product | nil
  def get(product_id) do
    rows = :ets.match(@table, {product_id, :'$1'})

    if rows != [] do
      rows |> hd() |> hd()
    end
  end

  @spec delete(Ecto.UUID.t) :: true
  def delete(product_id) do
    :ets.delete(@table, product_id)
  end
end
