defmodule PhoenixClusterWeb.ProductControllerTest do
  use PhoenixClusterWeb.ConnCase

  alias PhoenixCluster.Products
  alias PhoenixCluster.Products.Product
  alias PhoenixCluster.Products.LocalCache, as: ProductsCache

  @create_attrs %{
    name: "some name"
  }
  @update_attrs %{
    name: "some updated name"
  }
  @invalid_attrs %{name: nil}

  def fixture(name \\ "some name") do
    {:ok, product} = Products.create_product(%{@create_attrs | name: name})
    product
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all products", %{conn: conn} do
      fixture("list_product1")
      fixture("list_product2")

      conn = get(conn, Routes.product_path(conn, :index))
      set_from_response = MapSet.new(json_response(conn, 200)["data"])
      set_from_cache = id_name_set(ProductsCache.list())

      assert MapSet.equal?(set_from_cache, set_from_response)
    end
  end

  defp id_name_set(list) do
    list
    |> Enum.map(fn product -> Map.take(product, [:id, :name]) end)
    |> Jason.encode!
    |> Jason.decode!
    |> MapSet.new()
  end

  describe "create product" do
    test "renders product when data is valid", %{conn: conn} do
      conn = post(conn, Routes.product_path(conn, :create), product: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.product_path(conn, :show, id))

      assert %{
               "id" => _id,
               "name" => "some name"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.product_path(conn, :create), product: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update product" do
    setup [:create_product]

    test "renders product when data is valid", %{conn: conn, product: %Product{id: id} = product} do
      conn = put(conn, Routes.product_path(conn, :update, product), product: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.product_path(conn, :show, id))

      assert %{
               "id" => _id,
               "name" => "some updated name"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, product: product} do
      conn = put(conn, Routes.product_path(conn, :update, product), product: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete product" do
    setup [:create_product]

    test "deletes chosen product", %{conn: conn, product: product} do
      conn = delete(conn, Routes.product_path(conn, :delete, product))
      assert response(conn, 204)


      conn = get(conn, Routes.product_path(conn, :show, product))
      assert response(conn, 404)
    end
  end

  defp create_product(_) do
    product = fixture("product#{Enum.random(1000..9999)}")
    %{product: product}
  end
end
