defmodule PhoenixClusterWeb.ItemControllerTest do
  use PhoenixClusterWeb.ConnCase

  alias PhoenixCluster.Items
  alias PhoenixCluster.Items.Item
  alias PhoenixCluster.Items.LocalCache, as: ItemsCache

  @create_attrs %{
    name: "some name"
  }
  @update_attrs %{
    name: "some updated name"
  }
  @invalid_attrs %{name: nil}

  def fixture(name \\ "some name") do
    {:ok, item} = Items.create_item(%{@create_attrs | name: name})
    item
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all items", %{conn: conn} do
      fixture("list_item1")
      fixture("list_item2")

      conn = get(conn, Routes.item_path(conn, :index))
      set_from_response = MapSet.new(json_response(conn, 200)["data"])
      set_from_cache = id_name_set(ItemsCache.list())

      assert MapSet.equal?(set_from_cache, set_from_response)
    end
  end

  defp id_name_set(list) do
    list
    |> Enum.map(fn item -> Map.take(item, [:id, :name]) end)
    |> Jason.encode!
    |> Jason.decode!
    |> MapSet.new()
  end

  describe "create item" do
    test "renders item when data is valid", %{conn: conn} do
      conn = post(conn, Routes.item_path(conn, :create), item: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.item_path(conn, :show, id))

      assert %{
               "id" => _id,
               "name" => "some name"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.item_path(conn, :create), item: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update item" do
    setup [:create_item]

    test "renders item when data is valid", %{conn: conn, item: %Item{id: id} = item} do
      conn = put(conn, Routes.item_path(conn, :update, item), item: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.item_path(conn, :show, id))

      assert %{
               "id" => _id,
               "name" => "some updated name"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, item: item} do
      conn = put(conn, Routes.item_path(conn, :update, item), item: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete item" do
    setup [:create_item]

    test "deletes chosen item", %{conn: conn, item: item} do
      conn = delete(conn, Routes.item_path(conn, :delete, item))
      assert response(conn, 204)


      conn = get(conn, Routes.item_path(conn, :show, item))
      assert response(conn, 404)
    end
  end

  defp create_item(_) do
    item = fixture("item#{Enum.random(1000..9999)}")
    %{item: item}
  end
end
