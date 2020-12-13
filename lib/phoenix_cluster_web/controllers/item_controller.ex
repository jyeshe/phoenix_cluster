defmodule PhoenixClusterWeb.ItemController do
  use PhoenixClusterWeb, :controller

  alias PhoenixCluster.Items
  alias PhoenixCluster.Items.Item
  alias PhoenixCluster.Items.LocalCache, as: ItemsCache

  action_fallback PhoenixClusterWeb.FallbackController

  def index(conn, _params) do
    items = ItemsCache.list()
    render(conn, "index.json", items: items)
  end

  def create(conn, %{"item" => item_params}) do
    with {:ok, %Item{} = item} <- Items.create_item(item_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.item_path(conn, :show, item))
      |> render("show.json", item: item)
    end
  end

  def show(conn, %{"id" => id}) do
    item = ItemsCache.get(id)
    if item do
      render(conn, "show.json", item: item)
    else
      send_resp(conn, 404, Plug.Conn.Status.reason_phrase(404))
    end
  end

  def update(conn, %{"id" => id, "item" => item_params}) do
    item = Items.get_item!(id)

    with {:ok, %Item{} = item} <- Items.update_item(item, item_params) do
      ItemsCache.put(item)
      render(conn, "show.json", item: item)
    end
  end

  def delete(conn, %{"id" => id}) do
    item = Items.get_item!(id)

    with {:ok, %Item{}} <- Items.delete_item(item) do
      ItemsCache.delete(id)
      send_resp(conn, :no_content, "")
    end
  end
end
