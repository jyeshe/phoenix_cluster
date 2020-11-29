defmodule PhoenixClusterWeb.ProductController do
  use PhoenixClusterWeb, :controller

  alias PhoenixCluster.Products
  alias PhoenixCluster.Products.Product
  alias PhoenixCluster.Products.Cache, as: ProductsCache

  action_fallback PhoenixClusterWeb.FallbackController

  def index(conn, _params) do
    products = ProductsCache.list()
    render(conn, "index.json", products: products)
  end

  def create(conn, %{"product" => product_params}) do
    with {:ok, %Product{} = product} <- Products.create_product(product_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.product_path(conn, :show, product))
      |> render("show.json", product: product)
    end
  end

  def show(conn, %{"id" => id}) do
    product = ProductsCache.get(id)
    if product do
      render(conn, "show.json", product: product)
    else
      send_resp(conn, 404, Plug.Conn.Status.reason_phrase(404))
    end
  end

  def update(conn, %{"id" => id, "product" => product_params}) do
    product = Products.get_product!(id)

    with {:ok, %Product{} = product} <- Products.update_product(product, product_params) do
      ProductsCache.put(product)
      render(conn, "show.json", product: product)
    end
  end

  def delete(conn, %{"id" => id}) do
    product = Products.get_product!(id)

    with {:ok, %Product{}} <- Products.delete_product(product) do
      ProductsCache.delete(id)
      send_resp(conn, :no_content, "")
    end
  end
end
