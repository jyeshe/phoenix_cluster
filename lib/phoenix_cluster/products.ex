defmodule PhoenixCluster.Products do
  @moduledoc """
  The Products context.
  """

  import Ecto.Query, warn: false
  alias PhoenixCluster.Repo

  alias PhoenixCluster.Products.Product
  alias PhoenixCluster.Products.Cache, as: ProductsCache

  @doc """
  Returns the list of products.

  ## Examples

      iex> list_products()
      [%Product{}, ...]

  """
  def list_products do
    Repo.all(Product)
  end

  @doc """
  Gets a single product.

  Raises `Ecto.NoResultsError` if the Product does not exist.

  ## Examples

      iex> get_product!(123)
      %Product{}

      iex> get_product!(456)
      ** (Ecto.NoResultsError)

  """
  def get_product!(id), do: Repo.get!(Product, id)

  @doc """
  Creates a product.

  ## Examples

      iex> create_product(%{field: value})
      {:ok, %Product{}}

      iex> create_product(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_product(attrs \\ %{}) do
    result =
      %Product{}
      |> Product.changeset(attrs)
      |> Repo.insert()

    case result do
      {:ok, product} -> ProductsCache.put(product)
      _ -> :nop
    end

    result
  end

  @doc """
  Updates a product.

  ## Examples

      iex> update_product(product, %{field: new_value})
      {:ok, %Product{}}

      iex> update_product(product, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_product(%Product{} = product, attrs) do
    result =
      product
      |> Product.changeset(attrs)
      |> Repo.update()

    case result do
      {:ok, product} -> ProductsCache.put(product)
      _ -> :nop
    end

    result
  end

  @doc """
  Deletes a product.

  ## Examples

      iex> delete_product(product)
      {:ok, %Product{}}

      iex> delete_product(product)
      {:error, %Ecto.Changeset{}}

  """
  def delete_product(%Product{} = product) do
    result = Repo.delete(product)

    case result do
      {:ok, product} -> ProductsCache.delete(product.id)
      _ -> :nop
    end

    result
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking product changes.

  ## Examples

      iex> change_product(product)
      %Ecto.Changeset{data: %Product{}}

  """
  def change_product(%Product{} = product, attrs \\ %{}) do
    Product.changeset(product, attrs)
  end
end
