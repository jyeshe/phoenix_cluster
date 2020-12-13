defmodule PhoenixCluster.Items do
  @moduledoc """
  The Items context.
  """

  import Ecto.Query, warn: false
  alias PhoenixCluster.Repo

  alias PhoenixCluster.Items.Item
  alias PhoenixCluster.Items.DistCache

  @doc """
  Returns the list of items.

  ## Examples

      iex> list_items()
      [%Item{}, ...]

  """
  def list_items do
    Repo.all(Item)
  end

  @doc """
  Gets a single item.

  Raises `Ecto.NoResultsError` if the Item does not exist.

  ## Examples

      iex> get_item!(123)
      %Item{}

      iex> get_item!(456)
      ** (Ecto.NoResultsError)

  """
  def get_item!(id), do: Repo.get!(Item, id)

  @doc """
  Creates a item.

  ## Examples

      iex> create_item(%{field: value})
      {:ok, %Item{}}

      iex> create_item(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_item(attrs \\ %{}) do
    result =
      %Item{}
      |> Item.changeset(attrs)
      |> Repo.insert()

    case result do
      {:ok, item} -> DistCache.put(item)
      _ -> :nop
    end

    result
  end

  @doc """
  Updates a item.

  ## Examples

      iex> update_item(item, %{field: new_value})
      {:ok, %Item{}}

      iex> update_item(item, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_item(%Item{} = item, attrs) do
    result =
      item
      |> Item.changeset(attrs)
      |> Repo.update()

    case result do
      {:ok, item} -> DistCache.put(item)
      _ -> :nop
    end

    result
  end

  @doc """
  Deletes a item.

  ## Examples

      iex> delete_item(item)
      {:ok, %Item{}}

      iex> delete_item(item)
      {:error, %Ecto.Changeset{}}

  """
  def delete_item(%Item{} = item) do
    result = Repo.delete(item)

    case result do
      {:ok, item} -> DistCache.delete(item.id)
      _ -> :nop
    end

    result
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking item changes.

  ## Examples

      iex> change_item(item)
      %Ecto.Changeset{data: %Item{}}

  """
  def change_item(%Item{} = item, attrs \\ %{}) do
    Item.changeset(item, attrs)
  end
end
