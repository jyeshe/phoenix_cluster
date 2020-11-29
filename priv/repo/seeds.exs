alias PhoenixCluster.Repo
alias PhoenixCluster.Products.Product

now =
  NaiveDateTime.utc_now()
  |> NaiveDateTime.truncate(:second)

products =
  for i <- 1..10000 do
    %{
      id: Ecto.UUID.generate(),
      name: "product#{i}",
      inserted_at: now,
      updated_at: now
    }
  end

Repo.insert_all(Product, products)
