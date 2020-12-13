alias PhoenixCluster.Repo
alias PhoenixCluster.Items.Item

now =
  NaiveDateTime.utc_now()
  |> NaiveDateTime.truncate(:second)

items =
  for i <- 1..10000 do
    %{
      id: Ecto.UUID.generate(),
      name: "item#{i}",
      inserted_at: now,
      updated_at: now
    }
  end

Repo.insert_all(Item, items)
