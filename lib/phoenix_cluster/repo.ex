defmodule PhoenixCluster.Repo do
  use Ecto.Repo,
    otp_app: :phoenix_cluster,
    adapter: Ecto.Adapters.Postgres
end
