# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :phoenix_cluster,
  ecto_repos: [PhoenixCluster.Repo]

# Configures the endpoint
config :phoenix_cluster, PhoenixClusterWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "BcL1wnvcCbNztoL6VM26dkNTw+vSBNx39I+cpw5F7AQhd4spdeYAkwMLlvP4nOpB",
  render_errors: [view: PhoenixClusterWeb.ErrorView, accepts: ~w(json), layout: false],
  pubsub_server: PhoenixCluster.PubSub,
  live_view: [signing_salt: "3+zrVFvI"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
