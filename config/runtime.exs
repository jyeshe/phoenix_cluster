import Config

# For itemion, don't forget to configure the url host
# to something meaningful, Phoenix uses this information
# when generating URLs.
#
# Note we also include the path to a cache manifest
# containing the digested version of static files. This
# manifest is generated by the `mix phx.digest` task,
# which you should run after static files are built and
# before starting your itemion server.
config :phoenix_cluster, PhoenixClusterWeb.Endpoint,
  url: [port: System.get_env("PHX_PORT")],
  debug_errors: true,
  code_reloader: false,
  check_origin: false,
  watchers: []
  # cache_static_manifest: "priv/static/cache_manifest.json"

# Do not print debug messages in itemion
# config :logger, level: :info
