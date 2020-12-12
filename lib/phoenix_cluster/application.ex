defmodule PhoenixCluster.Application do
  @moduledoc false

  use Application
  require Logger

  alias PhoenixCluster.Products.LocalCache, as: ProductsCache

  def start(_type, _args) do
    # create cache table
    ProductsCache.init()

    # app supervised
    children = [
      PhoenixCluster.Repo,
      PhoenixCluster.Distribution,
      PhoenixCluster.Products.DistCache,
      PhoenixClusterWeb.Telemetry,
      {Phoenix.PubSub, name: PhoenixCluster.PubSub},
      PhoenixClusterWeb.Endpoint,
    ]

    opts = [strategy: :one_for_one, name: PhoenixCluster.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def start_phase(phase, _start_type, _phase_args) do
    if phase == :load_caches do
      load_caches()
    end
  end

  def load_caches() do
    PhoenixCluster.Products.DistCache.load_local()
    :ok
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    PhoenixClusterWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
