defmodule PhoenixCluster.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application
  require Logger

  alias PhoenixCluster.Products.Cache, as: ProductsCache

  def start(_type, _args) do
    # create cache table
    ProductsCache.init()

    children = [
      # Start the Ecto repository
      PhoenixCluster.Repo,
      # Start the Telemetry supervisor
      PhoenixClusterWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: PhoenixCluster.PubSub},
      # Start the Endpoint (http/https)
      PhoenixClusterWeb.Endpoint
      # Start a worker by calling: PhoenixCluster.Worker.start_link(arg)
      # {PhoenixCluster.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: PhoenixCluster.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def start_phase(phase, _start_type, _phase_args), do: if phase == :load_caches, do: load_caches()

  def load_caches() do
    begin = NaiveDateTime.utc_now()
    PhoenixCluster.Products.list_products()
    |> ProductsCache.load()
    ended = NaiveDateTime.utc_now()

    Logger.info("Products cache loaded in #{NaiveDateTime.diff(ended, begin, :millisecond)} ms")

    :ok
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    PhoenixClusterWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
