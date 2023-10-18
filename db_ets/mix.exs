defmodule DbEts.MixProject do
  use Mix.Project

  def project do
    [
      app: :db_ets,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :observer, :wx, :runtime_tools],
      mod: {DbEts.Application, []}
    ]
  end

  defp deps do
    [
      {:telemetry_metrics, "~> 0.6.1"},
      {:telemetry_poller, "~> 1.0"},
      {:telemetry_metrics_prometheus, "~> 1.1.0"}
    ]
  end
end
