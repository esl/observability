defmodule DbEts.Telemetry do
  use Supervisor
  import Telemetry.Metrics

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    children = [
      {:telemetry_poller, measurements: periodic_measurments(), period: 10_000, name: :db_ets},
      # Uncomment below line to send telemetry events to the console.
      # {Telemetry.Metrics.ConsoleReporter, metrics: metrics()},
      # Uncoment below linve to spint up Prometheus server and enable metrics on http://localhost:9568
      {TelemetryMetricsPrometheus, [metrics: metrics()]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def metrics do
    [
      # Prometheus metrics
      distribution("prometheus_metrics.scrape.duration.milliseconds",
        reporter_options: [buckets: [0.05, 0.1, 0.2, 0.5, 1]],
        description: "A histogram of the request duration for prometheus metrics scrape.",
        event_name: [:prometheus_metrics, :plug, :stop],
        measurement: :duration,
        tags: [:name],
        tag_values: fn %{conn: conn} ->
          %{name: conn.private[:prometheus_metrics_name]}
        end,
        unit: {:native, :millisecond}
      ),
      # custom metrics
      sum("db_ets.records.count", reporter_options: [prometheus_type: :gauge]),
      sum("db_ets.records.write.count", reporter_options: [prometheus_type: :counter]),
      distribution("db_ets.records.read.duration",
        reporter_options: [
          buckets: [0.0005, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1]
        ],
        measurement: :duration,
        unit: {:native, :millisecond}
      ),

      # Telemetry Poller VM metrics
      last_value("vm.memory.total", unit: :megabyte),
      last_value("vm.total_run_queue_lengths.total"),
      last_value("vm.total_run_queue_lengths.cpu"),
      last_value("vm.total_run_queue_lengths.io")
    ]
  end

  defp periodic_measurments do
    []
  end
end
