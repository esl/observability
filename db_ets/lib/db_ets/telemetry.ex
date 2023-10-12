defmodule DbEts.Telemetry do
  use Supervisor
  import Telemetry.Metrics
  alias DbEts.Measurments

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    children = [
      {:telemetry_poller, measurements: periodic_measurments(), period: 10_000, name: :db_ets}
      # Uncomment below line to send telemetry events to the console.
      # {Telemetry.Metrics.ConsoleReporter, metrics: metrics()},
      # Uncoment below linve to spint up Prometheus server and enable metrics on http://localhost:9568
      # {TelemetryMetricsPrometheus, [metrics: metrics()]}
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
      # custome metrics
      last_value([:db_ets, :records, :count]),

      # Telemetry Poller VM metrics
      last_value("vm.memory.total", unit: :byte),
      last_value("vm.total_run_queue_lengths.total"),
      last_value("vm.total_run_queue_lengths.cpu"),
      last_value("vm.total_run_queue_lengths.io")
    ]
  end

  defp periodic_measurments do
    [
      # Example of how to set up custome telemetry periodic measurments
      {Measurments, :dispatch_record_count, []}
    ]
  end
end
