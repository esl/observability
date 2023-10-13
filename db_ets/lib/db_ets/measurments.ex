defmodule DbEts.Measurments do
  def dispatch_record_count() do
    :telemetry.execute([:db_ets, :records], %{count: DbEts.count()}, %{})
  end

  def dispatch_record_write do
    :telemetry.execute([:db_ets, :records, :write], %{count: 1}, %{})
  end

  def dispatch_record_read(read_function) do
    :telemetry.span([:db_ets, :records, :read], %{}, fn -> {read_function, %{}} end)
  end
end
