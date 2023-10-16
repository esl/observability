defmodule DbEts.Measurments do
  def dispatch_record_write do
    :telemetry.execute([:db_ets, :records], %{count: 1}, %{})
    :telemetry.execute([:db_ets, :records, :write], %{count: 1}, %{})
  end

  def dispatch_record_delete do
    :telemetry.execute([:db_ets, :records], %{count: -1}, %{})
  end

  def dispatch_record_read(duration) do
    :telemetry.execute([:db_ets, :records, :read], %{duration: duration}, %{})
  end
end
