defmodule DbEts.Measurments do
  def dispatch_record_count() do
    :telemetry.execute([:db_ets, :records], %{count: DbEts.count()}, %{})
  end
end
