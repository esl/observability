defmodule DbEts do
  @moduledoc """
  GenServer implementing simple database using ETS table.
  """
  use GenServer
  alias DbEts.Measurments

  @table_name :db_ex

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def stop do
    GenServer.cast(__MODULE__, :stop)
  end

  def write(key, element) do
    GenServer.cast(__MODULE__, {:write, key, element})
  end

  def delete(key) do
    GenServer.cast(__MODULE__, {:delete, key})
  end

  def read(key) do
    GenServer.call(__MODULE__, {:read, key})
  end

  def match(element) do
    GenServer.call(__MODULE__, {:match, element})
  end

  @impl true
  def init(_opts) do
    :ets.new(@table_name, [:named_table, :set])
    {:ok, []}
  end

  @impl true
  def terminate(_reason, _state) do
    :ets.delete(@table_name)
  end

  @impl true
  def handle_cast({:write, key, element}, state) do
    :ets.insert(@table_name, {key, element})
    Measurments.dispatch_record_write()
    {:noreply, state}
  end

  @impl true
  def handle_cast({:delete, key}, state) do
    :ets.delete(@table_name, key)
    Measurments.dispatch_record_delete()
    {:noreply, state}
  end

  @impl true
  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  @impl true
  def handle_call({:read, key}, _from, state) do
    start = System.monotonic_time()

    reply =
      @table_name
      |> :ets.lookup(key)
      |> case do
        [] -> {:error, :not_found}
        [{^key, value}] -> {:ok, value}
      end

    duration = System.monotonic_time() - start
    Measurments.dispatch_record_read(duration)

    {:reply, reply, state}
  end

  @impl true
  def handle_call({:match, element}, _from, state) do
    reply =
      @table_name
      |> :ets.match({:"$1", element})
      |> List.flatten()

    {:reply, reply, state}
  end
end
