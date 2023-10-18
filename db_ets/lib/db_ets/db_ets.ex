defmodule DbEts do
  @moduledoc """
  GenServer implementing simple database using ETS table.
  """
  use GenServer

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
    :telemetry.execute([:db_ets, :records], %{count: 1}, %{})
    :telemetry.execute([:db_ets, :records, :write], %{count: 1}, %{})
    {:noreply, state}
  end

  @impl true
  def handle_cast({:delete, key}, state) do
    :ets.delete(@table_name, key)
    :telemetry.execute([:db_ets, :records], %{count: -1}, %{})
    {:noreply, state}
  end

  @impl true
  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  @impl true
  def handle_call({:read, key}, _from, state) do
    read_function =
      @table_name
      |> :ets.lookup(key)
      |> case do
        [] -> {:error, :not_found}
        [{^key, value}] -> {:ok, value}
      end

    reply = :telemetry.span([:db_ets, :records, :read], %{}, fn -> {read_function, %{}} end)

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
