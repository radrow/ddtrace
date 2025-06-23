defmodule TurnipFactory.Producer do
  use GenServer
  alias :ddmon, as: GenServer

  def start_link(turnip_metadata) do
    GenServer.start_link(__MODULE__, turnip_metadata, [])
  end

  def init(turnip_metadata) do
    {:ok, %{metadata: turnip_metadata}}
  end

  def handle_call({:produce_turnip, inspector}, _from, state) do
    IO.puts("#{inspect self()} Producing turnip... asking inspector #{inspect inspector} for audit")
    :timer.sleep(:rand.uniform(500))

    turnip = %{metadata: state.metadata, mass: :rand.uniform(50)}

    :ok = GenServer.call(inspector, {:inspect_turnip, turnip})

    {:reply, {:ok, turnip}, state}
  end

  def handle_call(:get_metadata, _from, state) do
    IO.puts("#{inspect self()} Giving producer metadata")
    {:reply, state.metadata, state}
  end
end
