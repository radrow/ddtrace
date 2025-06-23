defmodule MicrochipFactory.Producer do
  use GenServer
  # alias :ddmon, as: GenServer

  def start_link(microchip_metadata) do
    GenServer.start_link(__MODULE__, microchip_metadata, [])
  end

  def init(microchip_metadata) do
    {:ok, %{metadata: microchip_metadata}}
  end

  def handle_call({:produce_microchip, inspector}, _from, state) do
    IO.puts("#{inspect self()} Producing microchip... asking inspector #{inspect inspector} for audit")
    :timer.sleep(:rand.uniform(500))

    microchip = %{metadata: state.metadata, mass: :rand.uniform(50)}

    :ok = GenServer.call(inspector, {:inspect_microchip, microchip})

    {:reply, {:ok, microchip}, state}
  end

  def handle_call(:get_metadata, _from, state) do
    IO.puts("#{inspect self()} Giving producer metadata")
    {:reply, state.metadata, state}
  end
end
