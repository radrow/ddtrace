defmodule MicrochipFactory.Inspector do
  use GenServer
  # alias :ddmon, as: GenServer

  def start_link(reference_microchip) do
    GenServer.start_link(__MODULE__, reference_microchip, [])
  end

  def init(reference) do
    {:ok, %{reference: reference}}
  end

  def handle_call({:inspect_microchip, microchip}, _from, state) do
    :timer.sleep(:rand.uniform(50))

    IO.puts("#{inspect self()} Inspecting microchip #{inspect microchip} compliance with #{inspect state.reference}")

    metadata = GenServer.call(state.reference, :get_metadata)

    if Integer.gcd(microchip.metadata, metadata) == 1 do
      IO.puts("Microchip accepted")
      {:reply, :ok, state}
    else
      {:throw, :bad_microchip, state}
    end
  end
end
