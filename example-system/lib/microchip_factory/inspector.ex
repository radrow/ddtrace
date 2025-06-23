defmodule MicrochipFactory.Inspector do
  use GenServer
  # alias :ddmon, as: GenServer

  def start_link(reference_microchip, name) do
    GenServer.start_link(__MODULE__, {reference_microchip, name}, name: name)
  end

  def init({reference, name}) do
    {:ok, %{reference: reference, self: name}}
  end

  def handle_call({:inspect_microchip, microchip}, _from, state) do
    :timer.sleep(:rand.uniform(50))

    log(state, "Inspecting microchip #{inspect microchip} compliance with #{MicrochipFactory.Producer.format state.reference}")

    metadata = MicrochipFactory.Producer.get_metadata(state.reference)

    if Integer.gcd(microchip.metadata, metadata) == 1 or microchip.metadata == metadata do
      log(state, "Microchip accepted")
      {:reply, :ok, state}
    else
      {:throw, :bad_microchip, state}
    end
  end

  def format(name) do
    "\e[34;1mInspector #{name}\e[0m"
  end

  defp log(state, str) do
    IO.puts(format(state.self) <> ":  \t" <> str)
  end

  ### Interface

  def inspect_microchip(name, microchip) do
    GenServer.call(name, {:inspect_microchip, microchip})
  end
end
