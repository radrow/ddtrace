defmodule TurnipFactory.Inspector do
  use GenServer
  # alias :ddmon, as: GenServer

  def start_link(reference_turnip) do
    GenServer.start_link(__MODULE__, reference_turnip, [])
  end

  def init(reference) do
    {:ok, %{reference: reference}}
  end

  def handle_call({:inspect_turnip, turnip}, _from, state) do
    :timer.sleep(:rand.uniform(50))

    IO.puts("#{inspect self()} Inspecting turnip #{inspect turnip} compliance with #{inspect state.reference}")

    metadata = GenServer.call(state.reference, :get_metadata)

    if Integer.gcd(turnip.metadata, metadata) == 1 do
      IO.puts("Turnip accepted")
      {:reply, :ok, state}
    else
      {:throw, :bad_turnip, state}
    end
  end
end
