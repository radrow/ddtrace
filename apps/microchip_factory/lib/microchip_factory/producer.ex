defmodule MicrochipFactory.Producer do
  use GenServer

  def start_link(name, microchip_metadata, component_producers) do
    GenServer.start_link(__MODULE__, {microchip_metadata, component_producers, name}, name: name)
  end

  def init({microchip_metadata, component_producers, name}) do
    {:ok, %{metadata: microchip_metadata, component_producers: component_producers, self: name}}
  end

  def handle_call({:produce_microchip, inspector}, _from, state) do
    log(state, "Producing microchip...")
    :timer.sleep(:rand.uniform(50))

    components = for component_producer <- state.component_producers do
      log(state, "Producing microchip... obtaining component from #{MicrochipFactory.Producer.format component_producer}")
      {:ok, chip} = MicrochipFactory.Producer.produce_microchip(component_producer, inspector)
      chip.components
    end

    microchip = %{metadata: state.metadata, components: Enum.sum(components) + 1}

    log(state, "Producing microchip... asking #{MicrochipFactory.Inspector.format inspector} for audit")
    :ok = MicrochipFactory.Inspector.inspect_microchip(inspector, microchip)

    {:reply, {:ok, microchip}, state}
  end
  def handle_call(:get_metadata, _from, state) do
    log(state, "Giving producer metadata")
    {:reply, state.metadata, state}
  end


  def format({:via, _, {_, name}}) do
    format(name)
  end
  def format(name) do
    "\e[35;1mProducer #{inspect name}\e[0m"
  end

  defp log(state, str) do
    IO.puts(format(state.self) <> ":  \t" <> str)
  end

  ### Interface

  def produce_microchip(name, inspector) do
    GenServer.call(name, {:produce_microchip, inspector}, :infinity)
  end

  def get_metadata(name) do
    GenServer.call(name, :get_metadata, :infinity)
  end
end
