defmodule ElephantPatrol.Drone do
  @moduledoc """
  A GenServer representing a drone that observes an elephant.

  When the drone detects the elephant destroying crops, it asks its controller
  for permission to scare it off.
  """
  use GenServer
  require Logger

  # Cyan color for drones
  @color IO.ANSI.cyan()
  @reset IO.ANSI.reset()

  defstruct [:name, :elephant, :controller]

  # Client API

  @doc """
  Starts a Drone process.

  Options:
  - `:name` - Optional name for the process
  - `:elephant` - The elephant process to observe (pid or name)
  - `:controller` - The controller process to report to (pid or name)
  """
  def start_link(opts) do
    name = Keyword.get(opts, :name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Asks the drone to observe the elephant and report if it's destroying crops.
  If the elephant is destroying crops, the drone will ask its controller for
  permission to scare it off.

  Returns:
  - `{:ok, :calm}` if the elephant is calm
  - `{:ok, :scared_off}` if the elephant was destroying crops and was scared off
  - `{:ok, :not_approved}` if the controller did not approve scaring the elephant
  """
  def observe(drone) do
    GenServer.call(drone, :observe, 20_000)
  end

  @doc """
  Asks the drone to confirm whether it sees the elephant destroying crops.
  Used by controllers to get a second opinion.
  """
  def confirm_sighting(drone) do
    GenServer.call(drone, :confirm_sighting, 20_000)
  end

  # Server Callbacks

  @impl true
  def init(opts) do
    name = Keyword.get(opts, :name, self())
    elephant = Keyword.fetch!(opts, :elephant)
    controller = Keyword.fetch!(opts, :controller)
    state = %__MODULE__{name: format_name(name), elephant: elephant, controller: controller}
    Logger.info("#{@color}[#{state.name}] 🚁 Drone initialized | elephant=#{inspect(elephant)} controller=#{inspect(controller)}#{@reset}")
    {:ok, state}
  end

  @impl true
  def handle_call(:observe, _from, state) do
    Logger.info("#{@color}[#{state.name}] 🚁 Starting observation of elephant...#{@reset}")
    result = do_observe(state)
    Logger.info("#{@color}[#{state.name}] 🚁 Observation complete | result=#{inspect(result)}#{@reset}")
    {:reply, result, state}
  end

  @impl true
  def handle_call(:confirm_sighting, _from, state) do
    Logger.info("#{@color}[#{state.name}] 🚁 Received confirmation request, checking elephant...#{@reset}")
    elephant_state = ElephantPatrol.Elephant.get_state(state.elephant)
    is_destroying = elephant_state == :destroying_crops
    Logger.info("#{@color}[#{state.name}] 🚁 Confirmation result | elephant_state=#{inspect(elephant_state)} is_destroying=#{is_destroying}#{@reset}")
    {:reply, is_destroying, state}
  end

  # Private Functions

  defp do_observe(state) do
    case ElephantPatrol.Elephant.get_state(state.elephant) do
      :calm ->
        Logger.debug("#{@color}[#{state.name}] 🚁 Elephant is calm, no action needed#{@reset}")
        {:ok, :calm}

      :destroying_crops ->
        Logger.warning("#{@color}[#{state.name}] 🚁 Elephant destroying crops! Requesting permission to scare...#{@reset}")
        Logger.info("#{@color}[#{state.name}] 🚁 Calling controller #{inspect(state.controller)} for scare approval#{@reset}")

        case ElephantPatrol.Controller.request_scare(state.controller) do
          :approved ->
            Logger.info("#{@color}[#{state.name}] 🚁 Permission APPROVED! Scaring elephant...#{@reset}")
            ElephantPatrol.Elephant.scare(state.elephant)
            Logger.info("#{@color}[#{state.name}] 🚁 Elephant scared off successfully#{@reset}")
            {:ok, :scared_off}

          :rejected ->
            Logger.warning("#{@color}[#{state.name}] 🚁 Permission REJECTED. Standing down.#{@reset}")
            {:ok, :not_approved}
        end
    end
  end

  defp format_name(name) when is_atom(name), do: "Drone:#{name}"
  defp format_name(pid) when is_pid(pid), do: "Drone:#{inspect(pid)}"
  defp format_name({:via, _, name}), do: "Drone:#{inspect(name)}"
  defp format_name({:global, name}), do: "Drone:#{inspect(name)}"
  defp format_name(other), do: "Drone:#{inspect(other)}"
end
