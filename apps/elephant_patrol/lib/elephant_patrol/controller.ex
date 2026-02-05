defmodule ElephantPatrol.Controller do
  @moduledoc """
  A GenServer representing a controller that oversees a drone.

  When its drone requests to scare an elephant, the controller asks a different
  drone to confirm the sighting before approving the request.
  """
  use GenServer
  require Logger

  defstruct [:name, :drone, :confirming_drone]

  # Client API

  @doc """
  Starts a Controller process.

  Options:
  - `:name` - Optional name for the process
  - `:drone` - The drone process this controller oversees (pid or name)
  - `:confirming_drone` - A different drone used to confirm elephant sightings (pid or name)
  """
  def start_link(opts) do
    name = Keyword.get(opts, :name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Called by a drone to request permission to scare an elephant.
  The controller will ask the confirming drone to verify the sighting.

  Returns:
  - `:approved` if the confirming drone confirms the elephant is destroying crops
  - `:rejected` if the confirming drone does not confirm the sighting
  """
  def request_scare(controller) do
    GenServer.call(controller, :request_scare)
  end

  # Server Callbacks

  @impl true
  def init(opts) do
    name = Keyword.get(opts, :name, self())
    drone = Keyword.fetch!(opts, :drone)
    confirming_drone = Keyword.fetch!(opts, :confirming_drone)
    state = %__MODULE__{name: format_name(name), drone: drone, confirming_drone: confirming_drone}
    Logger.info("[#{state.name}] 🎮 Controller initialized | drone=#{inspect(drone)} confirming_drone=#{inspect(confirming_drone)}")
    {:ok, state}
  end

  @impl true
  def handle_call(:request_scare, _from, state) do
    Logger.info("[#{state.name}] 🎮 Received scare request from drone")
    Logger.info("[#{state.name}] 🎮 Requesting confirmation from #{inspect(state.confirming_drone)}...")

    result =
      case ElephantPatrol.Drone.confirm_sighting(state.confirming_drone) do
        true ->
          Logger.info("[#{state.name}] 🎮 Sighting CONFIRMED by confirming drone -> APPROVING scare")
          :approved

        false ->
          Logger.warning("[#{state.name}] 🎮 Sighting NOT CONFIRMED by confirming drone -> REJECTING scare")
          :rejected
      end

    Logger.info("[#{state.name}] 🎮 Scare request decision: #{inspect(result)}")
    {:reply, result, state}
  end

  # Private Functions

  defp format_name(name) when is_atom(name), do: "Controller:#{name}"
  defp format_name(pid) when is_pid(pid), do: "Controller:#{inspect(pid)}"
  defp format_name({:via, _, name}), do: "Controller:#{inspect(name)}"
  defp format_name({:global, name}), do: "Controller:#{inspect(name)}"
  defp format_name(other), do: "Controller:#{inspect(other)}"
end
