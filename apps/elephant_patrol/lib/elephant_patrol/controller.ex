defmodule ElephantPatrol.Controller do
  @moduledoc """
  A GenServer representing a controller that oversees a drone.

  When its drone requests to scare an elephant, the controller asks a different
  drone to confirm the sighting before approving the request.
  """
  use GenServer

  defstruct [:drone, :confirming_drone]

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
    drone = Keyword.fetch!(opts, :drone)
    confirming_drone = Keyword.fetch!(opts, :confirming_drone)
    {:ok, %__MODULE__{drone: drone, confirming_drone: confirming_drone}}
  end

  @impl true
  def handle_call(:request_scare, _from, state) do
    result =
      case ElephantPatrol.Drone.confirm_sighting(state.confirming_drone) do
        true -> :approved
        false -> :rejected
      end

    {:reply, result, state}
  end
end
