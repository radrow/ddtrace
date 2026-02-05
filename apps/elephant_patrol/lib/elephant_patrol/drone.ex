defmodule ElephantPatrol.Drone do
  @moduledoc """
  A GenServer representing a drone that observes an elephant.

  When the drone detects the elephant destroying crops, it asks its controller
  for permission to scare it off.
  """
  use GenServer

  defstruct [:elephant, :controller]

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
    GenServer.call(drone, :observe)
  end

  @doc """
  Asks the drone to confirm whether it sees the elephant destroying crops.
  Used by controllers to get a second opinion.
  """
  def confirm_sighting(drone) do
    GenServer.call(drone, :confirm_sighting)
  end

  # Server Callbacks

  @impl true
  def init(opts) do
    elephant = Keyword.fetch!(opts, :elephant)
    controller = Keyword.fetch!(opts, :controller)
    {:ok, %__MODULE__{elephant: elephant, controller: controller}}
  end

  @impl true
  def handle_call(:observe, _from, state) do
    result = do_observe(state)
    {:reply, result, state}
  end

  @impl true
  def handle_call(:confirm_sighting, _from, state) do
    elephant_state = ElephantPatrol.Elephant.get_state(state.elephant)
    is_destroying = elephant_state == :destroying_crops
    {:reply, is_destroying, state}
  end

  # Private Functions

  defp do_observe(state) do
    case ElephantPatrol.Elephant.get_state(state.elephant) do
      :calm ->
        {:ok, :calm}

      :destroying_crops ->
        case ElephantPatrol.Controller.request_scare(state.controller) do
          :approved ->
            ElephantPatrol.Elephant.scare(state.elephant)
            {:ok, :scared_off}

          :rejected ->
            {:ok, :not_approved}
        end
    end
  end
end
