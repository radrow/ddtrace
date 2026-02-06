defmodule ElephantPatrol do
  @moduledoc """
  A distributed Elixir application for monitoring elephants with drones.

  Components:
  - `Elephant`: Can stay calm or destroy crops
  - `Drone`: Observes an elephant and reports to its controller
  - `Controller`: Oversees a drone and confirms sightings with other drones

  ## Quick Start

  Start 3 terminals and run the scripts:

      ./apps/elephant_patrol/scripts/start_field.sh
      ./apps/elephant_patrol/scripts/start_patrol1.sh
      ./apps/elephant_patrol/scripts/start_patrol2.sh

  Then in the field node:

      ElephantPatrol.trigger_elephant()
  """

  @doc """
  Triggers the elephant to destroy crops and watches the patrol system respond.

  Options:
  - `:monitored` - if true, uses ddtrace to detect deadlocks (default: false)
  """
  defdelegate trigger_elephant(opts \\ []), to: ElephantPatrol.Simulation
end
