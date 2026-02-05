defmodule ElephantPatrol.Simulation do
  @moduledoc """
  Distributed simulation for the Elephant Patrol system.

  Topology:
  - field@localhost: Elephant (the shared elephant)
  - patrol1@localhost: Drone1 + Controller1
  - patrol2@localhost: Drone2 + Controller2

  Controller1 uses Drone2 for confirmation (cross-node)
  Controller2 uses Drone1 for confirmation (cross-node)

  ## Usage (Manual Setup - Recommended)

  Start each node in a separate terminal, then run these commands:

      # On field@localhost:
      ElephantPatrol.Simulation.connect_nodes()
      ElephantPatrol.Simulation.start_elephant()

      # On patrol1@localhost:
      ElephantPatrol.Simulation.connect_nodes()
      ElephantPatrol.Simulation.start_patrol1()

      # On patrol2@localhost:
      ElephantPatrol.Simulation.connect_nodes()
      ElephantPatrol.Simulation.start_patrol2()

      # On any node (after all processes started):
      ElephantPatrol.Simulation.run_simulation()
  """
  require Logger

  @field_node :"field@localhost"
  @patrol1_node :"patrol1@localhost"
  @patrol2_node :"patrol2@localhost"

  @elephant {:global, :elephant}
  @drone1 {:global, :drone1}
  @drone2 {:global, :drone2}
  @controller1 {:global, :controller1}
  @controller2 {:global, :controller2}

  @doc """
  Connects to all nodes in the cluster.
  Run this on each node before starting processes.
  """
  def connect_nodes do
    Logger.info("🌐 Connecting to cluster nodes...")

    nodes = [@field_node, @patrol1_node, @patrol2_node]
    current = Node.self()

    for node <- nodes, node != current do
      case Node.connect(node) do
        true ->
          Logger.info("✅ Connected to #{node}")
        false ->
          Logger.warning("❌ Failed to connect to #{node}")
        :ignored ->
          Logger.info("⏭️  Ignoring #{node} (not alive)")
      end
    end

    # Synchronize global name registry
    :global.sync()

    Logger.info("🌐 Connected nodes: #{inspect(Node.list())}")
    :ok
  end

  @doc """
  Starts the elephant on the field node.
  Run this on the field@localhost node.
  """
  def start_elephant do
    Logger.info("🐘 Starting elephant on field node...")

    {:ok, _pid} = ElephantPatrol.Elephant.start_link(name: @elephant)

    # Sync global names after registration
    :global.sync()

    Logger.info("🐘 Elephant registered globally as :elephant")
    :ok
  end

  @doc """
  Starts drone1 and controller1 on patrol1 node.
  Controller1 uses drone2 (on patrol2) for confirmation.
  Run this on the patrol1@localhost node.
  """
  def start_patrol1 do
    Logger.info("🚁 Starting patrol1 (drone1 + controller1)...")

    # Start drone1 - observes the global elephant, reports to controller1
    {:ok, _} = ElephantPatrol.Drone.start_link(
      name: @drone1,
      elephant: @elephant,
      controller: @controller1
    )

    # Start controller1 - uses drone2 (on patrol2) for confirmation
    {:ok, _} = ElephantPatrol.Controller.start_link(
      name: @controller1,
      drone: @drone1,
      confirming_drone: @drone2
    )

    # Sync global names after registration
    :global.sync()

    Logger.info("🚁 Patrol1 ready: drone1 + controller1")
    :ok
  end

  @doc """
  Starts drone2 and controller2 on patrol2 node.
  Controller2 uses drone1 (on patrol1) for confirmation.
  Run this on the patrol2@localhost node.
  """
  def start_patrol2 do
    Logger.info("🚁 Starting patrol2 (drone2 + controller2)...")

    # Start drone2 - observes the global elephant, reports to controller2
    {:ok, _} = ElephantPatrol.Drone.start_link(
      name: @drone2,
      elephant: @elephant,
      controller: @controller2
    )

    # Start controller2 - uses drone1 (on patrol1) for confirmation
    {:ok, _} = ElephantPatrol.Controller.start_link(
      name: @controller2,
      drone: @drone2,
      confirming_drone: @drone1
    )

    # Sync global names after registration
    :global.sync()

    Logger.info("🚁 Patrol2 ready: drone2 + controller2")
    :ok
  end

  @doc """
  Shows all registered global names. Useful for debugging.
  """
  def list_global_names do
    names = :global.registered_names()
    Logger.info("📋 Registered global names: #{inspect(names)}")
    names
  end

  @doc """
  Waits for all required processes to be registered globally.
  """
  def wait_for_processes(timeout \\ 10_000) do
    required = [:elephant, :drone1, :drone2, :controller1, :controller2]
    deadline = System.monotonic_time(:millisecond) + timeout

    Logger.info("⏳ Waiting for all processes to register...")

    wait_loop(required, deadline)
  end

  defp wait_loop([], _deadline) do
    Logger.info("✅ All processes registered!")
    :ok
  end

  defp wait_loop(remaining, deadline) do
    if System.monotonic_time(:millisecond) > deadline do
      Logger.error("❌ Timeout waiting for processes: #{inspect(remaining)}")
      {:error, :timeout, remaining}
    else
      still_missing =
        Enum.filter(remaining, fn name ->
          :global.whereis_name(name) == :undefined
        end)

      if still_missing == [] do
        Logger.info("✅ All processes registered!")
        :ok
      else
        Process.sleep(100)
        wait_loop(still_missing, deadline)
      end
    end
  end

  @doc """
  Runs a simple simulation scenario.
  Can be run from any connected node after all processes are started.
  """
  def run_simulation do
    Logger.info("""

    ╔════════════════════════════════════════════════════════════╗
    ║           ELEPHANT PATROL SIMULATION STARTING              ║
    ╚════════════════════════════════════════════════════════════╝
    """)

    # Ensure all processes are available
    case wait_for_processes(5_000) do
      :ok -> do_run_simulation()
      {:error, :timeout, missing} ->
        Logger.error("Cannot run simulation. Missing processes: #{inspect(missing)}")
        {:error, :missing_processes}
    end
  end

  defp do_run_simulation do
    # Give some time for logs to flush
    Process.sleep(500)

    # Step 1: Check initial state
    Logger.info("📍 Step 1: Checking initial elephant state...")
    state = ElephantPatrol.Elephant.get_state(@elephant)
    Logger.info("📍 Elephant is currently: #{inspect(state)}")
    Process.sleep(500)

    # Step 2: Drone1 observes - should see calm elephant
    Logger.info("\n📍 Step 2: Drone1 observes the calm elephant...")
    result1 = ElephantPatrol.Drone.observe(@drone1)
    Logger.info("📍 Drone1 observation result: #{inspect(result1)}")
    Process.sleep(500)

    # Step 3: Elephant starts destroying crops!
    Logger.info("\n📍 Step 3: OH NO! The elephant starts destroying crops!")
    ElephantPatrol.Elephant.destroy_crops(@elephant)
    Process.sleep(500)

    # Step 4: Drone1 observes - should detect and request scare
    Logger.info("\n📍 Step 4: Drone1 observes again...")
    result2 = ElephantPatrol.Drone.observe(@drone1)
    Logger.info("📍 Drone1 observation result: #{inspect(result2)}")
    Process.sleep(500)

    # Step 5: Verify elephant is calm again
    Logger.info("\n📍 Step 5: Verifying elephant state after intervention...")
    final_state = ElephantPatrol.Elephant.get_state(@elephant)
    Logger.info("📍 Elephant is now: #{inspect(final_state)}")

    Logger.info("""

    ╔════════════════════════════════════════════════════════════╗
    ║           SIMULATION COMPLETE                              ║
    ╚════════════════════════════════════════════════════════════╝
    """)

    :ok
  end

  @doc """
  Convenience function to set up and run everything from the field node.
  Assumes all nodes are already started.

  NOTE: Logs from remote nodes will appear on this node due to RPC behavior.
  For proper distributed logging, use manual setup (see module docs).
  """
  def full_setup_and_run do
    # Connect all nodes
    connect_nodes()
    Process.sleep(500)

    # Start elephant locally (on field)
    start_elephant()
    Process.sleep(500)

    # Start patrol1 on its node using spawn to keep process alive
    Logger.info("🌐 Starting patrol1 on #{@patrol1_node}...")
    Node.spawn(@patrol1_node, __MODULE__, :start_patrol1, [])
    Process.sleep(1000)

    # Start patrol2 on its node using spawn to keep process alive
    Logger.info("🌐 Starting patrol2 on #{@patrol2_node}...")
    Node.spawn(@patrol2_node, __MODULE__, :start_patrol2, [])
    Process.sleep(1000)

    # Sync global registry
    :global.sync()
    Process.sleep(500)

    # Run simulation
    run_simulation()
  end
end
