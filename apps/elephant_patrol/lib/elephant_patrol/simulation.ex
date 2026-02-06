defmodule ElephantPatrol.Simulation do
  @moduledoc """
  Distributed simulation for the Elephant Patrol system.

  Topology:
  - field@localhost: Elephant (the shared elephant)
  - patrol1@localhost: Drone1 + Controller1
  - patrol2@localhost: Drone2 + Controller2

  Controller1 uses Drone2 for confirmation (cross-node)
  Controller2 uses Drone1 for confirmation (cross-node)

  ## Usage

  Start each node in a separate terminal:

      # Terminal 1 - Field node
      ./apps/elephant_patrol/scripts/start_field.sh

      # Terminal 2 - Patrol1 node
      ./apps/elephant_patrol/scripts/start_patrol1.sh

      # Terminal 3 - Patrol2 node
      ./apps/elephant_patrol/scripts/start_patrol2.sh

      # On field node, run:
      ElephantPatrol.trigger_elephant()           # Without monitoring
      ElephantPatrol.trigger_elephant(monitored: true)  # With ddtrace
  """
  require Logger

  @field_node :"field@127.0.0.1"
  @patrol1_node :"patrol1@127.0.0.1"
  @patrol2_node :"patrol2@127.0.0.1"

  @elephant {:global, :elephant}
  @drone1 {:global, :drone1}
  @drone2 {:global, :drone2}
  @controller1 {:global, :controller1}
  @controller2 {:global, :controller2}

  # ============================================================================
  # Node Setup Functions
  # ============================================================================

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
        true -> Logger.info("✅ Connected to #{node}")
        false -> Logger.warning("❌ Failed to connect to #{node}")
        :ignored -> Logger.info("⏭️  Ignoring #{node} (not alive)")
      end
    end

    :global.sync()
    Logger.info("🌐 Connected nodes: #{inspect(Node.list())}")
    :ok
  end

  @doc """
  Starts the elephant on the field node.
  """
  def start_elephant(_opts \\ []) do
    Logger.info("🐘 Starting elephant on field node...")
    {:ok, _pid} = ElephantPatrol.Elephant.start_link(name: @elephant)
    :global.sync()
    Logger.info("🐘 Elephant registered globally as :elephant")
    :ok
  end

  @doc """
  Starts drone1 and controller1 on patrol1 node.
  """
  def start_patrol1(_opts \\ []) do
    Logger.info("🚁 Starting patrol1 (drone1 + controller1)...")

    {:ok, _} = ElephantPatrol.Drone.start_link(
      name: @drone1,
      elephant: @elephant,
      controller: @controller1
    )

    {:ok, _} = ElephantPatrol.Controller.start_link(
      name: @controller1,
      drone: @drone1,
      confirming_drone: @drone2
    )

    :global.sync()
    Logger.info("🚁 Patrol1 ready: drone1 + controller1")
    :ok
  end

  @doc """
  Starts drone2 and controller2 on patrol2 node.
  """
  def start_patrol2(_opts \\ []) do
    Logger.info("🚁 Starting patrol2 (drone2 + controller2)...")

    {:ok, _} = ElephantPatrol.Drone.start_link(
      name: @drone2,
      elephant: @elephant,
      controller: @controller2
    )

    {:ok, _} = ElephantPatrol.Controller.start_link(
      name: @controller2,
      drone: @drone2,
      confirming_drone: @drone1
    )

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

  # ============================================================================
  # Simulation Trigger
  # ============================================================================

  @doc """
  Triggers the elephant to destroy crops and watches the system respond.

  Options:
  - `:monitored` - if true, uses ddtrace to detect deadlocks (default: false)
  """
  def trigger_elephant(opts \\ []) do
    monitored = Keyword.get(opts, :monitored, false)

    Logger.info("""

    ╔════════════════════════════════════════════════════════════╗
    ║           🐘 TRIGGERING THE ELEPHANT 🐘                    ║
    ║           Monitoring: #{if monitored, do: "ENABLED", else: "DISABLED"}                        ║
    ╚════════════════════════════════════════════════════════════╝
    """)

    case wait_for_processes(5_000) do
      :ok -> do_trigger_elephant(monitored)
      {:error, :timeout, missing} ->
        Logger.error("Cannot trigger elephant. Missing processes: #{inspect(missing)}")
        {:error, :missing_processes}
    end
  end

  defp do_trigger_elephant(monitored) do
    Process.sleep(1000)

    # Step 1: Check initial state
    Logger.info("📍 Step 1: Checking initial elephant state...")
    state = ElephantPatrol.Elephant.get_state(@elephant)
    Logger.info("📍 Elephant is currently: #{inspect(state)}")
    Process.sleep(500)

    # Step 2: Elephant starts destroying crops!
    Logger.info("""

    ╔════════════════════════════════════════════════════════════╗
    ║   💥 OH NO! THE ELEPHANT IS DESTROYING CROPS! 💥           ║
    ╚════════════════════════════════════════════════════════════╝
    """)
    ElephantPatrol.Elephant.destroy_crops(@elephant)
    Process.sleep(1000)

    # Step 3: Both drones observe SIMULTANEOUSLY - this creates the deadlock!
    Logger.info("""

    ╔════════════════════════════════════════════════════════════╗
    ║   🚁 DISPATCHING BOTH DRONES SIMULTANEOUSLY...             ║
    ╚════════════════════════════════════════════════════════════╝
    """)

    # Collect all global names
    global_names = collect_all_global_names()
    Logger.info("🔍 Collected global names: #{inspect(global_names)}")

    # Setup monitors (like microchip_factory does)
    ctx = setup_monitors(monitored, global_names)

    # Prepare calls to both drones
    calls = [@drone1, @drone2]

    # Execute the calls
    result = do_calls(calls, timeout: 20_000, monitor_ctx: ctx)

    print_result(result)
    cleanup_monitors(ctx)

    Logger.info("""

    ╔════════════════════════════════════════════════════════════╗
    ║           🎬 DEMONSTRATION COMPLETE                        ║
    ╚════════════════════════════════════════════════════════════╝
    """)

    result
  end

  defp collect_all_global_names do
    # Return global names instead of PIDs
    %{
      elephant: @elephant,
      drone1: @drone1,
      drone2: @drone2,
      controller1: @controller1,
      controller2: @controller2
    }
  end

  # ============================================================================
  # Monitoring Setup
  # ============================================================================

  defp setup_monitors(false, _global_names), do: nil

  defp setup_monitors(true, global_names) do
    Logger.info("🔍 Setting up distributed monitors...")

    # Ensure pg scope is running on all connected nodes
    :mon_reg.ensure_started()
    for node <- Node.list() do
      :rpc.call(node, :mon_reg, :ensure_started, [])
    end

    # Group global names by their target node
    names_by_node =
      global_names
      |> Map.values()
      |> Enum.filter(& &1)  # Remove nils
      |> Enum.group_by(fn global_name ->
        case :global.whereis_name(elem(global_name, 1)) do
          :undefined -> nil
          pid -> node(pid)
        end
      end)
      |> Map.delete(nil)  # Remove any undefined processes

    Logger.info("🔍 Global names by node: #{inspect(names_by_node)}")

    # Create monitors for each global name
    monitors =
      Enum.reduce(names_by_node, %{}, fn {target_node, node_names}, acc ->
        if target_node == node() do
          Logger.info("🔍 Creating monitors for local processes on #{target_node}...")
          create_local_monitors(node_names, acc)
        else
          Logger.info("🔍 Creating monitors for remote processes on #{target_node} via RPC...")
          create_remote_monitors(target_node, node_names, acc)
        end
      end)

    # Wait a bit for all registrations to propagate
    Process.sleep(500)

    # Verify all monitors are registered
    Logger.info("🔍 Verifying monitor registrations...")
    for {global_name, monitor} <- monitors do
      lookup = :mon_reg.mon_of(global_name)
      Logger.info("   #{inspect(global_name)} => #{inspect(lookup)} (expected: #{inspect(monitor)})")
    end

    Logger.info("🔍 All monitors created: #{inspect(monitors)}")

    %{monitors: monitors}
  end

  defp create_local_monitors(global_names, acc) do
    Enum.reduce(global_names, acc, fn global_name, inner_acc ->
      case :ddtrace.start_link(global_name, []) do
        {:ok, monitor} ->
          Logger.info("   ✓ Monitor #{inspect(monitor)} for local #{inspect(global_name)}")
          Map.put(inner_acc, global_name, monitor)
        {:error, reason} ->
          Logger.error("   ✗ Failed to monitor #{inspect(global_name)}: #{inspect(reason)}")
          inner_acc
      end
    end)
  end

  defp create_remote_monitors(target_node, global_names, acc) do
    # For each remote global name, we need to spawn a monitor on that node
    # The monitor must be local to the process it's monitoring
    # Use :start instead of :start_link to avoid linking issues with RPC
    Enum.reduce(global_names, acc, fn global_name, inner_acc ->
      case :rpc.call(target_node, :ddtrace, :start, [global_name, []]) do
        {:ok, monitor} ->
          Logger.info("   ✓ Monitor #{inspect(monitor)} for remote #{inspect(global_name)} on #{target_node}")
          Map.put(inner_acc, global_name, monitor)
        {:error, reason} ->
          Logger.error("   ✗ Failed to monitor #{inspect(global_name)}: #{inspect(reason)}")
          inner_acc
        {:badrpc, reason} ->
          Logger.error("   ✗ RPC failed for #{inspect(global_name)}: #{inspect(reason)}")
          inner_acc
      end
    end)
  end

  defp cleanup_monitors(nil), do: :ok

  defp cleanup_monitors(%{monitors: monitors}) do
    Enum.each(monitors, fn {_global_name, monitor} ->
      try do
        :ddtrace.stop_tracer(monitor)
      catch
        :exit, _ -> :ok
      end
    end)
  end

  # ============================================================================
  # Call Execution (following microchip_factory pattern)
  # ============================================================================

  defp do_calls(drones, opts) do
    timeout = Keyword.fetch!(opts, :timeout)
    ctx = Keyword.get(opts, :monitor_ctx)

    # Prepare requests to both drones simultaneously
    reqs =
      Enum.map(drones, fn drone ->
        # Small delay between calls to make logs clearer
        Process.sleep(50)
        prepare_request(drone, ctx)
      end)

    # Wait for all responses, short-circuiting on deadlock
    results = collect_results(reqs, timeout)

    # Check for deadlock first
    case Enum.find(results, &match?({:deadlock, _}, &1)) do
      {:deadlock, dl} -> {:deadlock, dl}
      nil ->
        cond do
          Enum.any?(results, &(&1 == :timeout)) -> :timeout
          true ->
            replies = for {:ok, reply} <- results, do: reply
            {:success, replies}
        end
    end
  end

  defp prepare_request(drone, nil) do
    # No monitoring - just send the request
    call_req = :gen_server.send_request(drone, :observe)
    %{drone: drone, call_req: call_req}
  end

  defp prepare_request(drone, %{monitors: monitors}) do
    # With monitoring - also subscribe to deadlocks
    # drone is already a global name like {:global, :drone1}
    monitor = Map.fetch!(monitors, drone)

    call_req = :gen_server.send_request(drone, :observe)
    deadlock_req = :ddtrace.subscribe_deadlocks(monitor)

    %{drone: drone, call_req: call_req, deadlock_req: deadlock_req, monitor: monitor}
  end

  defp await_response(req_info, timeout) do
    reqs0 = :gen_server.reqids_new()
    reqs1 = :gen_server.reqids_add(req_info.call_req, {:call, req_info}, reqs0)

    reqs =
      case Map.get(req_info, :deadlock_req) do
        nil -> reqs1
        deadlock_req -> :gen_server.reqids_add(deadlock_req, {:deadlock, req_info}, reqs1)
      end

    wait_for_response(reqs, req_info, timeout)
  end

  defp collect_results([], _timeout), do: []

  defp collect_results([req | rest], timeout) do
    case await_response(req, timeout) do
      {:deadlock, _} = dl ->
        # Short-circuit: unsubscribe remaining requests and return deadlock
        Enum.each(rest, &maybe_unsubscribe/1)
        [dl]

      result ->
        [result | collect_results(rest, timeout)]
    end
  end

  defp wait_for_response(reqs, info, timeout) do
    case :gen_server.wait_response(reqs, timeout, true) do
      :timeout ->
        maybe_unsubscribe(info)
        :timeout

      {{:reply, {:deadlock, dl}}, {:deadlock, _}, _next_reqs} ->
        maybe_unsubscribe(info)
        {:deadlock, dl}

      {{:reply, payload}, {:call, _}, _next_reqs} ->
        maybe_unsubscribe(info)
        {:ok, payload}

      {{:error, _}, {:call, _}, _next_reqs} ->
        maybe_unsubscribe(info)
        :timeout

      {_, _, next_reqs} ->
        wait_for_response(next_reqs, info, timeout)

      :no_request ->
        maybe_unsubscribe(info)
        :timeout
    end
  end

  defp maybe_unsubscribe(%{monitor: monitor}) do
    try do
      :ddtrace.unsubscribe_deadlocks(monitor)
    catch
      :exit, _ -> :ok
    end

    :ok
  end

  defp maybe_unsubscribe(_), do: :ok

  # ============================================================================
  # Result Display
  # ============================================================================

  defp print_result(result) do
    case result do
      {:deadlock, dl} ->
        Logger.error("""

        ╔════════════════════════════════════════════════════════════╗
        ║   🔴 DEADLOCK DETECTED!                                   ║
        ╚════════════════════════════════════════════════════════════╝
        """)
        Logger.error("Deadlock cycle:")
        for p <- dl do
          name = find_process_name(p)
          Logger.error("   → #{name} (#{inspect(p)})")
        end

      {:success, replies} ->
        Logger.info("""

        ╔════════════════════════════════════════════════════════════╗
        ║   ✅ SUCCESS! All drones completed observation.           ║
        ╚════════════════════════════════════════════════════════════╝
        """)
        Logger.info("Results: #{inspect(replies)}")

      :timeout ->
        Logger.error("""

        ╔════════════════════════════════════════════════════════════╗
        ║   💀 TIMEOUT! Possible deadlock (undetected).             ║
        ╚════════════════════════════════════════════════════════════╝
        """)
    end
  end

  defp find_process_name(pid) do
    cond do
      pid == GenServer.whereis(@elephant) -> ":elephant"
      pid == GenServer.whereis(@drone1) -> ":drone1"
      pid == GenServer.whereis(@drone2) -> ":drone2"
      pid == GenServer.whereis(@controller1) -> ":controller1"
      pid == GenServer.whereis(@controller2) -> ":controller2"
      true -> inspect(pid)
    end
  end

  # ============================================================================
  # Full Setup Helper
  # ============================================================================

  @doc """
  Convenience function to set up and run everything from the field node.
  """
  def full_setup_and_run do
    connect_nodes()
    Process.sleep(500)

    start_elephant()
    Process.sleep(500)

    Logger.info("🌐 Starting patrol1 on #{@patrol1_node}...")
    Node.spawn(@patrol1_node, __MODULE__, :start_patrol1, [[]])
    Process.sleep(1000)

    Logger.info("🌐 Starting patrol2 on #{@patrol2_node}...")
    Node.spawn(@patrol2_node, __MODULE__, :start_patrol2, [[]])
    Process.sleep(1000)

    :global.sync()
    Process.sleep(500)

    trigger_elephant()
  end
end
