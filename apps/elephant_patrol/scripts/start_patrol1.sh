#!/bin/bash
# Start the patrol1 node (drone1 + controller1)

cd "$(dirname "$0")/../../.."

echo "🚁 Starting PATROL1 node..."
echo "   This node hosts Drone1 and Controller1."
echo ""

EVAL_CODE='  case :pg.start_link(:mon_reg_scope) do
    {:ok, _pid} -> IO.puts("✓ Started pg scope: mon_reg_scope")
    {:error, {:already_started, _pid}} -> IO.puts("✓ pg scope already running: mon_reg_scope")
    error -> IO.puts("✗ Failed to start pg scope: #{inspect(error)}")
  end
  spawn(fn ->
    Process.sleep(2000)
    ElephantPatrol.Simulation.connect_nodes()
    ElephantPatrol.Simulation.start_patrol1()
    IO.puts("\n🚁 Patrol1 node ready!\n")
  end)
'

iex --name patrol1@127.0.0.1 --cookie elephant_patrol --eval "$EVAL_CODE" -S mix
