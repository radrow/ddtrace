#!/bin/bash
# Start the patrol2 node (drone2 + controller2)

cd "$(dirname "$0")/../../.."

echo "🚁 Starting PATROL2 node..."
echo "   This node hosts Drone2 and Controller2."
echo ""

EVAL_CODE='  case :pg.start_link(:mon_reg_scope) do
    {:ok, _pid} -> IO.puts("✓ Started pg scope: mon_reg_scope")
    {:error, {:already_started, _pid}} -> IO.puts("✓ pg scope already running: mon_reg_scope")
    error -> IO.puts("✗ Failed to start pg scope: #{inspect(error)}")
  end
  spawn(fn ->
    Process.sleep(2000)
    ElephantPatrol.Simulation.connect_nodes()
    ElephantPatrol.Simulation.start_patrol2()
    IO.puts("\n🚁 Patrol2 node ready!\n")
  end)
'

iex --name patrol2@127.0.0.1 --cookie elephant_patrol --eval "$EVAL_CODE" -S mix
