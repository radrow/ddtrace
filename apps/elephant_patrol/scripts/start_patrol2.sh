#!/bin/bash
# Start the patrol2 node (drone2 + controller2)

cd "$(dirname "$0")/../../.."

echo "🚁 Starting PATROL2 node..."
echo "   This node hosts Drone2 and Controller2."
echo ""

EVAL_CODE='  spawn(fn ->
    Process.sleep(2000)
    ElephantPatrol.Simulation.connect_nodes()
    ElephantPatrol.Simulation.start_patrol2()
    IO.puts("\n🚁 Patrol2 node ready!\n")
  end)
'

iex --name patrol2@127.0.0.1 --cookie elephant_patrol --eval "$EVAL_CODE" -S mix
