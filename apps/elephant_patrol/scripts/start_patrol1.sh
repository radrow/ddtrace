#!/bin/bash
# Start the patrol1 node (drone1 + controller1)

cd "$(dirname "$0")/../../.."

echo "🚁 Starting PATROL1 node..."
echo "   This node hosts Drone1 and Controller1."
echo ""

EVAL_CODE='  spawn(fn ->
    Process.sleep(2000)
    ElephantPatrol.Simulation.connect_nodes()
    ElephantPatrol.Simulation.start_patrol1()
  end)
'

iex --name patrol1@127.0.0.1 --cookie elephant_patrol --eval "$EVAL_CODE" -S mix
