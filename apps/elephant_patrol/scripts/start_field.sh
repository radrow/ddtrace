#!/bin/bash
# Start the field node (controls the elephant)

cd "$(dirname "$0")/../../.."

echo "🌾 Starting FIELD node..."
echo "   This node hosts the Elephant."
echo ""
echo "   After all nodes are ready, run:"
echo "   ElephantPatrol.trigger_elephant()              # Without monitoring"
echo "   ElephantPatrol.trigger_elephant(monitored: true)   # With ddtrace"
echo ""

EVAL_CODE='
  spawn(fn ->
    Process.sleep(2000)
    ElephantPatrol.Simulation.connect_nodes()
    ElephantPatrol.Simulation.start_elephant()
    IO.puts("\n🌾 Field node ready!\n")
  end)
'

iex --name field@127.0.0.1 --cookie elephant_patrol --eval "$EVAL_CODE" -S mix
