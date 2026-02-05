#!/bin/bash
# Start the field node (controls the elephant)

cd "$(dirname "$0")/../../.."

echo "🌾 Starting FIELD node..."
echo "   This node hosts the Elephant."
echo ""
echo "   After all nodes are ready, run:"
echo "   ElephantPatrol.trigger_elephant()"
echo ""

iex --sname field@localhost --eval '
  spawn(fn ->
    Process.sleep(2000)
    ElephantPatrol.Simulation.connect_nodes()
    ElephantPatrol.Simulation.start_elephant()
    IO.puts("\n🌾 Field node ready! Run ElephantPatrol.trigger_elephant() when all nodes are up.\n")
  end)
' -S mix
