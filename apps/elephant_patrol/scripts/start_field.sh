#!/bin/bash
# Start the field node (controls the elephant)

cd "$(dirname "$0")/../../.."

echo "╔════════════════════════════════════════════════════════════════════════╗"
echo "║   Starting FIELD node...                                               ║"
echo "║   After all nodes are ready, run:                                      ║"
echo "║                                                                        ║"
echo "║   ElephantPatrol.trigger_elephant()                  # Without ddtrace ║"
echo "║   ElephantPatrol.trigger_elephant(monitored: true)   # With ddtrace    ║"
echo "╚════════════════════════════════════════════════════════════════════════╝"
echo ""

EVAL_CODE='
  spawn(fn ->
    Process.sleep(2000)
    ElephantPatrol.Simulation.connect_nodes()
    ElephantPatrol.Simulation.start_elephant()
  end)
'

iex --name field@127.0.0.1 --cookie elephant_patrol --eval "$EVAL_CODE" -S mix
