#!/bin/bash
# Start a patrol node with the specified name (alpha or bravo)

if [ -z "$1" ]; then
    echo "Usage: $0 <patrol_name>"
    echo "  patrol_name: alpha or bravo"
    exit 1
fi

PATROL_NAME=$1

# Validate patrol name
if [ "$PATROL_NAME" != "alpha" ] && [ "$PATROL_NAME" != "bravo" ]; then
    echo "Error: patrol_name must be 'alpha' or 'bravo'"
    exit 1
fi

cd "$(dirname "$0")/../../.."

echo "🚁 Starting $(echo $PATROL_NAME | tr '[:lower:]' '[:upper:]') patrol node..."
if [ "$PATROL_NAME" == "alpha" ]; then
    echo "   This node hosts DroneAlpha and ControllerAlpha."
else
    echo "   This node hosts DroneBravo and ControllerBravo."
fi
echo ""

EVAL_CODE="  spawn(fn ->
    Process.sleep(2000)
    ElephantPatrol.Simulation.connect_nodes()
    ElephantPatrol.Simulation.start_${PATROL_NAME}()
  end)
"

iex --name ${PATROL_NAME}@127.0.0.1 --cookie elephant_patrol --eval "$EVAL_CODE" -S mix
