#!/bin/bash
# Start the patrol2 node (drone2 + controller2)

cd "$(dirname "$0")/../../.."

echo "🚁 Starting PATROL2 node..."
echo "   This node hosts Drone2 and Controller2."
echo ""

iex --sname patrol2@localhost -S mix
