#!/bin/bash
# Start the patrol1 node (drone1 + controller1)

cd "$(dirname "$0")/../../.."

echo "🚁 Starting PATROL1 node..."
echo "   This node hosts Drone1 and Controller1."
echo ""

iex --sname patrol1@localhost -S mix
