#!/bin/bash
# Start the field node (controls the elephant)

cd "$(dirname "$0")/../../.."

echo "🌾 Starting FIELD node..."
echo "   This node hosts the Elephant."
echo ""

iex --sname field@localhost -S mix
