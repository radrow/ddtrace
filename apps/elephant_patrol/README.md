# Elephant Patrol

A distributed Elixir application for monitoring elephants with drones.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                        DISTRIBUTED SYSTEM                           │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌─────────────────┐                                                │
│  │  field@localhost │                                               │
│  │  ───────────────│                                                │
│  │     🐘 Elephant  │◄─────────────┬───────────────┐                │
│  │   (global name)  │              │               │                │
│  └─────────────────┘              │               │                │
│           ▲                       │               │                │
│           │ scare                 │ observe       │ observe        │
│           │                       │               │                │
│  ┌────────┴────────┐     ┌───────┴───────┐  ┌───┴───────────┐     │
│  │ patrol1@localhost│     │patrol2@localhost│                │     │
│  │  ────────────────│     │ ────────────────│                │     │
│  │                  │     │                  │                │     │
│  │  🚁 Drone1 ──────┼─────┼──► 🎮 Controller2│                │     │
│  │       │          │     │        │         │                │     │
│  │       ▼          │     │        ▼         │                │     │
│  │  🎮 Controller1 ◄┼─────┼── 🚁 Drone2      │                │     │
│  │                  │     │                  │                │     │
│  └──────────────────┘     └──────────────────┘                │     │
│                                                                     │
│  Legend:                                                            │
│  ────────► request_scare (to own controller)                       │
│  ◄──────── confirm_sighting (from other controller)                │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Components

- **Elephant**: Can stay calm or destroy crops. Lives on the field node.
- **Drone**: Observes the elephant. If it's destroying crops, asks its controller for permission to scare it.
- **Controller**: When its drone wants to scare the elephant, asks a *different* drone to confirm the sighting first.

## Running the Distributed System

### Option 1: Manual Setup (Recommended - Proper Distributed Logging)

Start each node in a separate terminal:

```bash
# Terminal 1 - Field node
./apps/elephant_patrol/scripts/start_field.sh

# Terminal 2 - Patrol1 node
./apps/elephant_patrol/scripts/start_patrol1.sh

# Terminal 3 - Patrol2 node
./apps/elephant_patrol/scripts/start_patrol2.sh
```

For **monitoring with ddtrace** (deadlock detection), add the `--monitored` flag:

```bash
./apps/elephant_patrol/scripts/start_field.sh --monitored
./apps/elephant_patrol/scripts/start_patrol1.sh --monitored
./apps/elephant_patrol/scripts/start_patrol2.sh --monitored
```

Then run these commands in each node's iex session:

```elixir
# On field@localhost (Terminal 1):
ElephantPatrol.Simulation.connect_nodes()
ElephantPatrol.Simulation.start_elephant()

# On patrol1@localhost (Terminal 2):
ElephantPatrol.Simulation.connect_nodes()
ElephantPatrol.Simulation.start_patrol1()

# On patrol2@localhost (Terminal 3):
ElephantPatrol.Simulation.connect_nodes()
ElephantPatrol.Simulation.start_patrol2()

# On any node (after all processes started):
ElephantPatrol.trigger_elephant()              # Without monitoring
# OR
ElephantPatrol.trigger_elephant(monitored: true)  # With deadlock detection
```

With this approach, each node shows its own logs locally.

### Option 2: Automatic Setup (Quick Start)

Start all three nodes (same as above), then from the field node run:

```elixir
ElephantPatrol.Simulation.full_setup_and_run()
```

Note: With this approach, logs from remote nodes will appear on the field node.

## Deadlock Detection

When run with `monitored: true`, the system uses the `ddtrace` monitoring framework to detect the deadlock:

- Without monitoring: The system will timeout after 20 seconds
- With monitoring: The system will detect the deadlock and report it immediately with cycle information

## Manual Testing

You can also test components individually:

```elixir
# Connect nodes manually
ElephantPatrol.Simulation.connect_nodes()

# Start processes on each node
ElephantPatrol.Simulation.start_elephant()  # on field
ElephantPatrol.Simulation.start_patrol1()   # on patrol1
ElephantPatrol.Simulation.start_patrol2()   # on patrol2

# Interact with the elephant
ElephantPatrol.Elephant.destroy_crops({:global, :elephant})
ElephantPatrol.Drone.observe({:global, :drone1})
```

## Message Flow

When drone1 observes an elephant destroying crops:

1. `Drone1` checks `Elephant` state → destroying crops
2. `Drone1` calls `Controller1.request_scare()`
3. `Controller1` calls `Drone2.confirm_sighting()` (cross-node!)
4. `Drone2` checks `Elephant` state → confirms destroying crops
5. `Controller1` approves the scare request
6. `Drone1` scares the `Elephant`
7. `Elephant` becomes calm
