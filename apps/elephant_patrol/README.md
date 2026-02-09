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
│  │ alpha@localhost  │     │bravo@localhost  │                │     │
│  │  ────────────────│     │ ────────────────│                │     │
│  │                  │     │                  │                │     │
│  │  🚁 DroneAlpha ───┼─────┼──► 🎮 ControllerBravo                │     │
│  │       │          │     │        │         │                │     │
│  │       ▼          │     │        ▼         │                │     │
│  │  🎮 ControllerAlpha◄┼─────┼── 🚁 DroneBravo   │                │     │
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

Start each node in a separate terminal:

```bash
# Terminal 1 - Field node
./apps/elephant_patrol/scripts/start_field.sh

# Terminal 2 - Alpha patrol node
./apps/elephant_patrol/scripts/start_patrol.sh alpha

# Terminal 3 - Bravo patrol node
./apps/elephant_patrol/scripts/start_patrol.sh bravo
```

Then run the following command in the `field` iex session:

```elixir
ElephantPatrol.trigger_elephant()                 # Without monitoring
# OR
ElephantPatrol.trigger_elephant(monitored: true)  # With deadlock detection
```

## Deadlock Detection

When run with `monitored: true`, the system uses the `ddtrace` monitoring framework to detect the deadlock:

- Without monitoring: The system will timeout after 20 seconds
- With monitoring: The system will detect the deadlock and report it immediately with cycle information

## Message Flow

When drone_alpha observes an elephant destroying crops:

1. `DroneAlpha` checks `Elephant` state → destroying crops
2. `DroneAlpha` calls `ControllerAlpha.request_scare()`
3. `ControllerAlpha` calls `DroneBravo.confirm_sighting()` (cross-node!)
4. `DroneBravo` checks `Elephant` state → confirms destroying crops
5. `ControllerAlpha` approves the scare request
6. `DroneAlpha` scares the `Elephant`
7. `Elephant` becomes calm
