# Implementation overview

The project implemented mainly in Erlang and is built using
[Mix](https://hexdocs.pm/elixir/introduction-to-mix.html). The overall structure
is standard:

- `mix.exs` — project configuration
- `src/` — Erlang code
- `lib/` — Elixir code

## Algorithm for distributed deadlock detection

Files of concern:

- `src/ddmon.erl` — implementation of the algorithm
- `src/ddmon.hrl` — common macros
- `src/gen_monitored.erl` — auxiliary layer between monitors and `gen_server`
  services

### Startup

To launch a generic server as a monitored service, call `ddmon:start` instead of
`gen_server:start` (or `start_link` respectively). The following things happen
when a service is started this way:

- First, a `ddmon` monitor is started. If a process name has been provided, the
  monitor will use it.
- During `ddmon` process initialisation, the original `gen_server` is started
  with the provided parameters (except name). The `gen_server` process is linked
  to the monitor.
- The PID of the monitor is returned.

### Operation of a monitored service

The wrapped generic server is started through the `gen_monitored` module. This
helps informing the service that it is monitored and is useful for integration
with unmonitored components of the system.

It is important that the service uses the `ddmon` module to perform calls to
other monitored services. This is because the `ddmon:call/2,3` function
redirects such calls into call requests towards the monitor, enabling
encapsulation required by the semantics of monitored services.

### Operation of the monitor

The monitor is implemented as a generic state machine (`gen_statem`) with three
states: `unlocked`, `locked` and `deadlocked`. It reacts to incoming calls
mimicking the `gen_server` interface. Such calls are forwarded to the monitored
service as non-blocking `gen_server` calls (via `gen_server:send_request`). The
monitor then actively waits for either a response or external call requests from
the service, while handling other incoming calls and probes. Probes are
communicated via `cast` messages. All unrecognised communication is forwarded
as-is.

#### Relating implementation to the semantics of monitored services (*Figure 9*)

The following refer to [state
callbacks](https://www.erlang.org/doc/apps/stdlib/gen_statem.html#state-callback)
(`unlocked`, `locked`, `deadlocked`) implemented in `src/ddmon.erl`.

- Rules `MON-I`, `MON-TO` and `MON-MI` are part of the Erlang runtime system as
  receiving messages (`call` and probe `cast` respectively).
- `MON-TI` specifies how the monitor reacts to incoming messages:
  - If the message is an incoming query, then it is handled as `{call, From}`
    where `From` points to a foreign service (i.e. not the one supervised by the
    monitor).
  - If the message is a response, then it appears as `info` ("miscellaneous
    message"). If the monitor is locked, it compares it to the current request
    id to distinguish it from other messages.
- `MON-O` triggers when an outgoing message is forwarded. In `ddmon`, this is
  handled in two variants:
  - If the message is a response, then it is handled as an `info` callback. The
    monitor keeps track of currently processed requests and is able to recognise
    outgoing responses using `gen_statem:check_response`.
  - Outgoing queries are pre-processed by `ddmon:call` to include information
    about the intended recipient (which is needed since the actual recipient is
    the monitor). The first parameter of the handler callback is of shape
    `{call, {Worker, PTag}}`, and the payload matches `{Msg, Server}`, where:
    - `Worker` is the PID of the monitored service
    - `PTag` is the unique reference of the call used to recognise a matching
      response. It is also used as the active probe until a response is
      received.
    - `Msg` is the original message to be forwarded
    - `Server` is the intended recipient of the call
- Rule `MON-TMI` describes how a probe is handled. In `ddmon`, probes are `cast`
  messages of form `{?PROBE, Probe, Chain}`, where
  - `?PROBE` is a constant Erlang atom defined in `src/ddmon.hrl`.
  - `Probe` is a unique `gen_server` call reference created when the initiator
    of the probe sent its request.
  - `Chain` is the list of nodes visited by the probe during the propagation.
    When a deadlock is reported, this list shows a minimal deadlocked set. This
    component is not part of the theory, but is mentioned in the paper in
    *Section 7.1*.
- In Rules `MON_TI`, `MON-O` and `MON-TMI` a sequence of messages yielded by the
  algorithm is prepended to the monitor queue. In case of this algorithm, these
  are always outgoing probes which are scheduled to be sent before any other
  message is handled. In the paper, this significantly simplified the syntax. In
  the implementation, `ddmon` simply sends these messages sequentially.

#### Relating implementation to the deadlock detecting algorithm

In the paper we declare the monitor state as a record with three fields; these
correspond to `ddmon` as follows:

| Paper               | DDMon                                          |
+---------------------+------------------------------------------------+
| `probe` (defined)   | `req_tag` in `state` record                    |
| `probe` (undefined) | State machine is in `unlocked` state           |
| `waiting`           | `waitees` in `state` record                    |
| `alarm`             | Whether state machine is in `deadlocked` state |

We use `gen_server:reply_tag()` to implement probes. These tags are uniquely
generated by `gen_server` internals for each call and thus pertain to our
requirements. Therefore, the `freshProbe` function is not explicitly defined, as
the probe is obtained directly from the outgoing query.

To implement the `waiting` list, we use the collection of request ids provided
by `gen_statem` (`gen_statem:request_id_collection()`). This lets us
conveniently manage the list of waiting services and provides us with a handle
to properly forward responses.

Rules from *Figure 12* are implemented by the state callbacks of `ddmon`. In
addition to the behaviours described in the paper, monitors propagate
information about observed deadlocks via replies of form `{?DEADLOCK, DL}` where
`?DEADLOCK` is a constant defined in `ddmon.hrl` and `DL` is a minimal
deadlocked set. This is helpful to terminate the system early in tests and
investigate deadlocks.

## Scenarios and testing

The following modules are related to benchmarking and testing described in
*Section 7*. They do not relate to the theory, but implement utilities such as
the scenario DSL, visualisation, supervision, and examples.

- `src/ansi_color.erl` — ANSI coloring and formatting.
- `src/tracer` — Erlang debugging process that inspects events in a simulated network.
- `src/scenario` — Entrypoint for benchmarks and tests. Parses scenario files,
  applies preprocessing, runs experiments and handles results.
- `src/logging.erl` — Visualisation of logs produced by the tracer.
- `src/scenario_gen.erl` — Generator of large scenarios from generic schema.
- `lib/test_server.ex` — Generic server evaluating the scenario DSL.
- `lib/main.ex` — Command line interface for running scenarios.

