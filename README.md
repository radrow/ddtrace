# DDTrace

DDTrace is a tool for asynchronous distributed deadlock detection in
`gen_server`-based systems.

## Repository layout

The three top-level applications are:

- `apps/ddtrace` – the main DDTrace library.
- `apps/model` – the scenario generator, tracer tooling, and supporting modules
  used for exercising the library.
- `apps/ddtrace_cli` – the Elixir escript wrapper that drives scenarios via the
  model application.

To build the tooling run:

```
mix deps.get
mix escript.build
```

The escript is written to `./ddtrace`. Example usage:

```
./ddtrace apps/model/priv/scenarios/deadlock.conf
```

## Prerequisites

- Erlang/OTP 26
- Elixir 1.14

## Application requirements

The monitored system must entirely consist of `gen_server` instances. Moreover,
each server must adhere to _Single-threaded Remote Procedure Call_ (SRPC), which
in practice means that it may only use `gen_server:call` and `gen_server:cast`
for communication. To calls, they must always reply via `{reply, _Reply,
_State}` (i.e. no accumulation of the `From` argument and returning `{noreply,
_State}`). Multi-calls* through `gen_server:multi_call` and manual request
handling via `gen_server:send_request`/`gen_server:reply` is also forbidden. In
order for deadlock detection to work properly, every generic server must be
monitored.

TODO: there is a chance that `gen_server:multi_call` would work, but this is to be investigated.

## Instrumenting generic servers with DDTrace

A monitor is started via `ddtrace:start` or `ddtrace:start_link`. The PID of the
monitored `gen_server` is passed as a parameter.

Monitors recognise each other via a *monitor registry* which maps generic
servers' PIDs to their monitors. The registry is implemented in the `mon_reg`
module and its reference needs to be passed as argument to each monitor on
startup. Monitors take care of registering themselves in the registry. It is
important that all monitors in the system use the same registry.

In order to receive a deadlock notification, the user needs to register itself
as a subscriber to a particular monitor. One would normally subscribe to a
monitor immediately after making a call, and unsubscribe upon receiving a
response or deadlock notification. To subscribe to deadlocks, use the
`ddmon:subscribe` function (use `ddmon:unsubscribe` to opt out). The
subscribtion function returns a request identifier that can be used in generic
server's `reqid` or listened to directly via `gen_server:wait_response`.

The following snippet shows exemplifies how to monitor a single generic
server with DDTrace:

``` erlang
%% Start the service
{ok, P} = gen_server:start(my_gen_server_module, []),

%% Start the mon register
{ok, MonReg} = mon_reg:start_link(),
%% Start the monitor
{ok, M} = ddtrace:start_link(P, MonReg),

%% Subscribe to deadlocks
ReqM = ddtrace:subscribe_deadlocks(Mon),

%% Call the service
ReqP = gen_server:send_request(P, request)

%% Set up request ID collection
ReqIds0 = gen_server:reqids_new(),
ReqIds1 = gen_server:reqids_add(ReqP, process, ReqIds0),
ReqIds2 = gen_server:reqids_add(MonP, monitor, ReqIds1),

case gen_statem:receive_response(ReqIds2, infinity, true) of
  {{reply, R}, process, _ReqIds} -> %% Handle reply
  {{reply, {deadlock, Cycle}}, monitor, _ReqIds} -> %% Handle deadlock
end.
```
**IMPORTANT:** Self-inflicted deadlocks (e.g. `gen_server:call(self(), lol)`)
are handled by `gen_server` and cause the process to crash without sending a
call message. DDTrace will handle this case as well, but the end user might a
receive crash result before the deadlock notification from DDTrace. Note that
simply waiting for `{error, {calling_self, _}, _Label, _ReqIds}` is not
sufficient, as this may happen in a nested call. Therefore, some additional
recursion might be needed to distinguish such a deadlock from a regular error.
