# DDTrace

DDTrace is a tool for asynchronous distributed deadlock detection in
`gen_server`-based systems.

## Prerequisites 

The monitored system must entirely consist of `gen_server` instances. Moreover,
each server must adhere to _Single-threaded Remote Procedure Call_ (SRPC), which
in practice means that it may only use `gen_server:call` and `gen_server:cast`
for communication. To calls, they must always reply via `{reply, _Reply,
_State}` (i.e. no accumulation of the `From` argument and returning `{noreply,
_State}`). Multi-calls* through `gen_server:multi_call` and manual request
handling via `gen_server:send_request`/`gen_server:reply` is also forbidden.

In order for deadlock detection to work properly, every generic server must be
monitored.

TODO: there is a chance that `gen_server:multi_call` would work, but this is to be investigated.

## Application 

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

`
