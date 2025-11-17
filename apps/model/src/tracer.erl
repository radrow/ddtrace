-module(tracer).

-include_lib("ddtrace/include/ddtrace.hrl").

 -export([ start_link/1, start_link/2
         , finish/1, finish/2
         ]).


start_link(Procs) ->
    start_link(Procs, []).

start_link(Procs, Opts) ->
    Init = self(),
    Tracer = spawn_link(fun() -> run_tracer(Init, Procs, Opts) end),
    receive {Tracer, ready} -> ok end,
    Tracer.


run_tracer(Init, Procs, Opts) ->
    config_tracer(Opts),

    %% By default, trace only monitors (ddtrace statem), not workers or ddtrace_monitor
    TraceMon = proplists:get_value(trace_mon, Opts, true),
    put(trace_int, proplists:get_value(trace_int, Opts, true)),
    put(live_log, proplists:get_value(live_log, Opts, false)),

    %% Normalize input: Procs can be [Pid] or [{Mon, _Proc}]
    Mons0 = [ case X of {M, _} when is_pid(M) -> M; P when is_pid(P) -> P; _ -> undefined end
              || X <- Procs
            ],
    Mons = [M || M <- Mons0, is_pid(M)],

    TraceOpts = ['call', strict_monotonic_timestamp],
    [ erlang:trace(M, true, TraceOpts) || TraceMon, M <- Mons ],

    put({type, Init}, init),
    put(init_time, erlang:monotonic_time()),

    Init ! {self(), ready},

    loop([]).


config_tracer(Opts) ->
        logging:conf(Opts),

        erlang:trace_pattern(
            {ddtrace, send_notif, 3},
            [ {['_', '_', '_'], [], [trace]} ],
            [local]
         ),
        erlang:trace_pattern(
            {ddtrace, state_deadlock, 2},
            [ {['_', '_'], [], [trace]} ],
            [local]
         ),
        erlang:trace_pattern(
            {ddtrace, state_lock, 2},
            [ {['_', '_'], [], [trace]} ],
            [local]
         ),
        erlang:trace_pattern(
            {ddtrace, state_unlock, 1},
            [ {['_'], [], [trace]} ],
            [local]
         ),
        erlang:trace_pattern(
            {ddtrace, state_unwait, 2},
            [ {['_', '_'], [], [trace]} ],
            [local]
         ),
        erlang:trace_pattern(
            {ddtrace, state_wait, 3},
            [ {['_', '_', '_'], [], [trace]} ],
            [local]
         ),
        erlang:trace_pattern(
            {ddtrace, handle_event, 4},
            [ {['_', '_', '_', '_'], [], [trace]} ],
            [local]
         ),

        ok.


loop(Log) ->
    receive
        {From, finito} ->
            From ! {self(), lists:reverse(Log)},
            ok;
        Trace ->
            El = handle(Trace),
            [logging:log_trace(get(init_time), El) || get(live_log)],
            loop([El|Log])
    end.


finish(Tracer) ->
    finish(Tracer, []).

finish(Tracer, Tracees) ->
    Refs = [ erlang:trace_delivered(Tracee) || Tracee <- Tracees ],
    [ receive {trace_delivered, _, Ref} -> ok end || Ref <- Refs],

    Tracer ! {self(), finito},
    receive
        {Tracer, Log} ->
            Log
    end.

handle({trace_ts, Who, 'call', 
        {ddtrace, send_notif, [To, MsgInfo, _Data]},
        Time}) ->
    {Time, Who, {send, {notif, To, MsgInfo}}};

handle({trace_ts, Who, 'call', 
        {ddtrace, state_deadlock, [DL, _Data]},
        Time}) ->
    {Time, Who, {state, {deadlock, DL}}};

handle({trace_ts, Who, 'call', 
        {ddtrace, state_lock, [ReqId, _Data]},
        Time}) ->
    {Time, Who, {state, {lock, ReqId}}};

handle({trace_ts, Who, 'call', 
        {ddtrace, state_unlock, [_Data]},
        Time}) ->
    {Time, Who, {state, unlock}};

handle({trace_ts, Who, 'call', 
        {ddtrace, state_wait, [_From, ReqId, _Data]},
        Time}) ->
    {Time, Who, {state, {wait, ReqId}}};

handle({trace_ts, Who, 'call', 
        {ddtrace, state_unwait, [ReqId, _Data]},
        Time}) ->
    {Time, Who, {state, {unwait, ReqId}}};

handle({trace_ts, Who, 'call', 
        {ddtrace, handle_event, [enter, OldState, NewState, _Data]},
        Time}) ->
    {Time, Who, {enter, OldState, NewState}};

handle({trace_ts, Who, 'call', 
        {ddtrace, handle_event, [Kind, Msg, State, _Data]}, Time}) ->
    {Time, Who, {Kind, Msg, State}}.
