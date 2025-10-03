%% Tracer (aka "monitor") for `gen_server` to detect deadlocks. In this version,
%% reacts to `call` messages and responses only. When notified about an outgoing
%% message, it sends a notification to the tracer of the recipient. Therefore,
%% the receiving tracer learns about incoming messages from two channels: from
%% its overseen process and from the tracer of the sender. Tracers try to keep
%% information about receiving messages in sync: when a notification comes from
%% another tracer, they temporarily await a confirmation from the process,
%% postponing all external messages, and vice-versa. This naturally yields 3
%% states of the tracer: synced, awaiting external tracer-notification, awaiting
%% trace from the worker.
%%
%% Tracers deduce whether the worker is locked by observing its communication:
%% an outgoing `call` request indicates a lock on the recipient until a response
%% is received. If a `call` is received, the sender's id is registered in the
%% `waitees` list, until a response is sent to it. When the monitor considers
%% its process locked, it maintains a unique probe (that happens to be the
%% request id on which the process is waiting) which is sent to the tracer of
%% every process that becomes locked on this process. If a foreign probe is
%% received, it is propagated to tracers of all members of the `waitees` If a
%% foreign probe is received, it is probapagated to watracers of all members of
%% the ```waitees``` list. If own probe is received, a deadlock is reported.
-module(ddmon).
-behaviour(gen_statem).

-include("ddmon.hrl").

%% API
-export([ start/2, start/3, start/4
        , start_link/2, start_link/3, start_link/4
        ]).

%% gen_statem callbacks
-export([init/1, callback_mode/0]).
-export([terminate/3, terminate/2]).

%% States
-export([state_synced/3, state_wait_worker/3, state_wait_mon/3]).

%% DDMon API
-export([]).

%%%======================
%%% DDMon Types
%%%======================

-record(data,
        { worker :: pid()
        , mon_register :: pid()
        , probe :: gen_server:request_id() | undefined
        , waitees :: gen_server:request_id_collection()
        , deadlock_subscribers :: list(pid())
        }).
-type data() :: #data{}.

%%%======================
%%% API Functions
%%%======================

start(Worker, MonRegister) ->
    start(Worker, MonRegister, [], []).
start(Worker, MonRegister, Opts) ->
    start(Worker, MonRegister, Opts, []).
start(Worker, MonRegister, Opts, GenOpts) ->
    gen_statem:start(?MODULE, {Worker, MonRegister, Opts}, GenOpts).

start_link(Worker, MonRegister) ->
    start_link(Worker, MonRegister, [], []).
start_link(Worker, MonRegister, Opts) ->
    start_link(Worker, MonRegister, Opts, []).
start_link(Worker, MonRegister, Opts, GenOpts) ->
    gen_statem:start_link(?MODULE, {Worker, MonRegister, Opts}, GenOpts).

%%%======================
%%% gen_statem Callbacks
%%%======================

init({Worker, MonRegister, Opts}) ->
    process_flag(trap_exit, true),
    put(trace_int, proplists:get_value(trace_int, Opts, true)),
    put(live_log, proplists:get_value(live_log, Opts, false)),

    TraceOpts = ['send', 'receive', strict_monotonic_timestamp],
    erlang:trace(Worker, true, TraceOpts),

    erlang:trace_pattern(
      'send',
      [ {['_', {'$gen_call', '_', '_'}], [], []} % gen_server call
      , {['_', {'_', '_'}], [], []} % gen_server reply
      ]
     ),
    erlang:trace_pattern(
      'receive',
      [ {['_', '_', {'$gen_call', '_', '_'}], [], []} % gen_server call
      , {['_', '_', {'$1', '_'}], [{'=/=', '$1', code_server}], []} % gen_server reply
      ]
     ),

    Data = #data{ worker = Worker
                , mon_register = MonRegister
                , probe = undefined
                , waitees = gen_server:request_id_collection()
                , deadlock_subscribers = []
                },
    {ok, unlocked, Data, [{next_event, internal, probe}]}.

callback_mode() ->
    state_functions.

terminate(Reason, _State, Data) ->
    terminate(Reason, Data).
terminate(_Reason, _Data) ->
    ok.

%%%======================
%%% State Functions
%%%======================

state_synced(cast, {subscribe, Pid}, Data) ->
    {keep_state_and_data, [{reply, Pid, ok}]};
state_synced(cast, {unsubscribe, Pid}, Data) ->
    NewSubs = lists:delete(Pid, Data#data.deadlock_subscribers);
state_synced(cast, _Msg, Data) ->
    {keep_state_and_data, []};
state_synced(info, {trace, From, send, To, Msg, _Ts}, Data) ->
    case Msg of
        {'$gen_call', ReqId, _Call} ->
            % Outgoing call, register probe and notify recipient's monitor
            Probe = ReqId,
            MonRegister = Data#data.mon_register,
            MonRegister ! {trace, self(), send, To, {probe, Probe}},
            NewData = Data#data{ probe = Probe },
            {next_state, locked, NewData, []};
        {'$', _ReplyTo, _Response} ->
            % Outgoing reply, nothing to do
            {keep_state_and_data, []};
        _Other ->
            % Other outgoing message, nothing to do
            {keep_state_and_data, []}
    end;
state_synced(info, {trace, _From, receive, _To, Msg, _Ts}, Data) ->
    case Msg of
        {'$gen_call', ReqId, _Call} ->
                                                % Incoming call, register sender as waitee and notify sender's monitor
            NewWaitees = gen_server:request_id_collection_add(ReqId, Data#data.waitees),
            MonRegister = Data#data.mon_register,
            {FromPid, _Alias} = ReqId,
            MonRegister ! {trace, self(), send, FromPid, {probe, ReqId}},
            NewData = Data#data{ waitees = NewWaitees },
            {keep_state, NewData, []};
        {'$', ReplyTo, _Response} ->
            % Incoming reply, nothing to do
            {keep_state_and_data, []};

        _Other ->
            % Other incoming message, nothing to do
            {keep_state_and_data, []}
    end;
state_synced(info, {trace, _From, probe, Probe}, Data) ->
    case Data#data.probe of
        Probe ->
            % Own probe received, deadlock detected
            DeadlockMsg = {?DEADLOCK, Probe},
            lists:foreach(fun(Pid) -> Pid ! DeadlockMsg end, Data#data.deadlock_subscribers),
            NewData = Data#data{ probe = undefined },
            {keep_state, NewData, []};
        undefined ->
            % No own probe, spurious notification, ignore
            {keep_state_and_data, []};
        _OtherProbe ->
            % Different probe, enter wait state
            {next_state, wait_mon, Data, []}
    end;
state_synced(internal, probe, Data) ->
    % Periodic probe check, nothing to do
    {keep_state_and_data, [{next_event, internal, probe}]}.
state_synced(info, {'EXIT', _Pid, _Reason}, Data) ->
    % Worker exited, terminate monitor
    {stop, normal, Data};
state_synced(_EventType, _EventContent, Data) ->
    {keep_state_and_data, []}.

state_wait_worker(cast, {subscribe, Pid}, Data) ->
    {keep_state_and_data, [{reply, Pid, ok}]};
state_wait_worker(cast, {unsubscribe, Pid}, Data) ->
    NewSubs = lists:delete(Pid, Data#data.deadlock_subscribers);
state_wait_worker(cast, _Msg, Data) ->
    {keep_state_and_data, []};
state_wait_worker(info, {trace, From, send, To, Msg, _Ts}, Data) ->
    case Msg of
        {'$gen_call', ReqId, _Call} ->
            % Outgoing call, register probe and notify recipient's monitor
            Probe = ReqId,
            MonRegister = Data#data.mon_register,
            MonRegister ! {trace, self(), send, To, {probe, Probe}},
            NewData = Data#data{ probe = Probe },
            {next_state, locked, NewData, []};
        {'$', _ReplyTo, _Response} ->
            % Outgoing reply, nothing to do
            {keep_state_and_data, []};
        _Other ->
            % Other outgoing message, nothing to do
            {keep_state_and_data, []}
    end;
state_wait_worker(info, {trace, _From, receive, _To, Msg, _Ts}, Data) ->
    case Msg of
        {'$gen_call', ReqId, _Call} ->
            % Incoming call, register sender as waitee and notify sender's monitor
            NewWaitees = gen_server:request_id_collection_add(ReqId, Data#data.waitees),
            MonRegister = Data#data.mon_register,
            {FromPid, _Alias} = ReqId,
            MonRegister ! {trace, self(), send, FromPid, {probe, ReqId}},
            NewData = Data#data{ waitees = NewWaitees },
            {keep_state, NewData, []};
        {'$', ReplyTo, _Response} ->
            % Incoming reply, nothing to do
            {keep_state_and_data, []};
        _Other ->
            % Other incoming message, nothing to do
            {keep_state_and_data, []}
    end;
state_wait_worker(info, {trace, _From, probe, Probe}, Data) ->
    case Data#data.probe of
        Probe ->
            % Own probe received, deadlock detected
            DeadlockMsg = {?DEADLOCK, Probe},
            lists:foreach(fun(Pid) -> Pid ! DeadlockMsg end, Data#data.deadlock_subscribers),
            NewData = Data#data{ probe = undefined },
            {keep_state, NewData, []};
        undefined ->
            % No own probe, spurious notification, ignore
            {keep_state_and_data, []};
        _OtherProbe ->
            % Different probe, stay in wait state
            {keep_state_and_data, []}
    end;
state_wait_worker(internal, probe, Data) ->
    % Periodic probe check, nothing to do
    {keep_state_and_data, [{next_event, internal, probe}]};
state_wait_worker(info, {'EXIT', _Pid, _Reason}, Data) ->
    % Worker exited, terminate monitor
    {stop, normal, Data};
state_wait_worker(_EventType, _EventContent, Data) ->
    {keep_state_and_data, []}.

state_wait_mon(cast, {subscribe, Pid}, Data) ->
    {keep_state_and_data, [{reply, Pid, ok}]};
state_wait_mon(cast, {unsubscribe, Pid}, Data) ->
    NewSubs = lists:delete(Pid, Data#data.deadlock_subscribers);
state_wait_mon(cast, _Msg, Data) ->
    {keep_state_and_data, []};
state_wait_mon(info, {trace, From, send, To, Msg, _Ts}, Data) ->
    case Msg of
        {'$gen_call', ReqId, _Call} ->
            % Outgoing call, register probe and notify recipient's monitor
            Probe = ReqId,
            MonRegister = Data#data.mon_register,
            MonRegister ! {trace, self(), send, To, {probe, Probe}},
            NewData = Data#data{ probe = Probe },
            {next_state, locked, NewData, []};
        {'$', _ReplyTo, _Response} ->
            % Outgoing reply, nothing to do
            {keep_state_and_data, []};
        _Other ->
            % Other outgoing message, nothing to do
            {keep_state_and_data, []}
    end;
state_wait_mon(info, {trace, _From, 'receive', _To, Msg, _Ts}, Data) ->
    case Msg of
        {'$gen_call', ReqId, _Call} ->
            % Incoming call, register sender as waitee and notify sender's monitor
            NewWaitees = gen_server:request_id_collection_add(ReqId, Data#data.waitees),
            MonRegister = Data#data.mon_register,
            {FromPid, _Alias} = ReqId,
            MonRegister ! {trace, self(), send, FromPid, {probe, ReqId}},
            NewData = Data#data{ waitees = NewWaitees },
            {keep_state, NewData, []};
        {'$', ReplyTo, _Response} ->
            % Incoming reply, nothing to do
            {keep_state_and_data, []};
        _Other ->
            % Other incoming message, nothing to do
            {keep_state_and_data, []}
    end;
state_wait_mon(info, {trace, _From, probe, Probe}, Data) ->
    case Data#data.probe of
        Probe ->
            % Own probe received, deadlock detected
            DeadlockMsg = {?DEADLOCK, Probe},
            lists:foreach(fun(Pid) -> Pid ! DeadlockMsg end, Data#data.deadlock_subscribers),
            NewData = Data#data{ probe = undefined },
            {keep_state, NewData, []};
        undefined ->
            % No own probe, spurious notification, ignore
            {keep_state_and_data, []};
        _OtherProbe ->
            % Different probe, stay in wait state
            {keep_state_and_data, []}
    end;
state_wait_mon(internal, probe, Data) ->
    % Periodic probe check, nothing to do
    {keep_state_and_data, [{next_event, internal, probe}]};
state_wait_mon(info, {'EXIT', _Pid, _Reason}, Data) ->
    % Worker exited, terminate monitor
    {stop, normal, Data};
state_wait_mon(_EventType, _EventContent, Data) ->
    {keep_state_and_data, []}.

%%%======================
%%% Internal Functions
%%%======================
