-module(ddtrace).
-behaviour(gen_statem).

-include("ddtrace.hrl").

%% API
-export([ start/2, start/3, start/4
        , start_link/2, start_link/3, start_link/4
        ]).

%% gen_statem callbacks
-export([init/1, callback_mode/0]).
-export([terminate/3, terminate/2]).

-export([handle_event/4]).

%% DDTrace API
-export([subscribe_deadlocks/1]).

%%%======================
%%% Types
%%%======================

-type process_name() ::
        pid()
      | atom()
      | {global, term()}
      | {via, module(), term()}.

-record(data,
    { worker               :: process_name() % the traced worker process
    , erl_monitor          :: reference()    % the Erlang monitor reference
    , mon_register         :: process_name() % registry of monitors for each worker process
    , mon_state            :: process_name() % the process holding the monitor state
    }).

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

init({Worker, MonRegister, _Opts}) when is_pid(Worker) ->
    process_flag(trap_exit, true),
    
    ErlMon = erlang:monitor(process, Worker),

    mon_reg:set_mon(MonRegister, Worker, self()),
    {ok, MonState} = ddtrace_state:start_link(Worker, MonRegister),

    init_trace(Worker),
    Data = #data{ worker = Worker
                , erl_monitor = ErlMon
                , mon_register = MonRegister
                , mon_state = MonState
                },

    {ok, synced, Data, []}.

callback_mode() ->
    [handle_event_function, state_enter].

terminate(Reason, _State, Data) ->
    terminate(Reason, Data).
terminate(_Reason, Data) ->
    ErlMon = Data#data.erl_monitor,
    erlang:demonitor(ErlMon, [flush]),
    ok.


init_trace(Worker) ->
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
     ).

%%%======================
%%% handle_event: All-time interactions
%%%======================

%% This is only to allow tracer to log state transitions
handle_event(enter, _OldState, _NewState, _Data) ->
    keep_state_and_data;

handle_event({call, From}, subscribe, _State, Data) ->
    cast_mon_state({subscribe, From}, Data),
    keep_state_and_data;

%% Worker process died
handle_event(info, {'DOWN', ErlMon, process, _Pid, _Reason}, _State, Data) ->
    erlang:demonitor(ErlMon, [flush]),
    {stop, normal, Data};

%%%======================
%%% handle_event: Deadlock propagation
%%%======================

handle_event(cast, ?DEADLOCK_PROP(DL), synced, Data) ->
    cast_mon_state(?DEADLOCK_PROP(DL), Data),
    keep_state_and_data;

%%%======================
%%% handle_event: Traces
%%%======================

%% Send query
handle_event(info,
             {trace_ts, _Worker, 'send', ?GS_CALL(ReqId), To, _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?SEND_INFO(To, ?QUERY_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% Send response (alias-based)
handle_event(info,
             {trace_ts, _Worker, 'send', ?GS_RESP_ALIAS(ReqId), To, _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?SEND_INFO(To, ?RESP_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% Send response (plain ReqId)
handle_event(info,
             {trace_ts, _Worker, 'send', ?GS_RESP(ReqId), To, _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?SEND_INFO(To, ?RESP_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% Receive query
handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_CALL_FROM(From, ReqId), _Ts},
             _State,
             Data) ->
    Event = {next_event, internal, ?RECV_INFO(?QUERY_INFO(ReqId))},
    case mon_of(Data, From) of
        undefined ->
            %% If the sender is not being monitored, we fake monitor notification
            FakeNotif = {next_event, cast, ?NOTIFY(From, ?QUERY_INFO(ReqId))},
            Events = [Event, FakeNotif];
        _Pid ->
            Events = [Event]
    end,
    {keep_state_and_data, Events};

%% Receive response (alias-based)
handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_RESP_ALIAS(ReqId), _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?RECV_INFO(?RESP_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% Receive response (plain ReqId)
handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_RESP(ReqId), _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?RECV_INFO(?RESP_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%%%======================
%%% handle_event: Events
%%%======================

%% Send query
handle_event(internal, ?SEND_INFO(To, MsgInfo = ?QUERY_INFO(ReqId)), State, Data)
  when State =:= synced;
       element(1, State) =:= wait_proc
       ->
    call_mon_state({lock, ReqId}, Data),
    send_notif(To, MsgInfo, Data),
    keep_state_and_data;

%% Send response
handle_event(internal, ?SEND_INFO(To, MsgInfo = ?RESP_INFO(_ReqId)), State, Data)
  when State =:= synced;
       element(1, State) =:= wait_proc
       ->
    call_mon_state({unwait, To}, Data),
    send_notif(To, MsgInfo, Data),
    keep_state_and_data;

%% Receive dispatcher: Synced -> wait for monitor notification
handle_event(internal, ?RECV_INFO(MsgInfo), synced, Data) ->
    {next_state, {wait_mon, MsgInfo}, Data};

%% Receive dispatcher: Synced -> wait for process trace
handle_event(cast, ?NOTIFY(From, MsgInfo), synced, Data) ->
    {next_state, {wait_proc, From, MsgInfo}, Data};

%% Receive dispatcher: Receive awaited notification
handle_event(cast, ?NOTIFY(From, MsgInfo), {wait_mon, MsgInfo}, Data) ->
    Event = {next_event, internal, ?HANDLE_RECV(From, MsgInfo)},
    {next_state, handle_recv, Data, Event};

%% Receive dispatcher: Awaiting notification, got irrelevant message
handle_event(_Kind, _Msg, {wait_mon, _MsgInfo}, _Data) ->
    {keep_state_and_data, postpone};

%% Receive dispatcher: Receive awaited process trace
handle_event(internal, ?RECV_INFO(MsgInfo), {wait_proc, From, MsgInfo}, Data) ->
    Event = {next_event, internal, ?HANDLE_RECV(From, MsgInfo)},
    {next_state, handle_recv, Data, Event};

%% Receive dispatcher: Awaiting process trace, got irrelevant message
handle_event(_Kind, _Msg, {wait_proc, _From}, _Data) ->
    {keep_state_and_data, postpone};

%% Receive response
handle_event(internal, ?HANDLE_RECV(_From, ?RESP_INFO(_ReqId)), handle_recv, Data) ->
    call_mon_state(unlock, Data),
    {next_state, synced, Data};

%% Receive query
handle_event(internal, ?HANDLE_RECV(From, ?QUERY_INFO(_ReqId)), handle_recv, Data) ->
    call_mon_state({wait, From}, Data),
    {next_state, synced, Data};

%% Postpone while handling recv
handle_event(internal, _Msg, handle_recv, _Data) ->
    {keep_state_and_data, postpone};

%% Receive probe
handle_event(cast, ?PROBE(Probe, L), synced, Data) ->
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;
handle_event(cast, ?PROBE(Probe, L), {wait_mon, _}, Data) ->
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

handle_event(_Kind, _Msg, _State, _Data) ->
    {keep_state_and_data, postpone}.

%%%======================
%%% Monitor user API
%%%======================

subscribe_deadlocks(Mon) ->
    gen_statem:send_request(Mon, subscribe).


%%%======================
%%% Internal Helper Functions
%%%======================

send_notif(To, MsgInfo, Data) ->
    Mon = mon_of(Data, To),
    case Mon of
        undefined -> ok;
        _ ->
            Worker = Data#data.worker,
            Msg = ?NOTIFY(Worker, MsgInfo),
            gen_statem:cast(Mon, Msg)
    end.


call_mon_state(Msg, Data = #data{mon_state = Pid}) ->
    Resp = gen_server:call(Pid, Msg),
    handle_mon_state_response(Resp, Data).


cast_mon_state(Msg, #data{mon_state = Pid}) ->
    gen_server:cast(Pid, Msg).


handle_mon_state_response(ok, _Data) ->
    ok;
handle_mon_state_response(deadlock, _Data) ->
    %% Worker = Data#data.worker,
    %% exit(Worker, shutdown),
    ok;
handle_mon_state_response({send, Sends}, _Data) ->
    [ gen_statem:cast(ToPid, ?PROBE(Probe, L))
      || {ToPid, ?PROBE(Probe, L)} <- Sends
    ],
    ok.


mon_of(Data, To) ->
    mon_reg:mon_of(Data#data.mon_register, To).
