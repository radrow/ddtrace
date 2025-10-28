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

-export([handle_event/4]).

%% DDMon API
-export([]).

%%%======================
%%% DDMon Types
%%%======================

-type process_name() ::
        pid()
      | atom()
      | {global, term()}
      | {via, module(), term()}.

-record(data,
    { worker               :: process_name() % the traced worker process
    , mon_register         :: process_name() % registry of monitors for each worker process
    , mon_state            :: process_name() % the process holding the monitor state
    , deadlock_subscribers :: [process_name()]
    }).

-define(RECV_INFO(MsgInfo), {'receive', MsgInfo}).
-define(SEND_INFO(To, MsgInfo), {send, To, MsgInfo}).
-define(PROBE(Probe), {probe, Probe}).
-define(QUERY_INFO(ReqId), {query, ReqId}).
-define(RESP_INFO(ReqId), {response, ReqId}).
-define(NOTIFY(From, MsgInfo), {notify, From, MsgInfo}).
-define(HANDLE_RECV(From, MsgInfo), {'receive', From, MsgInfo}).

%% -define(GS_CALL_FROM(From, ReqId), {'$gen_call', {From, [alias|ReqId]}, _}).
-define(GS_CALL_FROM(From, ReqId), {'$gen_call', {From, ReqId}, _}).
-define(GS_CALL(ReqId), ?GS_CALL_FROM(_, ReqId)).
-define(GS_RESP_ALIAS(ReqId), {[alias|ReqId], _Msg}).
-define(GS_RESP(ReqId), {ReqId, _Msg}).

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
    
    mon_reg:set_mon(MonRegister, Worker, self()),
    {ok, MonState} = ddmon_monitor:start_link(MonRegister),

    init_trace(Worker),
    Data = #data{ worker = Worker
                , mon_register = MonRegister
                , mon_state = MonState
                , deadlock_subscribers = []
                },

    

    {ok, synced, Data, []}.

callback_mode() ->
    handle_event_function.

terminate(Reason, _State, Data) ->
    terminate(Reason, Data).
terminate(_Reason, _Data) ->
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

handle_event(cast, {subscribe, From}, _State, Data = #data{deadlock_subscribers = DLS}) ->
    Data1 = Data#data{deadlock_subscribers = [From | DLS]},
    {keep_state, Data1};

handle_event(cast, {unsubscribe, From}, _State, Data = #data{deadlock_subscribers = DLS}) ->
    Data1 = Data#data{deadlock_subscribers = lists:delete(From, DLS)},
    {keep_state, Data1};

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
            Events = [Event, {next_event, cast, ?NOTIFY(From, ?QUERY_INFO(ReqId))}];
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
handle_event(internal, ?SEND_INFO(To, MsgInfo = ?QUERY_INFO(ReqId)), _State, Data) ->
    io:format("~p: trace send query to ~p~n", [self(), To]),
    call_mon_state({lock, ReqId}, Data),
    send_notif(To, MsgInfo, Data),
    keep_state_and_data;

%% Send response
handle_event(internal, ?SEND_INFO(To, MsgInfo = ?RESP_INFO(_ReqId)), _State, Data) ->
    io:format("~p: trace send response to ~p~n", [self(), To]),
    call_mon_state({unwait, To}, Data),
    send_notif(To, MsgInfo, Data),
    keep_state_and_data;

%% Receive dispatcher: Synced -> wait for monitor notification
handle_event(internal, ?RECV_INFO(MsgInfo), synced, Data) ->
    io:format("~p: trace receive of ~p~n", [self(), MsgInfo]),
    {next_state, {wait_mon, MsgInfo}, Data};

%% Receive dispatcher: Synced -> wait for process trace
handle_event(cast, ?NOTIFY(From, MsgInfo), synced, Data) ->
    io:format("~p: trace notify from ~p of ~p~n", [self(), From, MsgInfo]),
    {next_state, {wait_proc, From, MsgInfo}, Data};

%% Receive dispatcher: Receive awaited notification
handle_event(cast, ?NOTIFY(From, MsgInfo), {wait_mon, MsgInfo}, Data) ->
    io:format("~p: receive awaited notif from ~p of ~p~n", [self(), From, MsgInfo]),
    Event = {next_event, internal, ?HANDLE_RECV(From, MsgInfo)},
    {next_state, handle_recv, Data, Event};

%% Receive dispatcher: Awaiting notification, got irrelevant message
handle_event(_Kind, _Msg, {wait_mon, _MsgInfo}, _Data) ->
    io:format("~p: postponed msg ~p~n", [self(), _Msg]),
    {keep_state_and_data, postpone};

%% Receive dispatcher: Receive awaited process trace
handle_event(internal, ?RECV_INFO(MsgInfo), {wait_proc, From}, Data) ->
    io:format("~p: receive awaited message ~p~n", [self(), MsgInfo]),
    Event = {next_event, internal, ?HANDLE_RECV(From, MsgInfo)},
    {next_state, handle_recv, Data, Event};

%% Receive dispatcher: Awaiting process trace, got irrelevant message
handle_event(_Kind, _Msg, {wait_proc, _From}, _Data) ->
    io:format("~p: postponed msg ~p~n", [self(), _Msg]),
    {keep_state_and_data, postpone};

%% Receive response
handle_event(internal, ?HANDLE_RECV(_From, ?RESP_INFO(_ReqId)), handle_recv, Data) ->
    io:format("~p: handle received response from ~p", [self(), _From]),
    call_mon_state(unlock, Data),
    {next_state, synced, Data};

%% Receive query
handle_event(internal, ?HANDLE_RECV(From, ?QUERY_INFO(_ReqId)), handle_recv, Data) ->
    io:format("~p: handle received query from ~p~n", [self(), From]),
    call_mon_state({wait, From}, Data),
    {next_state, synced, Data};

%% Postpone while handling recv
handle_event(internal, _Msg, handle_recv, _Data) ->
    io:format("~p: postpone while handling recv~n", [self()]),
    {keep_state_and_data, postpone};

%% Receive probe
handle_event(cast, ?PROBE(Probe), synced, Data) ->
    io:format("~p: receive probe ~p~n", [self(), Probe]),
    call_mon_state(?PROBE(Probe), Data),
    keep_state_and_data;

%% Postpone probe
handle_event(cast, ?PROBE(_), _State, _Data) ->
    io:format("~p: postpone probe~n", [self()]),
    {keep_state_and_data, postpone}.


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

handle_mon_state_response(ok, _Data) ->
    ok;
handle_mon_state_response(deadlock, Data) ->
    %% Notify subscribers and keep running
    io:format("~p: DEADLOCK~n", [self()]),
    notify_deadlock_subscribers(Data),
    ok;
handle_mon_state_response({send, Sends}, _Data) ->
    io:format("~p: sends ~p~n", [self(), Sends]),
    [ gen_statem:cast(ToPid, ?PROBE(Probe))
      || {ToPid, ?PROBE(Probe)} <- Sends
    ],
    ok.

notify_deadlock_subscribers(#data{deadlock_subscribers = Subs, worker = Worker}) ->
    [ catch (Sub ! {?DEADLOCK, Worker}) || Sub <- Subs ],
    ok.

mon_of(Data, To) ->
    mon_reg:mon_of(Data#data.mon_register, To).
