-module(ddmon).
-behaviour(gen_statem).

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

-define(GS_CALL_FROM(From, ReqId), {'$gen_call', {From, [alias|ReqId]}, _}).
-define(GS_CALL(ReqId), ?GS_CALL_FROM(_, ReqId)).
-define(GS_RESP(ReqId), {[alias|ReqId], _Msg}).

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

init({Worker, MonRegister, _Opts}) ->
    process_flag(trap_exit, true),
    
    mon_reg:set_mon(MonRegister, Worker, self()),

    init_trace(Worker),
    Data = #data{ worker = Worker
                , mon_register = MonRegister
                , mon_state = ddmon_monitor:start_link(MonRegister)
                , deadlock_subscribers = []
                },

    

    {ok, synced, Data, []}.

callback_mode() ->
    state_event_function.

terminate(Reason, _State, Data) ->
    terminate(Reason, Data).
terminate(_Reason, _Data) ->
    ok.


init_trace(Worker) ->
    Session = trace:session_create(?MODULE, self(), []),
    TraceOpts = ['send', 'receive', strict_monotonic_timestamp],
    trace:process(Session, Worker, true, TraceOpts),

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
%%% All-time interactions
%%%======================

handle_event(cast, _State, {subscribe, From}, Data = #data{deadlock_subscribers = DLS}) ->
    Data1 = Data#data{deadlock_subscribers = [From | DLS]},
    {keep_state, Data1};

handle_event(cast, _State, {unsubscribe, From}, Data = #data{deadlock_subscribers = DLS}) ->
    Data1 = Data#data{deadlock_subscribers = lists:delete(From, DLS)},
    {keep_state, Data1};

%%%======================
%%% State Function --- traces
%%%======================

%% Send query
handle_event(info, _State,
             {trace, _Worker, 'send', ?GS_CALL(ReqId), To, _Ts},
             _Data) ->
    Event = {next_event, internal, ?SEND_INFO(To, ?QUERY_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% Send response
handle_event(info, _State,
             {trace, _Worker, 'send', ?GS_RESP(ReqId), To, _Ts},
             _Data) ->
    Event = {next_event, internal, ?SEND_INFO(To, ?RESP_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% Receive query
handle_event(info, _State,
             {trace, _Worker, 'receive', ?GS_CALL(ReqId), _Ts},
             _Data) ->
    Event = {next_event, internal, ?RECV_INFO(?QUERY_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% Receive response
handle_event(info, _State,
             {trace, _Worker, 'receive', ?GS_RESP(ReqId), _Ts},
             _Data) ->
    Event = {next_event, internal, ?RECV_INFO(?RESP_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%%%======================
%%% State Function --- events
%%%======================

%% Send query
handle_event(internal, _State, ?SEND_INFO(To, MsgInfo = ?QUERY_INFO(ReqId)), Data) ->
    call_mon_state({lock, ReqId}, Data),
    send_notif(To, MsgInfo, Data),
    keep_state_and_data;

%% Send response
handle_event(internal, _State, ?SEND_INFO(To, MsgInfo = ?RESP_INFO(ReqId)), Data) ->
    call_mon_state({unwait, ReqId}, Data),
    send_notif(To, MsgInfo, Data),
    keep_state_and_data;

%% Receive dispatcher: Synced -> wait for monitor notification
handle_event(internal, synced, ?RECV_INFO(MsgInfo), Data) ->
    {next_state, {wait_mon, MsgInfo}, Data};

%% Receive dispatcher: Synced -> wait for process trace
handle_event(cast, synced, ?NOTIFY(From, MsgInfo), Data) ->
    {next_state, {wait_proc, From, MsgInfo}, Data};

%% Receive dispatcher: Receive awaited notification
handle_event(cast, {wait_mon, MsgInfo}, ?NOTIFY(From, MsgInfo), Data) ->
    Event = {next_event, internal, ?HANDLE_RECV(From, MsgInfo)},
    {next_state, handle_recv, Data, Event};

%% Receive dispatcher: Awaiting notification, got irrelevant message
handle_event(cast, {wait_mon, _MsgInfo}, _Msg, _Data) ->
    {keep_state_and_data, postpone};

%% Receive dispatcher: Receive awaited process trace
handle_event(internal, {wait_proc, From}, ?RECV_INFO(MsgInfo), Data) ->
    Event = {next_event, internal, ?HANDLE_RECV(From, MsgInfo)},
    {next_state, handle_recv, Data, Event};

%% Receive dispatcher: Awaiting process trace, got irrelevant message
handle_event(internal, {wait_proc, _From}, _Msg, _Data) ->
    {keep_state_and_data, postpone};

%% Receive response
handle_event(internal, handle_recv, ?HANDLE_RECV(_From, ?RESP_INFO(_ReqId)), Data) ->
    call_mon_state(unlock, Data),
    {next_state, synced, Data};

%% Receive query
handle_event(internal, handle_recv, ?HANDLE_RECV(From, ?QUERY_INFO(_ReqId)), Data) ->
    call_mon_state({wait, From}, Data),
    {next_state, synced, Data};

%% Postpone while handling recv
handle_event(internal, handle_recv, _Msg, _Data) ->
    {keep_state_and_data, postpone};

%% Receive probe
handle_event(cast, synced, ?PROBE(Probe), Data) ->
    call_mon_state(?PROBE(Probe), Data),
    keep_state_and_data;

%% Postpone probe
handle_event(cast, _State, ?PROBE(_), _Data) ->
    {keep_state_and_data, postpone}.


%%%======================
%%% Internal Helper Functions
%%%======================

send_notif(To, MsgInfo, Data) ->
    Mon = mon_of(To, Data),
    Worker = Data#data.worker,
    Msg = ?NOTIFY(Worker, MsgInfo),
    gen_statem:cast(Mon, Msg).

call_mon_state(Msg, #data{mon_state = Pid}) ->
    Resp = gen_server:call(Pid, Msg),
    handle_mon_state_response(Resp).

handle_mon_state_response(ok) ->
    ok;
handle_mon_state_response(deadlock) ->
    throw(deadlock);
handle_mon_state_response({send, Sends}) ->
    [ gen_statem:cast(ToPid, ?PROBE(Probe)) 
      || {ToPid, ?PROBE(Probe)} <- Sends
    ].

mon_of(To, Data) ->
    mon_reg:mon_of(Data#data.mon_register, To).
