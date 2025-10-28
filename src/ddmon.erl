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

    init_trace(Worker),
    Data = #data{ worker = Worker
                , mon_register = MonRegister
                , mon_state = ddmon_monitor:start_link(MonRegister)
                , deadlock_subscribers = []
                },
    {ok, state_running, Data, []}.

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
%%% State Function --- traces
%%%======================

-define(GS_CALL_FROM(From, ReqId), {'$gen_call', {From, [alias|ReqId]}, _}).
-define(GS_CALL(ReqId), ?GS_CALL_FROM(_, ReqId)).
-define(GS_RESP(ReqId), {[alias|ReqId], _Msg}).

%% Send query
handle_event(info, _State,
             {trace, _Worker, 'send', ?GS_CALL(ReqId), To, _Ts},
             _Data) ->
    {keep_state_and_data,
     [{next_event, internal, {send, {query, To, ReqId}}}]
    };

%% Send response
handle_event(info, _State,
             {trace, _Worker, 'send', ?GS_RESP(ReqId), To, _Ts},
             _Data) ->
    {keep_state_and_data,
     [{next_event, internal, {send, {response, To, ReqId}}}]
    };

%% Receive query
handle_event(info, _State,
             {trace, _Worker, 'receive', ?GS_CALL_FROM(From, ReqId), _Ts},
             _Data) ->
    {keep_state_and_data,
     [{next_event, internal, {'receive', {query, From, ReqId}}}]
    };

%% Receive response
handle_event(info, _State,
             {trace, _Worker, 'receive', ?GS_RESP(ReqId), _Ts},
             _Data) ->
    {keep_state_and_data,
     [{next_event, internal, {'receive', {response, ReqId}}}]
    };

%%%======================
%%% State Function --- events
%%%======================

%% Send query
handle_event(internal, _State, {send, {query, To, ReqId}}, Data) ->
    call_mon_state({lock, ReqId}, Data),
    send_notif(To, Data),
    keep_state_and_data;

%% Send response
handle_event(internal, _State, {send, {response, To, ReqId}}, Data) ->
    call_mon_state({unwait, ReqId}, Data),
    send_notif(To, Data),
    keep_state_and_data;

%% Receive dispatcher: Synced -> wait for monitor notification
handle_event(internal, synced, Event = {'receive', {query, From, _ReqId}}, Data) ->
    FromMon = mon_of(From, Data),
    {next_state, {wait_mon, {query, FromMon}, Event}, Data};
handle_event(internal, synced, Event = {'receive', {response, _ReqId}}, Data) ->
    {next_state, {wait_mon, response, Event}, Data};

%% Receive dispatcher: Synced -> wait for process trace
handle_event(cast, synced, {notify, FromMon}, Data) ->
    {next_state, {wait_proc, FromMon}, Data};

%% Receive dispatcher: Receive awaited notification
handle_event(cast, {wait_mon, FromMon, Event}, {notify, FromMon}, Data) ->
    {next_state, synced, Data, [{next_event, internal, {add_waitee, ReqId, Pid}}]};

%% Receive dispatcher: Awaiting notification, got irrelevant message
handle_event(cast, {wait_mon, FromMon, Event}, _Msg, _Data) ->
    {keep_state_and_data, postpone};

%% Receive dispatcher: Receive awaited process trace
handle_event(internal, {wait_proc, From}, {'receive', {_Kind, From, _ReqId}}, Data) ->
    {next_state, handle_recv, Data, postpone};

%% Receive dispatcher: Awaiting process trace, got irrelevant message
handle_event(internal, {wait_proc, _From}, _Msg, _Data) ->
    {keep_state_and_data, postpone};

%% Receive response
handle_event(internal, handle_recv, {'receive', response, ReqId, Pid}, Data) ->
    call_mon_state(unlock, Data),
    {next_state, synced, Data};

%% Receive query
handle_event(internal, handle_recv, {'receive', query, ReqId, Pid}, Data) ->
    call_mon_state({wait, Pid}, Data),
    {next_state, synced, Data};

%% Receive probe
handle_event(cast, synced, {probe, Probe}, Data) ->
    call_mon_state({probe, Probe}, Data),
    keep_state_and_data;

%% Postpone probe
handle_event(cast, _State, {probe, _Probe}, _Data) ->
    {keep_state_and_data, postpone}.


%%%======================
%%% Internal Helper Functions
%%%======================

send_notif(To, Data) ->
    gen_statem:cast(mon_of(To, Data), {notify, self()}).

call_mon_state(Msg, #data{mon_state = Pid}) ->
    Resp = gen_server:call(Pid, Msg),
    handle_mon_state_response(Resp).

handle_mon_state_response(ok) ->
    ok;
handle_mon_state_response(deadlock) ->
    throw(deadlock);
handle_mon_state_response({send, Sends}) ->
    [ gen_statem:cast(ToPid, {probe, Probe}) 
      || {ToPid, {probe, Probe}} <- Sends
    ].

mon_of(To, Data) ->
    ddmon_monitor:mon_of(To, Data#data.mon_register).
