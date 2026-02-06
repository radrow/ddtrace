-module(srpc_tracer).
%% @doc """ Module to trace a generic server for SRPC events. Because raw
%% tracing can mess up the order of events (especially between 'send' and
%% 'receive'), this module tracks SRPC state to defer ones that obviously came
%% too early. For example, a response to a query that hasn't been observed yet
%% will be postponed until the query trace arrives. Without this, the monitoring
%% algorithm gets confused. """

-behaviour(gen_statem).

-include("ddtrace.hrl").

-export([start_link/2]).
-export([init/1, callback_mode/0]).
-export([handle_event/4, terminate/3]).

start_link(Worker, WorkerPid) ->
    gen_statem:start_link(?MODULE, {Worker, WorkerPid}, []).

callback_mode() ->
    handle_event_function.

init({Worker, WorkerPid}) ->
    process_flag(priority, low),

    init_trace(WorkerPid),
    process_flag(trap_exit, true),
    erlang:monitor(process, WorkerPid),

    Monitor = mon_reg:mon_of(Worker),

    Data = 
     #{worker => Worker,
       worker_pid => WorkerPid,
       monitor => Monitor,
       requests => #{}
      },
    {ok, unlocked, Data}.

init_trace(WorkerPid) ->
    TraceOpts = ['send', 'receive', strict_monotonic_timestamp],
    erlang:trace(WorkerPid, true, TraceOpts),
    
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

    ok.

terminate(_Reason, _State, Data) ->
    stop_tracing(Data),
    ok.

%%%======================
%%% handle_event: Process exit
%%%======================

handle_event(info, {'DOWN', _Ref, process, _Pid, _Reason},
             _State,
             Data) ->
    stop_tracing(Data),
    keep_state_and_data;

%%%======================
%%% handle_event: Translating traces to SRPC events
%%%======================

%% Casts are ignored. This needs to be explicit, otherwise we get a match with
%% call responses.
handle_event(info,
             {trace_ts, _Worker, 'receive', {'$gen_cast', _}, _Ts},
             _State,
             _Data) ->
    keep_state_and_data;
handle_event(info,
             {trace_ts, _Worker, 'send', {'$gen_cast', _}, _To, _Ts},
             _State,
             _Data) ->
    keep_state_and_data;

%% Send query
handle_event(info,
             {trace_ts, _Worker, 'send', ?GS_CALL(ReqId), To, _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?SEND_INFO(To, ?QUERY_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% Send response (alias-based) - lookup the actual destination PID from requests map
handle_event(info,
             {trace_ts, _Worker, 'send', ?GS_RESP_ALIAS_MSG(ReqId, _Msg), _AliasRef, _Ts},
             _State,
             Data) ->
    #{requests := Requests} = Data,
    case maps:get([alias|ReqId], Requests, undefined) of
        undefined ->
            keep_state_and_data;
        ToPid ->
            Event = {next_event, internal, ?SEND_INFO(ToPid, ?RESP_INFO([alias|ReqId]))},
            {keep_state_and_data, [Event]}
    end;

%% Send response (plain ReqId)
handle_event(info,
             {trace_ts, _Worker, 'send', ?GS_RESP(ReqId), To, _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?SEND_INFO(To, ?RESP_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% Receive query (from trace) - store the sender for later reply lookup
handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_CALL_FROM(From, ReqId), _Ts},
             State,
             Data) ->
    Event = {next_event, internal, ?RECV_INFO(?QUERY_INFO(ReqId))},
    
    %% Store the sender's PID for later reply destination lookup
    #{requests := Requests} = Data,
    Data1 = Data#{requests => Requests#{ReqId => From}},
    
    case mon_of(Data, From) of
        undefined ->
            %% If the sender is not being monitored, we fake monitor herald
            FakeNotif = ?HERALD(From, ?QUERY_INFO(ReqId)),
            Monitor = maps:get(monitor, Data),
            gen_statem:cast(Monitor, FakeNotif);
        _Pid -> ok
    end,
    
    %% Trigger state refresh to retry postponed events
    {next_state, state_change, Data1, [Event, {next_event, internal, {refresh_state, State}}]};

%% Receive response (alias-based) - preserve the full [alias|ReqId] format
handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_RESP_ALIAS_MSG(ReqId, _Msg), _Ts},
             _State,
             _Data) ->
    %% Keep the full [alias|ReqId] format for state matching
    Event = {next_event, internal, ?RECV_INFO(?RESP_INFO([alias|ReqId]))},
    {keep_state_and_data, [Event]};

%% Receive response (plain ReqId)
handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_RESP(ReqId), _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?RECV_INFO(?RESP_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% The gen_server is either gonna crash or handle this somehow. It definitely
%% won't change its SRPC state.
handle_event(info, {trace_ts, Worker, 'send_to_non_existing_process', _, To, _},
             _State, 
             _Data) ->
    logger:warning("~p: send_to_non_existing_process (~p) trace ignored", [Worker, To], #{module => ?MODULE, subsystem => ddtrace}),
    keep_state_and_data;


%% Other traces are ignored
handle_event(info, Trace, _State, _Data) when element(1, Trace) =:= trace_ts;
                                              element(1, Trace) =:= trace ->
    keep_state_and_data;

%%%======================
%%% handle_event: SRPC events
%%%======================

%% Send query
handle_event(internal, Ev = ?SEND_INFO(_To, ?QUERY_INFO(ReqId)), unlocked, Data) ->
    gen_statem:cast(maps:get(monitor, Data), Ev),
    {next_state, {locked, ReqId}, Data};

%% Send query when locked - postpone
handle_event(internal, ?SEND_INFO(_To, ?QUERY_INFO(_ReqId)), {locked, _LockedReqId}, _Data) ->
    {keep_state_and_data, postpone};

%% Send response
handle_event(internal, Ev = ?SEND_INFO(_To, ?RESP_INFO(ReqId)), unlocked, Data) ->
    #{requests := Requests} = Data,
    case maps:get(ReqId, Requests, undefined) of
        undefined -> 
            {keep_state_and_data, postpone};
        _ ->
            gen_statem:cast(maps:get(monitor, Data), Ev),
            Data1 = Data#{requests => maps:remove(ReqId, Requests)},
            {keep_state, Data1}
    end;

%% Receive query
handle_event(internal, Ev = ?RECV_INFO(?QUERY_INFO(_ReqId)), _State, Data) ->
    gen_statem:cast(maps:get(monitor, Data), Ev),
    keep_state_and_data;

%% Receive response (matching locked ReqId)
handle_event(internal, Ev = ?RECV_INFO(?RESP_INFO(ReqId)), {locked, ReqId}, Data) ->
    gen_statem:cast(maps:get(monitor, Data), Ev),
    {next_state, unlocked, Data};

%% Receive response (non-matching or wrong state) - postpone
handle_event(internal, ?RECV_INFO(?RESP_INFO(_ReqId)), _State, _Data) ->
    {keep_state_and_data, postpone};

%%%======================
%%% handle_event: Control
%%%======================

%% Forced state change to retry postponed events
handle_event(internal, {refresh_state, NewState}, state_change, _Data) ->
    {next_state, NewState, _Data};

%% Stop tracer
handle_event({call, From}, stop, _State, Data) ->
    stop_tracing(Data),
    {keep_state_and_data, {reply, From, ok}};

%% We postpone unexpected events naively trusting that no one is trolling us.
handle_event(_Kind, _Ev, _State, _Data) ->
    {keep_state_and_data, postpone}.

%%%======================
%%% Internal functions
%%%======================

mon_of(_Data, To) ->
    mon_reg:mon_of(To).

stop_tracing(Data) ->
    #{worker := Worker} = Data,
    catch erlang:trace(Worker, false, ['receive', 'send']),
    ok.

