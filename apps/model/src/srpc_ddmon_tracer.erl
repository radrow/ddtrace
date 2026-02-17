-module(srpc_ddmon_tracer).
%% @doc """
%% Module to trace a generic server in order of SRPC state transitions. 
%% """

-behaviour(gen_statem).

-include_lib("ddtrace/include/ddtrace.hrl").

-export([start_link/1]).
-export([init/1, callback_mode/0]).
-export([handle_event/4, terminate/3]).

start_link(Worker) ->
    gen_statem:start_link(?MODULE, {Worker}, []).

callback_mode() ->
    handle_event_function.

init({Worker}) ->
    init_trace(Worker),
    process_flag(trap_exit, true),
    erlang:monitor(process, Worker),

    Monitor = mon_reg:mon_of(Worker),

    Data = 
     #{worker => Worker,
       monitor => Monitor,
       requests => #{}
      },
    {ok, unlocked, Data}.

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
%%% handle_event: Traces
%%%======================

%% Casts are ignored
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(info,
             {trace_ts, _Worker, 'send', ?GS_CALL_MSG(_, '$get_workers'), _To, _Ts},
             _State,
             _Data) ->
    keep_state_and_data;

%% Send monitored query
handle_event(info,
             {trace_ts, _Worker, 'send', ?GS_CALL_FROM_MSG(From, [alias|ReqId], Msg), To, _Ts},
             State,
             Data) ->
    handle_event(info,
                 {trace_ts, _Worker, 'send', ?GS_CALL_FROM_MSG(From, ReqId, Msg), To, _Ts},
                 State,
                 Data);
handle_event(info,
             _Trace = {trace_ts, _Worker, 'send', ?GS_CALL_MSG(ReqId, {'$ddmon_monitored_call', _}), To, _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?SEND_INFO(To, ?QUERY_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%% Pass query to the process
handle_event(info,
             _Trace = {trace_ts, _Worker, 'send', ?GS_CALL(_ReqId), _To, _Ts},
             _State,
             _Data) ->
    keep_state_and_data;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Send response from DDMon about deadlock. We ignore it, since ddtrace takes
%% care of propagations.
handle_event(info,
             {trace_ts, _Worker, 'send', {_From, {'$ddmon_deadlock_spread', _DL}}, _To, _Ts},
             _State,
             _Data) ->
    keep_state_and_data;

%% Send response (alias-based)
handle_event(info,
             _Trace = {trace_ts, _Worker, 'send', ?GS_RESP_ALIAS_MSG(ReqId, Msg), To, _Ts},
             State,
             Data) ->
    handle_event(info,
                 {trace_ts, _Worker, 'send', ?GS_RESP_MSG(ReqId, Msg), To, _Ts},
                 State,
                 Data);

%% Send response (plain ReqId)
handle_event(info,
             _Trace = {trace_ts, _Worker, 'send', ?GS_RESP(ReqId), To, _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?SEND_INFO(To, ?RESP_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% Receive monitored query
handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_CALL_MSG(_, '$get_workers'), _Ts},
             _State,
             _Data) ->
    keep_state_and_data;

%% Receive monitored query
handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_CALL_FROM_MSG(From, [alias|ReqId], Msg), _Ts},
             _State,
             Data) ->
    handle_event(info,
                 {trace_ts, _Worker, 'receive', ?GS_CALL_FROM_MSG(From, ReqId, Msg), _Ts},
                 _State,
                 Data);

handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_CALL_FROM_MSG(From, ReqId, {'$ddmon_monitored_call', _}), _Ts},
             _State,
             Data) ->
    Event = {next_event, internal, ?RECV_INFO({From, ?QUERY_INFO(ReqId)})},
    case mon_of(Data, From) of
        undefined ->
            %% If the sender is not being monitored, we fake monitor herald
            FakeNotif = ?HERALD(From, ?QUERY_INFO(ReqId)),
            Monitor = maps:get(monitor, Data),
            gen_statem:cast(Monitor, FakeNotif);
        _Pid -> ok
    end,
    {keep_state_and_data, [Event]};

%% Receive query from the process
handle_event(info,
             _Trace = {trace_ts, _Worker, 'receive', ?GS_CALL_FROM(_From, _ReqId), _Ts},
             _State,
             _Data) ->
    keep_state_and_data;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Receive response from DDMon about deadlock. We ignore it, since ddtrace takes
%% care of propagations.
handle_event(info,
             {trace_ts, _Worker, 'receive', {_From, {'$ddmon_deadlock_spread', _DL}}, _Ts},
             _State,
             _Data) ->
    keep_state_and_data;

%% Receive response (alias-based)
handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_RESP_ALIAS_MSG(ReqId, Msg), _Ts},
             State,
             Data) ->
    handle_event(info,
                 {trace_ts, _Worker, 'receive', ?GS_RESP_MSG(ReqId, Msg), _Ts},
                 State,
                 Data);

%% Receive response (plain ReqId)
handle_event(info,
             {trace_ts, _Worker, 'receive', ?GS_RESP(ReqId), _Ts},
             _State,
             _Data) ->
    Event = {next_event, internal, ?RECV_INFO(?RESP_INFO(ReqId))},
    {keep_state_and_data, [Event]};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The gen_server is either gonna crash or handle this. It definitely won't
%% change its SRPC state.
handle_event(info, {trace_ts, _Worker, 'send_to_non_existing_process', _, _To, _},
             _State, 
             _Data) ->
    keep_state_and_data;

%% Other traces are ignored
handle_event(info, Trace, _State, _Data) when element(1, Trace) =:= trace_ts;
                                              element(1, Trace) =:= trace ->
    keep_state_and_data;

%%%======================
%%% handle_event: Events
%%%======================

handle_event(internal, Ev = ?SEND_INFO(_To, ?QUERY_INFO(ReqId)), unlocked, Data) ->
    gen_statem:cast(maps:get(monitor, Data), Ev),
    {next_state, {locked, ReqId}, Data};

handle_event(internal, _Ev = ?SEND_INFO(_To, ?RESP_INFO(ReqId)), unlocked, Data) ->
    #{requests := Requests} = Data,
    case maps:get(ReqId, Requests, undefined) of
        undefined -> 
            %% No wait for this response
            {keep_state_and_data, postpone};
        ToPid ->
            gen_statem:cast(maps:get(monitor, Data), ?SEND_INFO(ToPid, ?RESP_INFO(ReqId))),
            Data1 = Data#{requests => maps:remove(ReqId, Requests)},
            {keep_state, Data1}
    end;

handle_event(internal, _Ev = ?RECV_INFO({From, ?QUERY_INFO(ReqId)}), State, Data) ->
    [error({not_pid, From}) || not is_pid(From)],
    gen_statem:cast(maps:get(monitor, Data), ?RECV_INFO(?QUERY_INFO(ReqId))),
    #{requests := Requests} = Data,
    Data1 = Data#{requests => Requests#{ReqId => From}},
    
    %% Trigger state refresh to retry postponed events
    {next_state, {refresh_state, State}, Data1, [{next_event, internal, refresh_state}]};

handle_event(internal, Ev = ?RECV_INFO(?RESP_INFO(ReqId)), {locked, ReqId}, Data) ->
    gen_statem:cast(maps:get(monitor, Data), Ev),
    {next_state, unlocked, Data};

%% Forced state change to retry postponed events
handle_event(internal, refresh_state, {refresh_state, NewState}, Data) ->
    {next_state, NewState, Data};

%%%======================
%%% handle_event: Control
%%%======================

handle_event({call, From}, stop, _State, Data) ->
    stop_tracing(Data),
    {keep_state_and_data, {reply, From, ok}};

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
