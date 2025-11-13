-module(ddtrace).
-behaviour(gen_statem).

-include("ddtrace.hrl").

%% API
-export([ start/2, start/3, start/4
        , start_link/2, start_link/3, start_link/4
        ]).

%% gen_statem callbacks
-export([init/1, callback_mode/0]).
-export([terminate/3]).

-export([handle_event/4]).

%% DDTrace API
-export([subscribe_deadlocks/1, unsubscribe_deadlocks/1, stop_tracer/1]).

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
    , tracer               :: process_name() % the srpc_tracer process
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

init({Worker, MonRegister, Opts}) when is_pid(Worker) ->
    process_flag(trap_exit, true),
    %% io:format("~p: Starting ddtrace for ~p~n", [self(), Worker]),

    TracerMod = proplists:get_value(tracer_mod, Opts, srpc_tracer),
    StateMod = proplists:get_value(state_mod, Opts, ddtrace_state),
    
    ErlMon = erlang:monitor(process, Worker),

    mon_reg:set_mon(MonRegister, Worker, self()),
    {ok, MonState} = StateMod:start_link(Worker, MonRegister),
    {ok, Tracer} = TracerMod:start_link(Worker, self(), MonRegister),

    Data = #data{ worker = Worker
                , erl_monitor = ErlMon
                , mon_register = MonRegister
                , mon_state = MonState
                , tracer = Tracer
                },

    {ok, ?synced, Data, []}.

callback_mode() ->
    [handle_event_function, state_enter].

terminate(_Reason, State, Data) ->
    ErlMon = Data#data.erl_monitor,
    erlang:demonitor(ErlMon, [flush]),
    case State of
        ?wait_mon(_, _) ->
            io:format("~p: Terminating while waiting for monitor notification~n\n\n\n", [self()]);
        ?wait_proc(_, _, _) ->
            io:format("~p: Terminating while waiting for process trace~n\n\n\n", [self()]);
        _ ->
            ok
    end,
    ok.

%%%======================
%%% handle_event: All-time interactions
%%%======================

%% This is used allow the debug tracer (one that traces the monitor, not
%% gen_server) to log state transitions
handle_event(enter, _OldState, _NewState, _Data) ->
    deliver_traces(_Data),
    keep_state_and_data;

handle_event({call, From}, subscribe, _State, Data) ->
    cast_mon_state({subscribe, From}, Data),
    keep_state_and_data;

handle_event({call, From}, unsubscribe, _State, Data) ->
    cast_mon_state({unsubscribe, From}, Data),
    keep_state_and_data;

handle_event({call, From}, stop_tracer, _State, Data) ->
    Tracer = Data#data.tracer,
    gen_statem:call(Tracer, stop),
    {keep_state_and_data, {reply, From, ok}};

%% The worker has attempted a call to itself. When this happens, no actual
%% message is sent. We fake the call message to "detect" the deadlock.
handle_event(info, {'DOWN', _ErlMon, process, Pid, {calling_self, _}}, _State, Data = #data{worker = Pid}) ->
    io:format("AAAAA\n"),
    handle_recv(Data#data.worker, ?QUERY_INFO(make_ref()), Data),
    keep_state_and_data;
%% The worker process has died.
handle_event(info, {'DOWN', ErlMon, process, Pid, Reason}, _State, Data = #data{worker = Pid}) ->
    case is_self_loop(Reason) of
        true ->
            handle_recv(Data#data.worker, ?QUERY_INFO(make_ref()), Data),
            keep_state_and_data;
        false ->
            erlang:demonitor(ErlMon, [flush]),
            {stop, normal, Data}
    end;

%%%======================
%%% handle_event: Deadlock propagation
%%%======================

handle_event(cast, ?DEADLOCK_PROP(DL), ?synced, Data) ->
    state_deadlock(DL, Data),
    keep_state_and_data;

handle_event(cast, ?DEADLOCK_PROP(DL), ?wait_mon(_, _), Data) ->
    state_deadlock(DL, Data),
    keep_state_and_data;


%%%======================
%%% handle_event: Monitor operation
%%%======================

%%%======================
%% Send trace

%% Handle send trace in synced state
handle_event(cast, ?SEND_INFO(To, MsgInfo), ?synced, Data) ->
    Data1 = handle_send(To, MsgInfo, Data),
    send_notif(To, MsgInfo, Data),
    {keep_state, Data1};

%% Handle send trace while awaiting process trace
handle_event(cast, ?SEND_INFO(To, MsgInfo), ?wait_proc(_, _, _), Data) ->
    Data1 = handle_send(To, MsgInfo, Data),
    send_notif(To, MsgInfo, Data),
    {keep_state, Data1};

%% Awaiting notification: postpone
handle_event(cast, ?SEND_INFO(_, _), ?wait_mon(_, _), _Data) ->
    {keep_state_and_data, postpone};

%%%======================
%% Receive trace

%% We were synced, so now we wait for monitor notification
handle_event(cast, ?RECV_INFO(MsgInfo), ?synced, Data) ->
    {next_state, ?wait_mon(MsgInfo, []), Data};

%% Awaited process receive-trace
handle_event(cast, ?RECV_INFO(MsgInfo), ?wait_proc(From, MsgInfo, Rest), Data0) ->
    Data1 = handle_recv(From, MsgInfo, Data0),
    {next_state, ?wait(Rest), Data1};

%% Unwanted process receive-trace. We wait for notification first, and then
%% resume waiting for the process trace.
handle_event(cast, ?RECV_INFO(MsgInfoOther), ?wait_proc(From, MsgInfo, Rest), Data) ->
    ?wait(NewRest) = ?wait_proc(From, MsgInfo, Rest),
    State = ?wait_mon(MsgInfoOther, NewRest),
    {next_state, State, Data};

%% Awaiting notification: postpone
handle_event(cast, ?RECV_INFO(_), ?wait_mon(_, _), _Data) ->
    {keep_state_and_data, postpone};

%%%======================
%% Monitor notification
    
%% We were synced, so now we wait for process trace
handle_event(cast, ?NOTIFY(From, MsgInfo), ?synced, Data) ->
    {next_state, ?wait_proc(From, MsgInfo, []), Data};

%% Awaited notification
handle_event(cast, ?NOTIFY(From, MsgInfo), ?wait_mon(MsgInfo, Rest), Data0) ->
    Data1 = handle_recv(From, MsgInfo, Data0),
    {next_state, ?wait(Rest), Data1};

%% Unwanted notification: postpone
handle_event(cast, ?NOTIFY(_From, _MsgInfoOther), ?wait_mon(_MsgInfo, _Rest), _Data) ->
    {keep_state_and_data, postpone};

%% Awaiting trace: postpone
handle_event(cast, ?NOTIFY(_, _), ?wait_proc(_, _, _), _Data) ->
    {keep_state_and_data, postpone};

%%%======================
%% Probe

%% Handle probe in synced state
handle_event(cast, ?PROBE(Probe, L), ?synced, Data) ->
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

%% Handle probe while awaiting monitor notification (since probes come from
%% monitors). TODO: filter to make sure the probe comes from the right monitor
%% only?
handle_event(cast, ?PROBE(Probe, L), ?wait_mon(?RESP_INFO(_), _), Data) ->
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

%% Handle probe while awaiting monitor notification about a query: wrong
%% direction, postpone
handle_event(cast, ?PROBE(Probe, L), ?wait_mon(?QUERY_INFO(_), _), Data) ->
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

%% Awaiting trace: postpone
handle_event(cast, ?PROBE(_, _), ?wait_proc(_, _, _), _Data) ->
    {keep_state_and_data, postpone};

%%%======================
%% Edge cases

%% We are somehow non-exhaustive or someone's pranked us
handle_event(_Kind, _Msg, _State, _Data) ->
    error({unexpected_event, _Kind, _Msg, _State}).

%%%======================
%%% Monitor user API
%%%======================

stop_tracer(Mon) ->
    gen_statem:call(Mon, stop_tracer).

subscribe_deadlocks(Mon) ->
    gen_statem:send_request(Mon, subscribe).

unsubscribe_deadlocks(Mon) ->
    gen_statem:send_request(Mon, unsubscribe).

%%%======================
%%% Internal Helper Functions
%%%======================

handle_recv(From, ?QUERY_INFO([alias|ReqId]), Data) ->
    state_wait(From, ReqId, Data);
handle_recv(From, ?QUERY_INFO(ReqId), Data) ->
    state_wait(From, ReqId, Data);
handle_recv(_From, ?RESP_INFO(_ReqId), Data) ->
    state_unlock(Data).

handle_send(_To, ?QUERY_INFO(ReqId), Data) ->
    state_lock(ReqId, Data);
handle_send(To, ?RESP_INFO(_ReqId), Data) ->
    state_unwait(To, Data).

state_wait(Who, ReqId, Data) ->
    call_mon_state({wait, Who, ReqId}, Data).

state_unwait(Who, Data) ->
    call_mon_state({unwait, Who}, Data).

state_unlock(Data) ->
    call_mon_state(unlock, Data).
    
state_lock(ReqId, Data) ->
    call_mon_state({lock, ReqId}, Data).

state_deadlock(DL, Data) ->
    call_mon_state(?DEADLOCK_PROP(DL), Data).

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
    handle_mon_state_response(Resp, Data),
    Data.


cast_mon_state(Msg, #data{mon_state = Pid}) ->
    gen_server:cast(Pid, Msg).


handle_mon_state_response(ok, _Data) ->
    ok;
handle_mon_state_response({send, Sends}, _Data) ->
    [ begin
          gen_statem:cast(ToPid, Msg)
      end
      || {ToPid, Msg} <- Sends
    ],
    ok.

deliver_traces(Data) ->
    Worker = Data#data.worker,
    TRef = erlang:trace_delivered(Worker),
    receive {trace_delivered, Worker, TRef} -> ok end.

mon_of(Data, To) ->
    mon_reg:mon_of(Data#data.mon_register, To).

is_self_loop({calling_self, _}) ->
    true;
is_self_loop({E, _}) ->
    is_self_loop(E);
is_self_loop(_) ->
    false.

