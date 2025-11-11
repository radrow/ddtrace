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

init({Worker, MonRegister, _Opts}) when is_pid(Worker) ->
    process_flag(trap_exit, true),
    
    ErlMon = erlang:monitor(process, Worker),

    mon_reg:set_mon(MonRegister, Worker, self()),
    {ok, MonState} = ddtrace_state:start_link(Worker, MonRegister),
    {ok, Tracer} = srpc_tracer:start_link(Worker, self(), MonRegister),

    Data = #data{ worker = Worker
                , erl_monitor = ErlMon
                , mon_register = MonRegister
                , mon_state = MonState
                , tracer = Tracer
                },

    {ok, ?synced, Data, []}.

callback_mode() ->
    [handle_event_function, state_enter].

terminate(Reason, _State, Data) ->
    terminate(Reason, Data).
terminate(_Reason, Data) ->
    ErlMon = Data#data.erl_monitor,
    erlang:demonitor(ErlMon, [flush]),
    ok.

%%%======================
%%% handle_event: All-time interactions
%%%======================

%% This is only to allow tracer to log state transitions
handle_event(enter, _OldState, _NewState, _Data) ->
    %% Worker = Data#data.worker,
    %% TRef = erlang:trace_delivered(Worker),
    %% receive {trace_delivered, Worker, TRef} -> ok end,
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

%% Worker process died
handle_event(info, {'DOWN', ErlMon, process, _Pid, _Reason}, _State, Data) ->
    erlang:demonitor(ErlMon, [flush]),
    {stop, normal, Data};

%%%======================
%%% handle_event: Deadlock propagation
%%%======================

handle_event(cast, ?DEADLOCK_PROP(DL), ?synced, Data) ->
    state_deadlock(DL, Data),
    keep_state_and_data;
%% handle_event(cast, ?DEADLOCK_PROP(DL), ?wait_mon(_, _), Data) ->
%%     state_deadlock(DL, Data),
%%     keep_state_and_data;


%%%======================
%%% handle_event: Events
%%%======================

%% Send
handle_event(cast, ?SEND_INFO(To, MsgInfo), ?synced, Data) ->
    Data1 = handle_send(To, MsgInfo, Data),
    send_notif(To, MsgInfo, Data),
    {keep_state, Data1};
handle_event(cast, ?SEND_INFO(To, MsgInfo), ?wait_proc(_, _, _), Data) ->
    Data1 = handle_send(To, MsgInfo, Data),
    send_notif(To, MsgInfo, Data),
    {keep_state, Data1};

%% Receive dispatcher: Synced -> wait for monitor notification
handle_event(cast, ?RECV_INFO(MsgInfo), ?synced, Data) ->
    {next_state, ?wait_mon(MsgInfo, []), Data};

%% Receive dispatcher: Synced -> wait for process trace
handle_event(cast, ?NOTIFY(From, MsgInfo), ?synced, Data) ->
    {next_state, ?wait_proc(From, MsgInfo, []), Data};

%% Receive dispatcher: Receive awaited notification
handle_event(cast, ?NOTIFY(From, MsgInfo), ?wait_mon(MsgInfo, Rest), Data0) ->
    Data1 = handle_recv(From, MsgInfo, Data0),
    {next_state, ?wait(Rest), Data1};

%% Receive dispatcher: Awaiting notification, got irrelevant message
handle_event(_Kind, _Msg, ?wait_mon(_MsgInfo, _Rest), _Data) ->
    {keep_state_and_data, postpone};

%% Receive dispatcher: Receive awaited process trace
handle_event(cast, ?RECV_INFO(MsgInfo), ?wait_proc(From, MsgInfo, Rest), Data0) ->
    Data1 = handle_recv(From, MsgInfo, Data0),
    {next_state, ?wait(Rest), Data1};

%% Receive dispatcher: Receive unwanted process trace
handle_event(cast, ?RECV_INFO(MsgInfo), ?wait_proc(From, MsgInfoOther, Rest), Data) ->
    State = ?wait_mon(MsgInfoOther, ?wait_proc(From, MsgInfo, Rest)),
    {next_state, State, Data};
    
%% Receive dispatcher: Awaiting process trace, got irrelevant message
handle_event(_Kind, _Msg, ?wait_proc(_From, _MsgInfo, _Rest), _Data) ->
    {keep_state_and_data, postpone};

%% Receive probe
handle_event(cast, ?PROBE(Probe, L), ?synced, Data) ->
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;
handle_event(cast, ?PROBE(Probe, L), ?wait_mon(?RESP_INFO(_), _), Data) ->
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

handle_event(_Kind, _Msg, _State, _Data) ->
    {keep_state_and_data, postpone}.

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


mon_of(Data, To) ->
    mon_reg:mon_of(Data#data.mon_register, To).

