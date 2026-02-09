-module(ddtrace).
-behaviour(gen_statem).

-include("ddtrace.hrl").

%% API
-export([ start/1, start/2, start/3
        , start_link/1, start_link/2, start_link/3
        ]).

%% gen_statem callbacks
-export([init/1, callback_mode/0]).
-export([terminate/2,terminate/3]).

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
    { worker               :: process_name() % the traced worker process (name/global/pid)
    , worker_pid           :: pid()          % the resolved PID for tracing
    , erl_monitor          :: reference()    % the Erlang monitor reference
    , mon_state            :: process_name() % the process holding the monitor state
    , tracer               :: process_name() % the srpc_tracer process
    }).


%%%======================
%%% API Functions
%%%======================

start(Worker) ->
    start(Worker, [], []).
start(Worker, Opts) ->
    start(Worker, Opts, []).
start(Worker, Opts, GenOpts) ->
    gen_statem:start(?MODULE, {Worker, Opts}, GenOpts).

start_link(Worker) ->
    start_link(Worker, [], []).
start_link(Worker, Opts) ->
    start_link(Worker, Opts, []).
start_link(Worker, Opts, GenOpts) ->
    gen_statem:start_link(?MODULE, {Worker, Opts}, GenOpts).

%%%======================
%%% gen_statem Callbacks
%%%======================

init({Worker, Opts}) ->
    process_flag(priority, low),
    process_flag(trap_exit, true),

    mon_reg:ensure_started(),

    TracerMod = proplists:get_value(tracer_mod, Opts, srpc_tracer),
    StateMod = proplists:get_value(state_mod, Opts, ddtrace_detector),
    
    %% Resolve worker to PID for Erlang monitoring and tracing
    WorkerPid = resolve_to_pid(Worker),
    ErlMon = erlang:monitor(process, WorkerPid),

    %% Register monitor under both the original name and the PID
    %% - Original name: for application-level lookups
    %% - PID: for trace-level lookups (trace events contain PIDs, not names)
    mon_reg:set_mon(Worker, self()),
    case Worker of
        WorkerPid -> ok;
        _ -> mon_reg:set_mon(WorkerPid, self())
    end,

    %% Start detector and tracer
    {ok, MonState} = StateMod:start_link(Worker),
    {ok, Tracer} = TracerMod:start_link(Worker, WorkerPid),

    Data = #data{ worker = Worker
                , worker_pid = WorkerPid
                , erl_monitor = ErlMon
                , mon_state = MonState
                , tracer = Tracer
                },

    {ok, ?synced, Data, []}.

callback_mode() ->
    %% We use `state_enter` to debug major state transitions via tracing. Leave
    %% it unless it causes performance issues.
    [handle_event_function, state_enter].

terminate(State, Data) ->
    terminate(shutdown, State, Data).
terminate(Reason, _State, Data) ->
    if Reason =:= normal; Reason =:= shutdown; element(1, Reason) =:= shutdown ->
            ok;
         true ->
            Worker = Data#data.worker,
            logger:error("~p: Monitored process ~p died abnormally: ~w", [self(), Worker, Reason], #{module => ?MODULE, subsystem => ddtrace})
    end,
    ErlMon = Data#data.erl_monitor,
    erlang:demonitor(ErlMon, [flush]),
    ok.

%%%======================
%%% handle_event: All-time interactions
%%%======================

%% Wait for the tracer to deliver all traces to quit non-synced state asap..
handle_event(enter, _OldState, ?synced, _Data) ->
    ?DDT_DBG_STATE("[~p@~p] ~p -> synced", [_Data#data.worker, node(), _OldState]),
    keep_state_and_data;
handle_event(enter, _OldState, ?wait_mon(_MsgInfo), _Data) ->
    ?DDT_DBG_STATE("[~p@~p] ~p -> wait_mon(~p)", [_Data#data.worker, node(), _OldState, _MsgInfo]),
    TimeoutAction = {state_timeout, 1000, synchronisation},
    {keep_state_and_data, [TimeoutAction]};
handle_event(enter, _OldState, _NewState, Data) ->
    ?DDT_DBG_STATE("[~p@~p] ~p -> ~p", [Data#data.worker, node(), _OldState, _NewState]),
    deliver_traces(Data),
    keep_state_and_data;

handle_event(state_timeout, synchronisation, ?wait_mon(MsgInfo), Data) ->
    Worker = Data#data.worker,
    logger:warning("~p: Waiting for herald for too long: ~w", [Worker, MsgInfo], #{module => ?MODULE, subsystem => ddtrace}),
    keep_state_and_data;
handle_event(state_timeout, synchronisation, ?wait_mon_proc(MsgInfo, _, _), Data) ->
    Worker = Data#data.worker,
    logger:warning("~p: Waiting for herald for too long: ~w", [Worker, MsgInfo], #{module => ?MODULE, subsystem => ddtrace}),
    keep_state_and_data;
handle_event(state_timeout, synchronisation, ?wait_proc(From, MsgInfo), Data) ->
    Worker = Data#data.worker,
    logger:warning("~p: Waiting for trace for too long: ~w / ~w", [Worker, From, MsgInfo], #{module => ?MODULE, subsystem => ddtrace}),
    keep_state_and_data;

handle_event({call, From}, subscribe, _State, Data) ->
    cast_mon_state({subscribe, From}, Data),
    keep_state_and_data;

handle_event({call, From}, unsubscribe, _State, Data) ->
    cast_mon_state({unsubscribe, From}, Data),
    keep_state_and_data;

handle_event({call, From}, stop_tracer, _State, Data) ->
    %% Unregister from mon_reg before stopping
    Worker = Data#data.worker,
    WorkerPid = Data#data.worker_pid,
    mon_reg:unset_mon(Worker),
    case Worker of
        WorkerPid -> ok;
        _ -> mon_reg:unset_mon(WorkerPid)
    end,

    Tracer = Data#data.tracer,
    gen_statem:call(Tracer, stop),
    {keep_state_and_data, {reply, From, ok}};

%% The worker has attempted a call to itself. When this happens, no actual
%% message is sent. We fake the call message to "detect" the deadlock.
handle_event(info, {'DOWN', _ErlMon, process, Pid, {calling_self, _Reason}}, _State, Data = #data{worker_pid = Pid}) ->
    handle_recv(Data#data.worker, ?QUERY_INFO(make_ref()), Data),
    keep_state_and_data;
%% The worker process has died.
handle_event(info, {'DOWN', ErlMon, process, Pid, Reason}, _State, Data = #data{worker_pid = Pid}) ->
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

handle_event(cast, ?DEADLOCK_PROP(DL), _State, Data) ->
    state_deadlock(DL, Data),
    keep_state_and_data;

%%%======================
%%% handle_event: Monitor operation
%%%======================

%%%======================
%% Send trace

%% Handle send trace in synced state
handle_event(cast, ?SEND_INFO(To, MsgInfo), ?synced, Data) ->
    ?DDT_DBG_STATE("[~p@~p] SEND synced: To=~p MsgInfo=~p", [Data#data.worker, node(), To, MsgInfo]),
    Data1 = handle_send(To, MsgInfo, Data),
    send_herald(To, MsgInfo, Data),
    {keep_state, Data1};

%% Handle send trace while awaiting process trace
handle_event(cast, ?SEND_INFO(To, MsgInfo), ?wait_proc(_From, _ProcMsgInfo), Data) ->
    ?DDT_DBG_STATE("[~p@~p] SEND wait_proc: To=~p MsgInfo=~p", [Data#data.worker, node(), To, MsgInfo]),
    Data1 = handle_send(To, MsgInfo, Data),
    send_herald(To, MsgInfo, Data),
    {keep_state, Data1};

%% Awaiting herald: postpone
handle_event(cast, ?SEND_INFO(_To, _MsgInfo), _State, _Data) ->
    {keep_state_and_data, postpone};

%%%======================
%% Receive trace

%% We were synced, so now we wait for monitor herald
handle_event(cast, ?RECV_INFO(MsgInfo), ?synced, _Data) ->
    ?DDT_DBG_LOCK("~p: Waiting for lock (herald) for ~p (synced -> wait_mon)", [_Data#data.worker, MsgInfo]),
    {next_state, ?wait_mon(MsgInfo), _Data};

%% Awaited process receive-trace
handle_event(cast, ?RECV_INFO(MsgInfo), ?wait_proc(From, MsgInfo), Data0) ->
    ?DDT_DBG_LOCK("~p: Lock acquired - process received message ~p from ~p (wait_proc -> synced)", [Data0#data.worker, MsgInfo, From]),
    Data1 = handle_recv(From, MsgInfo, Data0),
    {next_state, ?synced, Data1};

%% Unwanted process receive-trace. We wait for herald first, and then
%% resume waiting for the process trace.
handle_event(cast, ?RECV_INFO(MsgInfoNotif), ?wait_proc(From, MsgInfo), Data) when MsgInfoNotif =/= MsgInfo ->
    {next_state, ?wait_mon_proc(MsgInfoNotif, From, MsgInfo), Data};

%% Awaiting herald: postpone
handle_event(cast, ?RECV_INFO(_MsgInfo), _State, _Data) ->
    {keep_state_and_data, postpone};

%%%======================
%% Monitor herald
    
%% We were synced, so now we wait for process trace
handle_event(cast, ?HERALD(From, MsgInfo), ?synced, _Data) ->
    ?DDT_DBG_HERALD("~p: Herald from ~p for ~p (synced -> wait_proc)", [_Data#data.worker, From, MsgInfo]),
    {next_state, ?wait_proc(From, MsgInfo), _Data};

%% Awaited herald
handle_event(cast, ?HERALD(From, MsgInfo), ?wait_mon(MsgInfo), Data0) ->
    ?DDT_DBG_HERALD("~p: Awaited herald from ~p for ~p (wait_mon -> synced)", [Data0#data.worker, From, MsgInfo]),
    Data1 = handle_recv(From, MsgInfo, Data0),
    {next_state, ?synced, Data1};

handle_event(cast, ?HERALD(From, MsgInfo), ?wait_mon_proc(MsgInfo, FromProc, MsgInfoProc), Data0) ->
    Data1 = handle_recv(From, MsgInfo, Data0),
    {next_state, ?wait_proc(FromProc, MsgInfoProc), Data1};

%% Unwanted herald: postpone
handle_event(cast, ?HERALD(_From, _MsgInfoOther), _State, _Data) ->
    {keep_state_and_data, postpone};

%%%======================
%% Probe

%% Handle probe in synced state
handle_event(cast, ?PROBE(Probe, L), ?synced, Data) ->
    ?DDT_DBG_PROBE("~p: Received probe ~p with path ~p in synced state", [Data#data.worker, Probe, L]),
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

%% Handle probe while awaiting monitor herald (since probes come from monitors).
%% TODO: filter to make sure the probe comes from the right monitor only?
handle_event(cast, ?PROBE(Probe, L), ?wait_mon(?RESP_INFO(_ReqId)), Data) ->
    ?DDT_DBG_PROBE("~p: Received probe ~p with path ~p while awaiting monitor", [Data#data.worker, Probe, L]),
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

handle_event(cast, ?PROBE(Probe, L), ?wait_mon_proc(?RESP_INFO(_ReqId), _FromProc, _MsgInfoProc), Data) ->
    ?DDT_DBG_PROBE("~p: Received probe ~p with path ~p while awaiting monitor proc", [Data#data.worker, Probe, L]),
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

%% Unwanted probe: postpone
handle_event(cast, ?PROBE(_Probe, _L), _State, _Data) ->
    ?DDT_DBG_STATE("~p: Postponing probe ~p with path ~p in state ~p", [_Data#data.worker, _Probe, _L, _State]),
    {keep_state_and_data, postpone};

%%%======================
%% Edge cases

%% We are somehow non-exhaustive or someone's pranked us
handle_event(_Kind, _Msg, _State, _Data) ->
    error({unexpected_event, _Kind, _Msg, _State}).

%%%======================
%%% Monitor user API
%%%======================

%% @doc Stops tracing for the monitored process. This does not terminate the
%% tracing process itself, just stops listening to subsequent events.
stop_tracer(Mon) ->
    gen_statem:call(Mon, stop_tracer).

%% @doc Sends a `gen_statem` request which will be replied when a deadlock is
%% detected. Useful for simultaneous waiting for either a response from the
%% gen_server or a deadlock.
subscribe_deadlocks(Mon) ->
    gen_statem:send_request(Mon, subscribe).

%% @doc Sends a gen_statem request to abandon a deadlock subscription. Once
%% processed, the previous subscription will not be replied to.
unsubscribe_deadlocks(Mon) ->
    gen_statem:send_request(Mon, unsubscribe).

%%%======================
%%% Internal Helper Functions
%%%======================

%% @doc Handle receive trace.
handle_recv(From, ?QUERY_INFO([alias|ReqId]), Data) ->
    NormalizedFrom = normalize_worker(From, Data),
    state_wait(NormalizedFrom, ReqId, Data);
handle_recv(From, ?QUERY_INFO(ReqId), Data) ->
    NormalizedFrom = normalize_worker(From, Data),
    state_wait(NormalizedFrom, ReqId, Data);
handle_recv(_From, ?RESP_INFO(_ReqId), Data) ->
    state_unlock(Data).

%% @doc Handle send trace.
handle_send(_To, ?QUERY_INFO(ReqId), Data) ->
    state_lock(ReqId, Data);
handle_send(To, ?RESP_INFO(_ReqId), Data) ->
    NormalizedTo = normalize_worker(To, Data),
    state_unwait(NormalizedTo, Data).

%% @doc Register a client
state_wait(Who, ReqId, Data) ->
    call_mon_state({wait, Who, ReqId}, Data).

%% @doc Unregister a client
state_unwait(Who, Data) ->
    call_mon_state({unwait, Who}, Data).

%% @doc Register unlocking
state_unlock(Data) ->
    call_mon_state(unlock, Data).
    
%% @doc Register locking
state_lock(ReqId, Data) ->
    call_mon_state({lock, ReqId}, Data).

%% @doc Register a deadlock
state_deadlock(DL, Data) ->
    call_mon_state(?DEADLOCK_PROP(DL), Data).


%% @doc Send monitor herald to another monitor. The [To] should refer to the
%% worker process, not the monitor directly. If [To] is not monitored, the
%% function does nothing.
send_herald(To, MsgInfo, Data) ->
    NormalizedTo = normalize_worker(To, Data),
    Mon = mon_of(Data, NormalizedTo),
    case Mon of
        undefined -> ok;
        _ ->
            Worker = Data#data.worker,
            Msg = ?HERALD(Worker, MsgInfo),
            gen_statem:cast(Mon, Msg),
            ok
    end.


%% @doc Send a call message to the monitor state process and handle the
%% response.
call_mon_state(Msg, Data = #data{mon_state = Pid}) ->
    Resp = gen_server:call(Pid, Msg),
    handle_mon_state_response(Resp, Data),
    Data.


%% Send a cast message to the monitor state process.
cast_mon_state(Msg, #data{mon_state = Pid}) ->
    gen_server:cast(Pid, Msg).


%% @doc Handle reponse of the monitoring algorithm. Execute all scheduled sends.
handle_mon_state_response(ok, _Data) ->
    ok;
handle_mon_state_response({send, Sends}, _Data) ->
    [ gen_statem:cast(ToPid, Msg) || {ToPid, Msg} <- Sends ],
    ok.


%% @doc Make sure that all traces have been delivered before proceeding.
deliver_traces(Data) ->
    WorkerPid = Data#data.worker_pid,
    spawn(fun() ->
                  TRef = erlang:trace_delivered(WorkerPid),
                  receive {trace_delivered, WorkerPid, TRef} -> ok end
          end),
    ok.


%% @doc Inspect the monitor of a process.
mon_of(_Data, Pid) ->
    mon_reg:mon_of(Pid).


%% @doc Check if shutdown reason was caused by a (possibly remote) deadlock
%% caused by a call to self.
is_self_loop({calling_self, _}) ->
    true;
is_self_loop({E, _}) ->
    is_self_loop(E);
is_self_loop(_) ->
    false.

 
%% @doc Resolve a process name to a PID for Erlang monitoring
resolve_to_pid(Pid) when is_pid(Pid) -> Pid;
resolve_to_pid({global, Name}) ->
    case global:whereis_name(Name) of
        undefined -> exit({noproc, {global, Name}});
        Pid -> Pid
    end;
resolve_to_pid({via, Mod, Name}) ->
    case Mod:whereis_name(Name) of
        undefined -> exit({noproc, {via, Mod, Name}});
        Pid -> Pid
    end;
resolve_to_pid(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> exit({noproc, Name});
        Pid -> Pid
    end.


%% @doc Normalize a worker identity - prefer global names over PIDs This is used
%% to convert PIDs from trace events to their registered names
normalize_worker(Worker, _Data) when is_pid(Worker) ->
    %% Check if this PID has a corresponding global name in mon_reg
    %% by looking through registered names
    case find_registered_name(Worker) of
        undefined -> Worker;  % No global name, use PID
        Name -> Name          % Use global name
    end;
normalize_worker(Worker, _Data) ->
    Worker.  % Already a name, keep it


%% @doc Find a global name registered for a PID
find_registered_name(Pid) ->
    %% Scan global names to find one that resolves to this PID
    Names = global:registered_names(),
    case lists:filter(fun(Name) -> 
        global:whereis_name(Name) =:= Pid 
    end, Names) of
        [Name | _] -> {global, Name};
        [] -> undefined
    end.
