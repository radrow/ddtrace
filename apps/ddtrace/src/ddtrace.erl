-module(ddtrace).
-behaviour(gen_statem).

-include("ddtrace.hrl").

%% API
-export([ start/2, start/3, start/4
        , start_link/2, start_link/3, start_link/4
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
%%% Helper Functions
%%%======================

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

%% @doc Normalize a worker identity - prefer global names over PIDs
%% This is used to convert PIDs from trace events to their registered names
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

%%%======================
%%% gen_statem Callbacks
%%%======================

init({Worker, MonRegister, Opts}) ->
    process_flag(priority, low),
    process_flag(trap_exit, true),

    TracerMod = proplists:get_value(tracer_mod, Opts, srpc_tracer),
    StateMod = proplists:get_value(state_mod, Opts, ddtrace_detector),
    
    %% Resolve worker to PID for Erlang monitoring and tracing
    WorkerPid = resolve_to_pid(Worker),
    ErlMon = erlang:monitor(process, WorkerPid),

    %% Register monitor under BOTH the original name and the PID
    %% - Original name (global name): for application-level lookups
    %% - PID: for trace-level lookups (trace events contain PIDs)
    mon_reg:set_mon(MonRegister, Worker, self()),
    case Worker of
        WorkerPid -> ok;  % Already registered (Worker was a PID)
        _ -> mon_reg:set_mon(MonRegister, WorkerPid, self())  % Also register by PID
    end,
    
    %% Detector only needs Worker name, tracer needs both Worker name and PID
    {ok, MonState} = StateMod:start_link(Worker, MonRegister),
    {ok, Tracer} = TracerMod:start_link(Worker, WorkerPid, MonRegister),

    Data = #data{ worker = Worker
                , worker_pid = WorkerPid
                , erl_monitor = ErlMon
                , mon_register = MonRegister
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
handle_event(enter, OldState, ?synced, Data) ->
    logger:debug("[~p@~p] STATE ENTER: ~p -> synced", [Data#data.worker, node(), OldState], #{module => ?MODULE}),
    keep_state_and_data;
handle_event(enter, OldState, ?wait_mon(MsgInfo), Data) ->
    logger:debug("[~p@~p] STATE ENTER: ~p -> wait_mon(~p)", [Data#data.worker, node(), OldState, MsgInfo], #{module => ?MODULE}),
    TimeoutAction = {state_timeout, 1000, synchronisation},
    {keep_state_and_data, [TimeoutAction]};
handle_event(enter, OldState, NewState, Data) ->
    logger:debug("[~p@~p] STATE ENTER: ~p -> ~p", [Data#data.worker, node(), OldState, NewState], #{module => ?MODULE}),
    deliver_traces(Data),
    keep_state_and_data;

handle_event(state_timeout, synchronisation, ?wait_mon(MsgInfo), Data) ->
    Worker = Data#data.worker,
    logger:warning("[~p@~p] TIMEOUT wait_mon: MsgInfo=~w", [Worker, node(), MsgInfo], #{module => ?MODULE, subsystem => ddtrace}),
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
    MonRegister = Data#data.mon_register,
    mon_reg:unset_mon(MonRegister, Worker),
    case Worker of
        WorkerPid -> ok;  % Same key, already unregistered
        _ -> mon_reg:unset_mon(MonRegister, WorkerPid)
    end,
    
    Tracer = Data#data.tracer,
    gen_statem:call(Tracer, stop),
    {keep_state_and_data, {reply, From, ok}};

%% The worker has attempted a call to itself. When this happens, no actual
%% message is sent. We fake the call message to "detect" the deadlock.
handle_event(info, {'DOWN', _ErlMon, process, Pid, {calling_self, Reason}}, State, Data = #data{worker = Pid}) ->
    logger:warning("[~p@~p] SELF-LOOP DEADLOCK in ~p: ~p", [Data#data.worker, node(), State, Reason]),
    handle_recv(Data#data.worker, ?QUERY_INFO(make_ref()), Data),
    keep_state_and_data;
%% The worker process has died.
handle_event(info, {'DOWN', ErlMon, process, Pid, Reason}, State, Data = #data{worker = Pid}) ->
    case is_self_loop(Reason) of
        true ->
            logger:warning("[~p@~p] WORKER DIED (self-loop) in ~p: ~p", [Data#data.worker, node(), State, Reason]),
            %% It was because it depended on someone who make a call to itself.
            handle_recv(Data#data.worker, ?QUERY_INFO(make_ref()), Data),
            keep_state_and_data;
        false ->
            logger:info("[~p@~p] WORKER DIED (normal) in ~p: ~p", [Data#data.worker, node(), State, Reason]),
            %% Normal termination: clean up and stop.
            erlang:demonitor(ErlMon, [flush]),
            {stop, normal, Data}
    end;

%%%======================
%%% handle_event: Deadlock propagation
%%%======================

handle_event(cast, ?DEADLOCK_PROP(DL), State, Data) ->
    logger:warning("[~p@~p] DEADLOCK DETECTED in ~p: ~p", [Data#data.worker, node(), State, DL]),
    state_deadlock(DL, Data),
    keep_state_and_data;

%%%======================
%%% handle_event: Monitor operation
%%%======================

%%%======================
%% Send trace

%% Handle send trace in synced state
handle_event(cast, ?SEND_INFO(To, MsgInfo), ?synced, Data) ->
    logger:debug("[~p@~p] SEND in synced: To=~p MsgInfo=~p", [Data#data.worker, node(), To, MsgInfo]),
    Data1 = handle_send(To, MsgInfo, Data),
    send_herald(To, MsgInfo, Data),
    {keep_state, Data1};

%% Handle send trace while awaiting process trace
handle_event(cast, ?SEND_INFO(To, MsgInfo), ?wait_proc(From, ProcMsgInfo), Data) ->
    logger:debug("[~p@~p] SEND in wait_proc(~p, ~p): To=~p MsgInfo=~p", [Data#data.worker, node(), From, ProcMsgInfo, To, MsgInfo]),
    Data1 = handle_send(To, MsgInfo, Data),
    send_herald(To, MsgInfo, Data),
    {keep_state, Data1};

%% Awaiting herald: postpone (send events)
handle_event(cast, ?SEND_INFO(To, MsgInfo), State, Data) ->
    logger:debug("[~p@~p] SEND POSTPONED in ~p: To=~p MsgInfo=~p", [Data#data.worker, node(), State, To, MsgInfo]),
    {keep_state_and_data, postpone};

%%%======================
%% Receive trace

%% We were synced, so now we wait for monitor herald
handle_event(cast, ?RECV_INFO(MsgInfo), ?synced, Data) ->
    logger:debug("[~p@~p] RECV in synced: MsgInfo=~p -> wait_mon", [Data#data.worker, node(), MsgInfo]),
    {next_state, ?wait_mon(MsgInfo), Data};

%% Awaited process receive-trace
handle_event(cast, ?RECV_INFO(MsgInfo), ?wait_proc(From, MsgInfo), Data0) ->
    logger:debug("[~p@~p] RECV in wait_proc (MATCH): From=~p MsgInfo=~p -> synced", [Data0#data.worker, node(), From, MsgInfo]),
    Data1 = handle_recv(From, MsgInfo, Data0),
    {next_state, ?synced, Data1};

%% Unwanted process receive-trace. We wait for herald first, and then
%% resume waiting for the process trace.
handle_event(cast, ?RECV_INFO(MsgInfoNotif), ?wait_proc(From, MsgInfo), Data) when MsgInfoNotif =/= MsgInfo ->
    logger:debug("[~p@~p] RECV in wait_proc (MISMATCH): got ~p, waiting for ~p -> wait_mon_proc", [Data#data.worker, node(), MsgInfoNotif, MsgInfo]),
    {next_state, ?wait_mon_proc(MsgInfoNotif, From, MsgInfo), Data};

%% Awaiting herald: postpone
handle_event(cast, ?RECV_INFO(MsgInfo), State, Data) ->
    logger:debug("[~p@~p] RECV POSTPONED in ~p: MsgInfo=~p", [Data#data.worker, node(), State, MsgInfo]),
    {keep_state_and_data, postpone};

%%%======================
%% Monitor herald
    
%% We were synced, so now we wait for process trace
handle_event(cast, ?HERALD(From, MsgInfo), ?synced, Data) ->
    logger:debug("[~p@~p] HERALD in synced: From=~p MsgInfo=~p -> wait_proc", [Data#data.worker, node(), From, MsgInfo]),
    {next_state, ?wait_proc(From, MsgInfo), Data};

%% Awaited herald
handle_event(cast, ?HERALD(From, MsgInfo), ?wait_mon(MsgInfo), Data0) ->
    logger:debug("[~p@~p] HERALD MATCH in wait_mon: From=~p MsgInfo=~p -> synced", [Data0#data.worker, node(), From, MsgInfo]),
    Data1 = handle_recv(From, MsgInfo, Data0),
    {next_state, ?synced, Data1};

handle_event(cast, ?HERALD(From, MsgInfo), ?wait_mon_proc(MsgInfo, FromProc, MsgInfoProc), Data0) ->
    logger:debug("[~p@~p] HERALD MATCH in wait_mon_proc: From=~p MsgInfo=~p -> wait_proc(~p, ~p)", [Data0#data.worker, node(), From, MsgInfo, FromProc, MsgInfoProc]),
    Data1 = handle_recv(From, MsgInfo, Data0),
    {next_state, ?wait_proc(FromProc, MsgInfoProc), Data1};

%% Unwanted herald: postpone
handle_event(cast, ?HERALD(From, MsgInfoOther), State, Data) ->
    logger:debug("[~p@~p] HERALD POSTPONED in ~p: From=~p MsgInfo=~p", [Data#data.worker, node(), State, From, MsgInfoOther]),
    {keep_state_and_data, postpone};

%%%======================
%% Probe

%% Handle probe in synced state
handle_event(cast, ?PROBE(Probe, L), ?synced, Data) ->
    logger:debug("[~p@~p] PROBE in synced: Probe=~p L=~p", [Data#data.worker, node(), Probe, L]),
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

%% Handle probe while awaiting monitor herald (since probes come from
%% monitors). TODO: filter to make sure the probe comes from the right monitor
%% only?
handle_event(cast, ?PROBE(Probe, L), ?wait_mon(?RESP_INFO(ReqId)), Data) ->
    logger:debug("[~p@~p] PROBE in wait_mon(response ~p): Probe=~p L=~p", [Data#data.worker, node(), ReqId, Probe, L]),
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

handle_event(cast, ?PROBE(Probe, L), ?wait_mon_proc(?RESP_INFO(ReqId), FromProc, MsgInfoProc), Data) ->
    logger:debug("[~p@~p] PROBE in wait_mon_proc(response ~p, ~p, ~p): Probe=~p L=~p", [Data#data.worker, node(), ReqId, FromProc, MsgInfoProc, Probe, L]),
    call_mon_state(?PROBE(Probe, L), Data),
    keep_state_and_data;

%% Unwanted probe: postpone
handle_event(cast, ?PROBE(Probe, L), State, Data) ->
    logger:debug("[~p@~p] PROBE POSTPONED in ~p: Probe=~p L=~p", [Data#data.worker, node(), State, Probe, L]),
    {keep_state_and_data, postpone};

%%%======================
%% Edge cases

%% We are somehow non-exhaustive or someone's pranked us
handle_event(_Kind, _Msg, _State, _Data) ->
    error({unexpected_event, _Kind, _Msg, _State}).

%%%======================
%%% Monitor user API
%%%======================

%% Stops tracing for the monitored process. This does not terminate the tracing
%% process itself, just stops listening to subsequent events.
stop_tracer(Mon) ->
    gen_statem:call(Mon, stop_tracer).

%% Sends a gen_statem request which will be replied when a deadlock is detected.
%% Useful for simultaneous waiting for either a response from the gen_server or a
%% deadlock.
subscribe_deadlocks(Mon) ->
    gen_statem:send_request(Mon, subscribe).

%% Sends a gen_statem request to abandon a deadlock subscription. Once
%% processed, the previous subscription will not be replied to.
unsubscribe_deadlocks(Mon) ->
    gen_statem:send_request(Mon, unsubscribe).

%%%======================
%%% Internal Helper Functions
%%%======================

%% Handle receive trace.
handle_recv(From, ?QUERY_INFO([alias|ReqId]), Data) ->
    NormalizedFrom = normalize_worker(From, Data),
    state_wait(NormalizedFrom, ReqId, Data);
handle_recv(From, ?QUERY_INFO(ReqId), Data) ->
    NormalizedFrom = normalize_worker(From, Data),
    state_wait(NormalizedFrom, ReqId, Data);
handle_recv(_From, ?RESP_INFO(_ReqId), Data) ->
    state_unlock(Data).

%% Handle send trace.
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
    logger:warning("[~p@~p] state_deadlock: propagating deadlock ~p", [Data#data.worker, node(), DL]),
    call_mon_state(?DEADLOCK_PROP(DL), Data).

%% Send monitor herald to another monitor. The [To] should refer to the
%% worker process, not the monitor directly. If [To] is not monitored, the
%% function does nothing.
send_herald(To, MsgInfo, Data) ->
    Mon = mon_of(Data, To),
    case Mon of
        undefined -> 
            logger:debug("[~p@~p] send_herald SKIP: To=~p (no monitor found)", [Data#data.worker, node(), To]),
            ok;
        _ ->
            Worker = Data#data.worker,
            Msg = ?HERALD(Worker, MsgInfo),
            logger:debug("[~p@~p] send_herald: To=~p Mon=~p MsgInfo=~p", [Data#data.worker, node(), To, Mon, MsgInfo]),
            gen_statem:cast(Mon, Msg),
            ok
    end.


%% Send a call message to the monitor state process and handle the response.
call_mon_state(Msg, Data = #data{mon_state = Pid}) ->
    Resp = gen_server:call(Pid, Msg),
    handle_mon_state_response(Resp, Data),
    Data.


%% Send a cast message to the monitor state process.
cast_mon_state(Msg, #data{mon_state = Pid}) ->
    gen_server:cast(Pid, Msg).


%% Handle reponse of the monitoring algorithm. Execute all scheduled sends.
handle_mon_state_response(ok, Data) ->
    logger:debug("[~p@~p] mon_state response: ok", [Data#data.worker, node()]),
    ok;
handle_mon_state_response({send, Sends}, Data) ->
    logger:debug("[~p@~p] mon_state response: send ~p messages", [Data#data.worker, node(), length(Sends)]),
    [ begin
          case Msg of
              ?PROBE(Probe, L) ->
                  logger:debug("[~p@~p]   -> SEND PROBE ~p to ~p with L=~p", [Data#data.worker, node(), Probe, ToPid, L]);
              ?DEADLOCK_PROP(DL) ->
                  logger:debug("[~p@~p]   -> SEND DEADLOCK_PROP to ~p: ~p", [Data#data.worker, node(), ToPid, DL]);
              _ ->
                  logger:debug("[~p@~p]   -> sending ~p to ~p", [Data#data.worker, node(), Msg, ToPid])
          end,
          gen_statem:cast(ToPid, Msg)
      end
      || {ToPid, Msg} <- Sends
    ],
    ok.

%% Make sure that all traces have been delivered before proceeding.
deliver_traces(Data) ->
    WorkerPid = Data#data.worker_pid,
    spawn(fun() ->
                  TRef = erlang:trace_delivered(WorkerPid),
                  receive {trace_delivered, WorkerPid, TRef} -> ok end
          end),
    ok.

%% Inspect the monitor of a process.
mon_of(Data, Pid) ->
    mon_reg:mon_of(Data#data.mon_register, Pid).

%% Check if shutdown reason was caused by a (possibly remote) deadlock caused by
%% a call to self.
is_self_loop({calling_self, _}) ->
    true;
is_self_loop({E, _}) ->
    is_self_loop(E);
is_self_loop(_) ->
    false.

