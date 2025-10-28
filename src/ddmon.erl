%% Asynchronous deadlock detecting tracer for gen_server processes.
%%
%% Concept (classic probe based wait-for cycle detection):
%%  * Outgoing gen_server:call (trace of 'send' with {'$gen_call', ReqId, _}) means
%%    our worker now waits on the callee (edge: Self -> Callee). While waiting we
%%    are "locked". If we were previously unlocked we create a fresh probe whose
%%    identifier is that ReqId (root probe). We send this probe to the callee's
%%    tracer (indirectly via MonRegister).
%%  * Incoming gen_server:call (trace of 'receive' with {'$gen_call', ReqId, _})
%%    means the caller waits on us (edge: Caller -> Self). We store the ReqId in
%%    waitees. If we currently hold a probe (we are locked) we propagate our probe
%%    to the newcomer immediately so it can flow further along the graph.
%%  * Incoming reply to our outstanding call (trace 'receive' with {'$', ReqId, _})
%%    removes the wait (edge Self -> Callee gone). If we are no longer waiting on
%%    anyone, we drop our probe (root no longer needed). We keep waitees, since
%%    others may still wait on us.
%%  * Outgoing reply (trace 'send' with {'$', ReqId, _}) completes one caller's
%%    wait; we remove that ReqId from waitees.
%%  * Probe message ({trace, _, probe, ProbeId}) implements graph exploration.
%%    When a probe arrives:
%%      - If it equals our own probe id (we are root and our probe returned) a
%%        cycle exists => report deadlock to subscribers.
%%      - Otherwise, if we have not forwarded this probe before, we forward it to
%%        all currently waiting callers (waitees). We never store foreign probes
%%        (only maintain the root probe while locked) but remember their ids in
%%        seen_probes to avoid unlimited re-propagation.
%%  * While unlocked we do not hold a probe; we still forward any foreign probe
%%    to current waitees.
%%
%% Differences from the previous version:
%%  - Removed artificial synchronisation states (synced / wait_worker / wait_mon).
%%    As full consistency is not required for eventual cycle detection, we run in
%%    a single state function. This avoids losing probe messages while "unsynced".
%%  - Correct probe semantics: only one local probe (root) while locked; probes
%%    are not ignored in any phase; foreign probes are always forwarded exactly
%%    once per waitee set transition.
%%  - Added seen_probes set to suppress infinite flooding of identical probes.
%%
%% Limitations / Simplifications:
%%  - Only tracks a single outstanding outgoing call (typical for a gen_server
%%    doing a synchronous call). Nested outgoing calls would require a stack.
%%  - Relies on an external monitor register process (MonRegister) to route
%%    {trace, _, send, ToPid, {probe, ProbeId}} to the correct tracer.
%%
%% Deadlock notification: subscribers receive {?DEADLOCK, ProbeId} exactly once
%% when a cycle involving the root probe is detected.
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

%% Single state implementation
-export([state_running/3]).

%% DDMon API
-export([]).

%%%======================
%%% DDMon Types
%%%======================

-record(data,
    { worker              :: pid()
    , mon_register        :: pid()
    , probe               :: gen_server:request_id() | undefined
    , waiting_on          :: pid() | undefined       % callee we are currently blocked on
    , waitees             :: [gen_server:request_id()] % callers waiting on us
    , seen_probes         :: sets:set()              % foreign probe ids already forwarded
    , deadlock_subscribers :: [pid()]
    }).
-export_type([data/0]).
-type data() :: #data{}.

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

init({Worker, MonRegister, Opts}) ->
    process_flag(trap_exit, true),
    put(trace_int, proplists:get_value(trace_int, Opts, true)),
    put(live_log, proplists:get_value(live_log, Opts, false)),

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

    Data = #data{ worker = Worker
                , mon_register = MonRegister
                , probe = undefined
                , waiting_on = undefined
                , waitees = []
                , seen_probes = sets:new()
                , deadlock_subscribers = []
                },
    {ok, state_running, Data, []}.

callback_mode() ->
    state_functions.

terminate(Reason, _State, Data) ->
    terminate(Reason, Data).
terminate(_Reason, _Data) ->
    ok.

%%%======================
%%% State Function (single state)
%%%======================

state_running(cast, {subscribe, Pid}, Data=#data{deadlock_subscribers = Subs}) ->
    {keep_state, Data#data{deadlock_subscribers = lists:usort([Pid|Subs])}, []};
state_running(cast, {unsubscribe, Pid}, Data) ->
    {keep_state, Data#data{deadlock_subscribers = lists:delete(Pid, Data#data.deadlock_subscribers)}, []};
state_running(cast, _Other, _Data) ->
    keep_state_and_data;

%% Trace: outgoing messages from worker
state_running(info, {trace, _FromTracer, send, ToPid, Msg, _Ts}, Data0) ->
    case Msg of
        {'$gen_call', ReqId, _Call} ->
            %% We initiate (or extend) a wait.
            Probe0 = Data0#data.probe,
            NewProbe = case Probe0 of undefined -> ReqId; _ -> Probe0 end,
            NewData1 = Data0#data{probe = NewProbe, waiting_on = ToPid},
            %% If we just created a new probe (root), propagate to existing waitees.
            NewData2 = case {Probe0, NewProbe, Data0#data.waitees} of
                           {undefined, _Created, []} -> NewData1;
                           {undefined, _Created, Waitees} -> propagate_probe(Waitees, NewProbe, NewData1);
                           _ -> NewData1
                       end,
            send_probe(ToPid, NewProbe, NewData2),
            {keep_state, NewData2, []};
        {'$', ReplyReqId, _Reply} ->
            %% Outgoing reply -> caller no longer waits on us
            NewData = remove_waitee(ReplyReqId, Data0),
            {keep_state, NewData, []};
        _ ->
            keep_state_and_data
    end;

%% Trace: incoming messages to worker
state_running(info, {trace, _FromTracer, 'receive', _FromPid, Msg, _Ts}, Data0) ->
    case Msg of
        {'$gen_call', ReqId, _Call} ->
            %% A caller becomes waiting on us.
            NewData1 = add_waitee(ReqId, Data0),
            %% If we hold a probe (we are locked) propagate it to the new waitee immediately.
            NewData2 = case NewData1#data.probe of
                           undefined -> NewData1;
                           ProbeId -> propagate_probe([ReqId], ProbeId, NewData1)
                       end,
            {keep_state, NewData2, []};
        {'$', _IncomingReplyReqId, _Reply} ->
            %% Reply to our outstanding call -> we may unlock.
            NewData1 = case Data0#data.probe of
                           undefined -> Data0; % was not locked
                           _ -> Data0
                       end,
            %% Clear waiting_on when matching reply.
            NewData2 = case NewData1#data.waiting_on of
                           undefined -> NewData1;
                           _Pid -> NewData1#data{waiting_on = undefined}
                       end,
            %% If no longer waiting on anyone, drop probe.
            NewData3 = case NewData2#data.waiting_on of
                           undefined -> NewData2#data{probe = undefined};
                           _ -> NewData2
                       end,
            {keep_state, NewData3, []};
        _ ->
            keep_state_and_data
    end;

%% Probe propagation / detection
state_running(info, {trace, _FromTracer, probe, ProbeId}, Data0) ->
    case Data0#data.probe of
        ProbeId ->
            %% Our own probe returned -> cycle / deadlock.
            DeadlockMsg = {?DEADLOCK, ProbeId},
            lists:foreach(fun(P) -> P ! DeadlockMsg end, Data0#data.deadlock_subscribers),
            %% We stay locked but clear probe so we don't re-report.
            {keep_state, Data0#data{probe = undefined}, []};
        _Other ->
            %% Foreign probe: forward if not seen before.
            Seen = Data0#data.seen_probes,
            case sets:is_element(ProbeId, Seen) of
                true -> keep_state_and_data;
                false ->
                    Data1 = Data0#data{seen_probes = sets:add_element(ProbeId, Seen)},
                    Data2 = propagate_probe(Data1#data.waitees, ProbeId, Data1),
                    {keep_state, Data2, []}
            end
    end;

%% Worker termination
state_running(info, {'EXIT', Pid, _Reason}, Data=#data{worker = Pid}) ->
    {stop, normal, Data};

%% Ignore everything else
state_running(_Type, _Content, _Data) ->
    keep_state_and_data.

%%%======================
%%% Internal Helper Functions
%%%======================

add_waitee(ReqId, Data=#data{waitees = Ws}) ->
    case lists:member(ReqId, Ws) of
        true -> Data;
        false -> Data#data{waitees = [ReqId|Ws]}
    end.

remove_waitee(ReplyReqId, Data=#data{waitees = Ws}) ->
    Data#data{waitees = [ R || R <- Ws, R =/= ReplyReqId ]}.

send_probe(ToPid, ProbeId, #data{mon_register = MonReg}) ->
    MonReg ! {trace, self(), send, ToPid, {probe, ProbeId}},
    ok.

propagate_probe([], _ProbeId, Data) -> Data;
propagate_probe([ReqId|Rest], ProbeId, Data) ->
    {FromPid, _Tag} = ReqId,
    send_probe(FromPid, ProbeId, Data),
    propagate_probe(Rest, ProbeId, Data).
