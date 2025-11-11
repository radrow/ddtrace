-module(ddtrace_state).
-behaviour(gen_server).

-include("ddtrace.hrl").
-export([start_link/2, stop/1, init/1, handle_call/3, handle_cast/2]).

-type process_name() ::
        pid()
      | atom()
      | {global, term()}
      | {via, module(), term()}.

-record(state,
    { worker             :: process_name()
    , mon_register       :: process_name()
    , probe              :: gen_server:request_id() | undefined
    , waitees            :: [process_name()] % callers waiting on us
    , reqid_map = #{}
    , subscribers = []   :: [gen_statem:from()]
    , deadlocked = false :: {true, [process_name()]} | false
    }).


%% API

start_link(Worker, MonRegister) ->
    gen_server:start_link(?MODULE, [Worker, MonRegister], []).


stop(Pid) ->
    gen_server:call(Pid, stop).


%% gen_server callbacks

init([Worker, MonRegister]) ->
    State = #state{
               worker = Worker,
               mon_register = MonRegister,
               probe = undefined,
               waitees = []
              },
    {ok, State}.

%% Add waitee (deadlocked)
handle_call({wait, Who, ReqId}, _From, State = #state{deadlocked = {true, DL}}) ->
    State1 = add_waitee(Who, ReqId, State),
    Resp = case mon_reg:mon_of(State#state.mon_register, Who) of
               undefined -> ok;
               MonPid when is_pid(MonPid) ->
                   {send, [{MonPid, ?DEADLOCK_PROP(DL)}]}
           end,
    {reply, Resp, State1};

%% Add waitee (unlocked)
handle_call({wait, Who, ReqId}, _From, State = #state{probe = undefined}) ->
    State1 = add_waitee(Who, ReqId, State),
    {reply, ok, State1};

%% Add waitee (locked)
handle_call({wait, Who, ReqId}, _From, State = #state{probe = Probe}) ->
    State1 = add_waitee(Who, ReqId, State),
    Resp = case mon_reg:mon_of(State#state.mon_register, Who) of
               undefined -> ok;
               MonPid when is_pid(MonPid) ->
                   Worker = State#state.worker,
                   {send, [{MonPid, ?PROBE(Probe, [Worker])}]}
           end,
    {reply, Resp, State1};

%% Remove waitee while deadlocked --- error
handle_call({unwait, _}, _From, #state{deadlocked = {true, _}}) ->
    error(unwait_deadlocked);

%% Remove waitee
handle_call({unwait, WhoId}, _From, State) ->
    State1 = remove_waitee(WhoId, State),
    {reply, ok, State1};

%% Lock while already locked --- error
handle_call({lock, _}, _From, #state{probe = Probe}) when Probe =/= undefined ->
    error(already_locked);

%% Set lock
handle_call({lock, Probe}, _From, State) ->
    State1 = State#state{probe = Probe},
    {reply, ok, State1};

%% Unlock while not locked --- error
handle_call(unlock, _From, #state{probe = undefined}) ->
    error(unlock_not_locked);

%% Unlock while deadlocked --- error
handle_call(unlock, _From, #state{deadlocked = {true, _}}) ->
    error(unlock_deadlocked);

%% Unlock
handle_call(unlock, _From, State) ->
    State1 = State#state{probe = undefined},
    {reply, ok, State1};

%% Probe while not locked --- ignore
handle_call(?PROBE(_Probe, _L), _From, State = #state{probe = undefined}) ->
    {reply, ok, State};

%% Own probe returned --- deadlock
handle_call(?PROBE(Probe, DL), _From, State = #state{probe = Probe}) ->
    Worker = State#state.worker,
    {DlProp, State1} = report_deadlock([Worker|DL], State),
    {reply, {send, DlProp}, State1};

%% Foreign probe --- propagate
handle_call(?PROBE(Probe, L), _From, State) ->
    Worker = State#state.worker,
    Waits = State#state.waitees,
    Mons = [ mon_reg:mon_of(State#state.mon_register, Who) || Who <- Waits ],
    Sends = [ {Mon, ?PROBE(Probe, [Worker|L])} || Mon <- Mons ],
    Resp = case Sends of [] -> ok; _ -> {send, Sends} end,
    {reply, Resp, State};

%% Deadlock propagation --- not even locked 
handle_call(?DEADLOCK_PROP(DL), _From, #state{probe = undefined}) ->
    error({deadlock_not_locked, DL});

%% Deadlock propagation --- propagate and become deadlocked
handle_call(?DEADLOCK_PROP(DL), _From, State = #state{deadlocked = false}) ->
    {DlProp, State1} = report_deadlock(DL, State),
    {reply, {send, DlProp}, State1};

%% Deadlock propagation while deadlocked --- ignore
handle_call(?DEADLOCK_PROP(_DL), _From, State) ->
    {reply, ok, State}.


%% Deadlock subscription while deadlocked --- reply immediately
handle_cast({subscribe, From}, State = #state{deadlocked = {true, DL}}) ->
    gen_statem:reply(From, {deadlock, DL}),
    {noreply, State};

%% Deadlock subscription --- add subscriber
handle_cast({subscribe, From}, State = #state{deadlocked = false}) ->
    {noreply, State #state{subscribers = [From | State#state.subscribers]}};

handle_cast({unsubscribe, From}, State = #state{subscribers = Subs}) ->
    NewSubs = lists:delete(From, Subs),
    {noreply, State #state{subscribers = NewSubs}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Private functions

get_waitee(Who, State = #state{reqid_map = WaitsRev}) when is_reference(Who) ->
    case maps:find(Who, WaitsRev) of
        {ok, WhoPid} -> get_waitee(WhoPid, State);
        _ -> undefined
    end;
get_waitee(Who, #state{waitees = Waits}) when is_pid(Who) ->
    case lists:member(Who, Waits) of
        true -> {ok, Who};
        _ -> undefined
    end.

add_waitee(Who, ReqId, State = #state{waitees = Waits, reqid_map = ReqMap}) when is_pid(Who) ->
    case get_waitee(Who, State) of
        {ok, _} -> error({already_waiting, Who});
        _ -> ok
    end,
    State#state{
      waitees = [Who|Waits],
      reqid_map = ReqMap#{ReqId=>Who}
     }.

remove_waitee(Who, State = #state{waitees = Waits, reqid_map = ReqMap}) ->
    case get_waitee(Who, State) of
        undefined -> error({not_waiting, Who});
        {ok, WhoPid} ->
            NewWaits = lists:delete(WhoPid, Waits),
            NewReqMap = maps:filter(fun(_K, V) -> V =/= WhoPid end, ReqMap),
            State#state{waitees = NewWaits, reqid_map = NewReqMap}
    end.
                                              
            
report_deadlock(DL, State) ->
    %% Notify waitees
    Sends = [ begin
                  Mon = mon_reg:mon_of(State#state.mon_register, Pid),
                  {Mon, ?DEADLOCK_PROP(DL)}
              end
              || Pid <- State#state.waitees
            ],

    %% Notify subscribers
    [ begin
          gen_statem:reply(From, {deadlock, DL})
      end
     || From <- State#state.subscribers
    ],
    
    %% Set deadlocked flag. Clear subscribers (so they are notified only once).
    State1 = State#state{
               deadlocked = {true, DL},          
               subscribers = []
              },
    
    {Sends, State1}.
