-module(ddmon_state).
-behaviour(gen_server).

-include("ddmon.hrl").
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
    , subscribers = []   :: [gen_statem:from()]
    , deadlocked = false :: {deadlocked, [process_name()]} | false
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


%% Add waitee (unlocked)
handle_call({wait, Who}, _From, State = #state{probe = undefined, waitees = Waits}) ->
    State1 = State#state{waitees = [Who | Waits]},
    {reply, ok, State1};

%% Add waitee (locked)
handle_call({wait, Who}, _From, State = #state{probe = Probe, waitees = Waits}) ->
    State1 = State#state{waitees = [Who | Waits]},

    Resp = case mon_reg:mon_of(State#state.mon_register, Who) of
               undefined -> ok;
               MonPid when is_pid(MonPid) ->
                   Worker = State#state.worker,
                   {send, [{MonPid, ?PROBE(Probe, [Worker])}]}
           end,
    {reply, Resp, State1};

%% Remove waitee
handle_call({unwait, Who}, _From, State) ->
    State1 = State#state{waitees = lists:delete(Who, State#state.waitees)},
    {reply, ok, State1};

%% Lock while already locked --- error
handle_call({lock, _}, _From, #state{probe = Probe}) when Probe =/= undefined ->
    throw({badarg, already_locked});

%% Set lock
handle_call({lock, Probe}, _From, State) ->
    State1 = State#state{probe = Probe},
    {reply, ok, State1};

%% Unlock while not locked --- error
handle_call(unlock, _From, #state{probe = undefined}) ->
    throw({badarg, not_locked});

%% Unlock while deadlocked --- error
handle_call(unlock, _From, #state{deadlocked = {deadlocked, _}}) ->
    throw({badarg, deadlocked});

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
    State1 = report_deadlock([Worker|DL], State),
    {reply, deadlock, State1};

%% Foreign probe --- propagate
handle_call(?PROBE(Probe, L), _From, State) ->
    Worker = State#state.worker,
    Mons = [ mon_reg:mon_of(State#state.mon_register, Who) || Who <- State#state.waitees ],
    Sends = [ {Mon, ?PROBE(Probe, [Worker|L])} || Mon <- Mons ],
    Resp = case Sends of [] -> ok; _ -> {send, Sends} end,
    {reply, Resp, State}.


%% Deadlock subscription
handle_cast({subscribe, From}, State = #state{subscribers = DLS, deadlocked = DLed}) ->
    State1 = State#state{subscribers = [From | DLS]},
    case DLed of {deadlocked, DL} -> gen_statem:reply(From, {deadlock, DL}); false -> ok end,
    {noreply, State1};

%% Deadlock propagation --- propagate and become deadlocked
handle_cast(?DEADLOCK_PROP(DL), State = #state{deadlocked = false}) ->
    State1 = report_deadlock(DL, State),
    {noreply, State1};

%% Deadlock propagation while deadlocked --- ignore
handle_cast(?DEADLOCK_PROP(_DL), State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Private functions

report_deadlock(DL, State) ->
    %% Notify waitees
    [ begin
          Mon = mon_reg:mon_of(State#state.mon_register, Pid),
          gen_statem:cast(Mon, ?DEADLOCK_PROP(DL))
      end
      || Pid <- State#state.waitees
    ],

    %% Notify subscribers
    [ begin
          gen_statem:reply(From, {deadlock, DL})
      end
     || From <- State#state.subscribers
    ],
    
    %% Set deadlocked flag
    State1 = State#state{deadlocked = {deadlocked, DL}},
    
    State1.
