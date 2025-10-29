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
    , deadlocked = false :: boolean()
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


%% Add waitee
handle_call({wait, Who}, _From, State) ->
    State1 = State#state{waitees = [Who | State#state.waitees]},
    Resp = 
        case State#state.probe of
            undefined -> ok;
            Probe ->
                case mon_reg:mon_of(State#state.mon_register, Who) of
                    undefined -> ok;
                    MonPid when is_pid(MonPid) ->
                        Worker = State#state.worker,
                        {send, [{MonPid, ?PROBE(Probe, [Worker])}]}
                end
        end,
    {reply, Resp, State1};

%% Remove waitee
handle_call({unwait, Who}, _From, State) ->
    State1 = State#state{waitees = lists:delete(Who, State#state.waitees)},
    {reply, ok, State1};

%% Set lock
handle_call({lock, Probe}, _From, State) ->
    State1 = State#state{probe = Probe},
    {reply, ok, State1};

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


handle_cast({subscribe, From}, State = #state{subscribers = DLS}) ->
    State1 = State#state{subscribers = [From | DLS]},
    {noreply, State1};

%% Deadlock propagation while deadlocked --- ignore
handle_cast(?DEADLOCK_PROP(_DL), #state{deadlocked = true} = State) ->
    {noreply, State};

%% Deadlock propagation --- propagate and become deadlocked
handle_cast(?DEADLOCK_PROP(DL), State) ->
    State1 = report_deadlock(DL, State),
    {noreply, State1};

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
    State1 = State#state{deadlocked = true},
    
    State1.
