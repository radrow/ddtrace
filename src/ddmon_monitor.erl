-module(ddmon_monitor).
-behaviour(gen_server).

-include("ddmon.hrl").
-export([start_link/1, stop/1, init/1, handle_call/3, handle_cast/2]).

-type process_name() ::
        pid()
      | atom()
      | {global, term()}
      | {via, module(), term()}.

-record(state,
    { mon_register        :: process_name()
    , probe               :: gen_server:request_id() | undefined
    , waitees             :: [process_name()] % callers waiting on us
    }).

-type state() :: #state{}.


%% API

start_link(MonRegister) ->
    gen_server:start_link(?MODULE, MonRegister, []).


stop(Pid) ->
    gen_server:call(Pid, stop).


%% gen_server callbacks

init(MonRegister) ->
    State = #state{
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
                    MonPid when is_pid(MonPid) -> {send, [{MonPid, {probe, Probe}}]}
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
handle_call({probe, _Probe}, _From, State = #state{probe = undefined}) ->
    {reply, ok, State};

%% Own probe returned --- deadlock
handle_call({probe, Probe}, _From, State = #state{probe = Probe}) ->
    {reply, deadlock, State};

%% Foreign probe --- propagate
handle_call({probe, Probe}, _From, State) ->
    Sends0 = [ mon_reg:mon_of(State#state.mon_register, Who) || Who <- State#state.waitees ],
    Sends = [ {MonPid, {probe, Probe}} || MonPid <- Sends0, is_pid(MonPid) ],
    Resp = case Sends of [] -> ok; _ -> {send, Sends} end,
    {reply, Resp, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

%% Private functions
