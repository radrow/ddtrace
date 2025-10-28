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
    , waitees             :: [gen_server:request_id()] % callers waiting on us
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


handle_call({wait, Who}, _From, State) ->
    State1 = State#state{waitees = [Who | State#state.waitees]},
    Resp = 
        case State#state.probe of
            undefined -> ok;
            Probe ->
                {send, [{via_mon(State, Who), {probe, Probe}}]}
        end,
    {reply, Resp, State1};

handle_call({unwait, Who}, _From, State) ->
    State1 = State#state{waitees = lists:delete(Who, State#state.waitees)},
    {reply, ok, State1};

handle_call({lock, Probe}, _From, State) ->
    State1 = State#state{probe = Probe},
    {reply, ok, State1};

handle_call(unlock, _From, State) ->
    State1 = State#state{probe = undefined},
    {reply, ok, State1};

handle_call({probe, _Probe}, _From, State = #state{probe = undefined}) ->
    {reply, ok, State};

handle_call({probe, Probe}, _From, State = #state{probe = Probe}) ->
    % Deadlock detected
    {reply, deadlock, State};

handle_call({probe, Probe}, _From, State) ->
    Sends = [ {via_mon(State, Who), {probe, Probe}}
              || Who <- State#state.waitees
            ],
    {reply, {send, Sends}, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


%% Private functions

via_mon(State, Pid) ->
    mon_reg:via(State#state.mon_register, Pid).
