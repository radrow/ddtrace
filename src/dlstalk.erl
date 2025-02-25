-module(dlstalk).
-behaviour(gen_statem).

-include("dlstalk.hrl").


%% API
-export([start/3, start_link/3]).

%% gen_server interface
-export([call/2, cast/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0]).

%% States
-export([unlocked/3, locked/3, deadlocked/3]).

%% Internal state queries
-export([ state_get_worker/1, state_get_req_tag/1, state_get_req_id/1, state_get_waitees/1
        , deadstate_get_worker/1, deadstate_get_deadlock/1
        ]).

%%%======================
%%% DlStalk Types
%%%======================

-record(state,{worker :: pid(),
               req_tag :: gen_server:reply_tag(),
               req_id :: gen_statem:request_id(),
               waitees :: list(pid())
              }).

state_get_worker(State) ->
    State#state.worker.

state_get_req_tag(State) ->
    State#state.req_tag.

state_get_req_id(State) ->
    State#state.req_id.

state_get_waitees(State) ->
    State#state.waitees.


-record(deadstate,{worker :: pid(),
                   deadlock :: list(pid())
                  }).

deadstate_get_worker(State) ->
    State#deadstate.worker.

deadstate_get_deadlock(State) ->
    State#deadstate.deadlock.

%%%======================
%%% API Functions
%%%======================

start(Module, Args, Options) ->
    gen_statem:start(?MODULE, {Module, Args, Options}, Options).

start_link(Module, Args, Options) ->
    gen_statem:start_link(?MODULE, {Module, Args, Options}, Options).


%%%======================
%%% gen_server interface
%%%======================

call(Server, Request) ->
    case get(?MON_PID) of
        undefined ->
            gen_server:call(Server, Request);
        Mon ->
            gen_statem:call(Mon, {Request, Server})
    end.

cast(Server, Message) ->
    gen_server:cast(Server, Message).


%%%======================
%%% gen_statem Callbacks
%%%======================

init({Module, Args, Options}) ->
    case gen_monitored:start_link(Module, Args, Options) of
        {ok, Pid} ->
            State =
                #state{worker = Pid,
                       waitees = gen_statem:reqids_new(),
                       req_tag = undefined,
                       req_id = undefined
                      },
            {ok, unlocked, State};
        E -> E
    end.

callback_mode() ->
    [state_functions, state_enter].


unlocked(enter, _, _) ->
    keep_state_and_data;

unlocked({call, From}, '$get_child', #state{worker = Worker}) ->
    {keep_state_and_data, {reply, From, Worker}};


%% Our service wants a call to itworker (either directly or the monitor)
unlocked({call, {Worker, _PTag}}, {_Msg, Server}, State = #state{worker = Worker})
  when Server =:= Worker orelse Server =:= self() ->
    {next_state, deadlocked,
     State
    };

%% Our service wants a call
unlocked({call, {Worker, PTag}}, {Msg, Server}, State = #state{worker = Worker}) ->
    %% Forward the request as `call` asynchronously
    ExtTag = gen_statem:send_request(Server, Msg),

    {next_state, locked,
     State#state{
       req_tag = PTag,
       req_id = ExtTag
      }
    };

%% Incoming external call
unlocked({call, From}, Msg, State = #state{waitees = Waitees0}) ->
    %% Forward to the process
    ReqId = gen_server:send_request(State#state.worker, Msg),

    %% Register the request
    Waitees1 = gen_statem:reqids_add(ReqId, From, Waitees0),

    {keep_state,
     State#state{waitees = Waitees1}
    };

%% Probe while unlocked --- ignore
unlocked(cast, {probe, _Probe}, _) ->
    keep_state_and_data;

%% Unknown cast
unlocked(cast, Msg, #state{worker = Worker}) ->
    gen_server:cast(Worker, Msg),
    keep_state_and_data;

%% Process sent a reply (or not)
unlocked(info, Msg, State = #state{waitees = Waitees0, worker = Worker}) ->
    case gen_statem:check_response(Msg, Waitees0, _Delete = true) of
        no_request ->
            %% Unknown info (waitees empty). Let the process handle it.
            Worker ! Msg,
            keep_state_and_data;

        no_reply ->
            %% Unknown info. Let the process handle it.
            worker ! Msg,
            keep_state_and_data;

        {{reply, Reply}, From, Waitees1} ->
            %% It's a reply from the process. Forward it.
            {keep_state,
             State#state{waitees = Waitees1},
             {reply, From, Reply}
            }
    end.


locked(enter, _, _) ->
    keep_state_and_data;

locked({call, From}, '$get_child', #state{worker = Worker}) ->
    {keep_state_and_data, {reply, From, Worker}};

%% Incoming external call
locked({call, From}, Msg, State = #state{req_tag = PTag, waitees = Waitees0}) ->
    %% Forward to the process
    ReqId = gen_server:send_request(State#state.worker, Msg),

    %% Register the request
    Waitees1 = gen_statem:reqids_add(ReqId, From, Waitees0),

    %% Send a probe
    gen_statem:cast(element(1, From), {?PROBE, PTag, [self()]}),

    {keep_state,
     State#state{waitees = Waitees1}
    };

%% Incoming reply
locked(info, Msg, State = #state{worker = Worker, req_tag = PTag, req_id = ReqId}) ->
    case gen_statem:check_response(Msg, ReqId) of
        no_reply ->
            %% Unknown info. Let the process handle it.
            worker ! Msg,
            keep_state_and_data;

        {reply, Reply} ->
            %% Pass the reply to the process. We are unlocked now.
            {next_state, unlocked,
             State,
             {reply, {Worker, PTag}, Reply}
            }
    end;

%% Incoming own probe. Alarm! Panic!
locked(cast, {?PROBE, PTag, Chain}, #state{worker = Worker, req_tag = PTag}) ->
    {next_state, deadlocked, #deadstate{worker = Worker, deadlock = [self() | Chain]}};

%% Incoming probe
locked(cast, {?PROBE, Probe, Chain}, #state{waitees = Waitees}) ->
    %% Propagate the probe to all waitees.
    [ begin
          gen_statem:cast(W, {?PROBE, Probe, [self()|Chain]})
      end
      || {_, {W, _}} <- gen_statem:reqids_to_list(Waitees)
    ],
    keep_state_and_data;

%% Unknown cast
locked(cast, Msg, #state{worker = Worker}) ->
    gen_server:cast(Worker, Msg),
    keep_state_and_data.


%% We are fffrankly in a bit of a trouble
deadlocked(enter, _OldState, _State) ->
    keep_state_and_data;

deadlocked({call, From}, '$get_child', #deadstate{worker = Worker}) ->
    {keep_state_and_data, {reply, From, Worker}};

deadlocked({call, _From}, _, _State) ->
    keep_state_and_data;

%% Probe
deadlocked(cast, {?PROBE, _, _}, _State) ->
    keep_state_and_data;

%% Unknown cast
deadlocked(cast, Msg, #deadstate{worker = Worker}) ->
    gen_server:cast(Worker, Msg),
    keep_state_and_data.
