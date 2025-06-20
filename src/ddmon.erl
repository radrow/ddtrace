-module(ddmon).
-behaviour(gen_statem).

-include("ddmon.hrl").

-define(PROBE_DELAY, '$ddmon_probe_delay').

%% API
-export([start/2, start/3, start_link/2, start_link/3]).

%% gen_server interface
-export([call/2, call/3, cast/2, stop/3]).

%% gen_statem callbacks
-export([init/1, callback_mode/0]).
-export([terminate/3, terminate/2]).

%% States
-export([unlocked/3, locked/3, deadlocked/3]).

%% Internal state queries
-export([ state_get_worker/1, state_get_req_tag/1, state_get_req_id/1, state_get_waitees/1
        , deadstate_get_worker/1, deadstate_get_deadlock/1, deadstate_is_foreign/1
        ]).

%%%======================
%%% DDMon Types
%%%======================

-record(state,{worker :: pid(),
               req_tag :: gen_server:reply_tag() | undefined,
               req_id :: gen_statem:request_id() | undefined,
               waitees :: gen_statem:request_id_collection()
              }).
-record(deadstate,{worker :: pid(),
                   deadlock :: list(pid()),
                   req_id :: gen_statem:request_id(),
                   foreign = false :: boolean()
                  }).

state_get_worker(#state{worker = Worker}) ->
    Worker;
state_get_worker(#deadstate{worker = Worker}) ->
    Worker.

state_get_req_tag(State) ->
    State#state.req_tag.

state_get_req_id(State) ->
    State#state.req_id.

state_get_waitees(State) ->
    State#state.waitees.

deadstate_get_worker(State) ->
    State#deadstate.worker.

deadstate_get_deadlock(State) ->
    State#deadstate.deadlock.

deadstate_is_foreign(State) ->
    State#deadstate.foreign.

%%%======================
%%% API Functions
%%%======================

start(Module, Args) ->
    start(Module, Args, []).

start(Module, Args, Options) ->
    %% We allow running unmonitored systems via options
    case proplists:get_value(unmonitored, proplists:get_value(ddmon_opts, Options, []), false) of
        true ->
            gen_server:start(Module, Args, Options);
        false ->
            %% Elixir compat
            ChildOptions = proplists:delete(name, Options),
            case proplists:get_value(name, Options) of
                undefined ->
                    gen_statem:start(?MODULE, {Module, Args, ChildOptions}, Options);
                Name when is_atom(Name) ->
                    gen_statem:start({local, Name}, ?MODULE, {Module, Args, ChildOptions}, Options);
                {global, Name} ->
                    gen_statem:start({global, Name}, ?MODULE, {Module, Args, ChildOptions}, Options)
            end
    end.


start_link(Module, Args) ->
    start_link(Module, Args, []).

start_link(Module, Args, Options) ->
    %% We allow running unmonitored systems via options
    case proplists:get_value(unmonitored, proplists:get_value(ddmon_opts, Options, []), false) of
        true ->
            gen_server:start_link(Module, Args, Options);
        false ->
            %% Elixir compat
            ChildOptions = proplists:delete(name, Options),
            case proplists:get_value(name, Options) of
                undefined ->
                    gen_statem:start_link(?MODULE, {Module, Args, ChildOptions}, Options);
                Name when is_atom(Name) ->
                    gen_statem:start_link({local, Name}, ?MODULE, {Module, Args, ChildOptions}, Options);
                {global, Name} ->
                    gen_statem:start_link({global, Name}, ?MODULE, {Module, Args, ChildOptions}, Options)
            end
    end.


%%%======================
%%% gen_server interface
%%%======================

call(Server, Request) ->
    call(Server, Request, 5000).
call(Server, Request, Timeout) ->
    case get(?MON_PID) of
        undefined ->
            gen_server:call(Server, Request, Timeout);
        Mon ->
            gen_statem:call(Mon, {Request, Server}, Timeout)
    end.

cast(Server, Message) ->
    gen_server:cast(Server, Message).


stop(Server, Reason, Timeout) ->
    gen_server:stop(Server, Reason, Timeout).

%%%======================
%%% gen_statem Callbacks
%%%======================

init({Module, Args, Options}) ->
    DlsOpts = proplists:get_value(ddmon_opts, Options, []),
    ProcOpts = proplists:delete(ddmon_opts, proplists:delete(name, Options)),
    case gen_monitored:start_link(Module, Args, ProcOpts) of
        {ok, Pid} ->
            State =
                #state{worker = Pid,
                       waitees = gen_statem:reqids_new(),
                       req_tag = undefined,
                       req_id = undefined
                      },
            put(?PROBE_DELAY, proplists:get_value(probe_delay, DlsOpts, -1)),
            {ok, unlocked, State};
        E -> E
    end.


terminate(_Reason, _Data) ->
    ok.

terminate(_Reason, _State, _Data) ->
    ok.

%% terminate(Reason, #state{worker = Worker}) ->
%%     erlang:unlink(Worker),
%%     gen_server:stop(Worker, Reason, 3000);
%% terminate(Reason, #deadstate{worker = Worker}) ->
%%     erlang:unlink(Worker),
%%     gen_server:stop(Worker, Reason, 3000).


callback_mode() ->
    [state_functions, state_enter].


unlocked(enter, _, _) ->
    keep_state_and_data;

unlocked({call, From}, '$get_child', #state{worker = Worker}) ->
    {keep_state_and_data, {reply, From, Worker}};


%% Our service wants a call to itself (either directly or the monitor)
unlocked({call, {Worker, PTag}}, {_Msg, Server}, _State = #state{worker = Worker, waitees = Waitees})
  when Server =:= Worker orelse Server =:= self() ->
    [ begin
          gen_statem:reply(W, {?DEADLOCK, [self(), self()]})
      end
      || {_, W} <- gen_statem:reqids_to_list(Waitees)
    ],
    {next_state, deadlocked,
     #deadstate{worker = Worker, deadlock = [self(), self()], req_id = PTag}
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

%% Scheduled probe
unlocked(cast, {?SCHEDULED_PROBE, _To, _Probe}, _State) ->
    keep_state_and_data;

%% Process died
unlocked(info, {'DOWN', _, process, Worker, Reason}, #state{worker=Worker}) ->
    {stop, Reason};

%% Someone (???) died
unlocked(info, {'DOWN', _, process, _Worker, _Reason}, _) ->
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
            Worker ! Msg,
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

    case get(?PROBE_DELAY) of
        -1 ->
            %% Send a probe
            gen_statem:cast(element(1, From), {?PROBE, PTag, [self()]});
        N when is_integer(N) ->
            %% Schedule a delayed probe
            Self = self(),
            spawn_link(
              fun() ->
                      timer:sleep(N),
                      gen_statem:cast(Self, { ?SCHEDULED_PROBE
                                            , _To = element(1, From)
                                            , _Probe = {?PROBE, PTag, [Self]}
                                            })
                  end)
    end,

    {keep_state,
     State#state{waitees = Waitees1}
    };

%% Process died
locked(info, {'DOWN', _, process, Worker, Reason}, #state{worker=Worker}) ->
    {stop, Reason};

%% Someone (???) died
locked(info, {'DOWN', _, process, _Worker, _Reason}, _) ->
    keep_state_and_data;

%% Incoming reply
locked(info, Msg, State = #state{worker = Worker, req_tag = PTag, req_id = ReqId, waitees = Waitees}) ->
    case gen_statem:check_response(Msg, ReqId) of
        no_reply ->
            %% Unknown info. Let the process handle it.
            Worker ! Msg,
            keep_state_and_data;

        {reply, {?DEADLOCK, DL}} ->
            %% Deadlock information
            [ begin
                  gen_statem:reply(W, {?DEADLOCK, [self() | DL]})
              end
              || {_, W} <- gen_statem:reqids_to_list(Waitees)
            ],
            {next_state, deadlocked, #deadstate{foreign = true, worker = Worker, deadlock = [self() | DL], req_id = ReqId}};

        {reply, Reply} ->
            %% Pass the reply to the process. We are unlocked now.
            {next_state, unlocked,
             State,
             {reply, {Worker, PTag}, Reply}
            }
    end;

%% Incoming own probe. Alarm! Panic!
locked(cast, {?PROBE, PTag, Chain}, #state{worker = Worker, req_tag = PTag, req_id = ReqId, waitees = Waitees}) ->
    [ begin
          gen_statem:reply(W, {?DEADLOCK, [self() | Chain]})
      end
      || {_, W} <- gen_statem:reqids_to_list(Waitees)
    ],
    {next_state, deadlocked, #deadstate{worker = Worker, deadlock = [self() | Chain], req_id = ReqId}};

%% Incoming probe
locked(cast, {?PROBE, Probe, Chain}, #state{waitees = Waitees}) ->
    %% Propagate the probe to all waitees.
    [ begin
          gen_statem:cast(W, {?PROBE, Probe, [self()|Chain]})
      end
      || {_, {W, _}} <- gen_statem:reqids_to_list(Waitees)
    ],
    keep_state_and_data;

%% Scheduled probe
locked(cast, {?SCHEDULED_PROBE, To, Probe = {?PROBE, PTagProbe, _}}, #state{req_tag = PTag}) ->
    case PTagProbe =:= PTag of
        true ->
            gen_statem:cast(To, Probe);
        false ->
            ok
    end,
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

%% Incoming external call. We just tell them about the deadlock.
deadlocked({call, From}, Msg, State = #deadstate{deadlock = DL}) ->
    %% Forward to the process just in case
    gen_server:send_request(State#deadstate.worker, Msg),
    %% gen_statem:reply(element(1, From),k ),
    {keep_state_and_data, {reply, From, {?DEADLOCK, DL}}};

deadlocked({call, _From}, Msg, State) ->
    %% Forward to the process, who cares
    gen_server:send_request(State#deadstate.worker, Msg),
    keep_state_and_data;

%% Probe
deadlocked(cast, {?PROBE, _, _}, _State) ->
    keep_state_and_data;

%% Scheduled probe
deadlocked(cast, {?SCHEDULED_PROBE, _To, _Probe}, _State) ->
    keep_state_and_data;

%% Unknown cast
deadlocked(cast, Msg, #deadstate{worker = Worker}) ->
    gen_server:cast(Worker, Msg),
    keep_state_and_data;

%% Process died
deadlocked(info, {'DOWN', _, process, Worker, Reason}, #state{worker=Worker}) ->
    {stop, Reason};

%% Someone (???) died
deadlocked(info, {'DOWN', _, process, _Worker, _Reason}, _) ->
    keep_state_and_data;

%% Incoming random message
deadlocked(info, Msg, #deadstate{worker = Worker, req_id = ReqId}) ->
    case gen_statem:check_response(Msg, ReqId) of
        no_reply ->
            %% Forward to the process, who cares
            Worker ! Msg,
            keep_state_and_data;
        {reply, {?DEADLOCK, _}} ->
            keep_state_and_data;
        {reply, Reply} ->
            %% A reply after deadlock?!
            error({'REPLY_AFTER_DEADLOCK', Reply})
    end.
