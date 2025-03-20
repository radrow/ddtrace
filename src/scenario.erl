-module(scenario).

-export([run/1, run/2]).

-include("dlstalk.hrl").


%% Turns a session description into something evaluable:
%% - Dereferences process indices into PIDs
%% - Evaluates randomness
fix_session(_Map, []) -> [];
fix_session(_Map, spawn) ->
    spawn;
fix_session(Map, I) when is_integer(I) ->
    fix_session(Map, [I]);
fix_session(Map, A) when is_atom(A) ->
    fix_session(Map, [A]);
fix_session(Map, [{random, Is} | Session]) when is_list(Is) ->
    I = lists:nth(rand:uniform(length(Is)), Is),
    fix_session(Map, [I | Session]);
fix_session(Map, [{'let', Name, Expr} | Session]) ->
    [{'let', Name, fix_session(Map, Expr)} | fix_session(Map, Session)];
fix_session(Map, [I | Session]) when is_integer(I) ->
    {M, _P} = maps:get(I, Map),
    [M | fix_session(Map, Session)];
fix_session(Map, [A | Session]) when is_atom(A) ->
    [A | fix_session(Map, Session)];
fix_session(Map, [Instr | Session]) when is_atom(element(1, Instr)) ->
    {fix_session(Map, Instr), fix_session(Map, Session)};
fix_session(_Map, {wait, TimeMin, TimeMax}) ->
    {wait, TimeMin + rand:uniform(TimeMax - TimeMin)};
fix_session(_Map, Session) when is_atom(element(1, Session)) ->
    Session;
fix_session(Map, Session) when is_tuple(Session) ->
    L = tuple_to_list(Session),
    list_to_tuple([fix_session(Map, S) || S <- L]);
fix_session(Map, Session) when is_map(Session) ->
    maps:map(fun (_, S) -> fix_session(Map, S) end, Session).

fix_scenario(Map, Scenario) ->
    [ {SessionId, {element(1, maps:get(Init, Map)), fix_session(Map, Session)}}
     || {SessionId, {Init, Session}} <- Scenario
    ].


%% Estimates (poorly) how many actors there are in a scenario
session_size([]) -> 0;
session_size([Stmt|Rest]) ->
    max(session_size(Stmt), session_size(Rest));
session_size(I) when is_integer(I) ->
    I;
session_size(A) when is_atom(A) ->
    0;
session_size({random, Is}) when is_list(Is) ->
    lists:max(Is);
session_size({'let', _Name, Expr}) ->
    session_size(Expr);
session_size(W) when element(1, W) =:= wait ->
    0;
session_size(S) when is_tuple(S) ->
    session_size(tuple_to_list(S)).

scenario_size(Scenario) ->
    lists:max([0 | [ max(Init, session_size(Session))
                     || {_SessionId, {Init, Session}} <- Scenario
                   ]]).


%% Estimates (poorly) what timeout should be set for a scenario
session_time([]) -> 0;
session_time({wait, Time}) ->
    Time;
session_time({'let', _Name, Expr}) ->
    session_time(Expr);
session_time(I) when is_integer(I) orelse is_pid(I) orelse is_atom(I) ->
    10; % Super precise estimate on how long a call takes
session_time([S|Rest]) ->
    session_time(S) + session_time(Rest);
session_time(S) when is_tuple(S) ->
    session_time(tuple_to_list(S)).

scenario_time(Scenario) ->
    lists:max([ 0 | [ session_time(Session)
                      || {_SessionId, {_Init, Session}} <- Scenario
                    ]]).


%% Prepares and evaluates a scenario
run_scenario(Scenario, Opts) ->
    Init = self(),
    {LogKnown, LogFresh} = logging:mk_ets(),
    logging:conf(Opts),

    logging:remember(Init, 'I', 0),

    Routers = proplists:get_all_values(router, Opts),

    ProcMap = maps:from_list(
            [ begin
                  Args = case proplists:lookup(I, Routers) of
                             {I, N} -> {router, N};
                             none -> worker
                         end,
                  {ok, M} = 'Elixir.Dlstalk.TestServer':start_link(I, Args),
                  P = gen_statem:call(M, '$get_child'),
                  logging:remember(M, 'M', I),
                  logging:remember(P, 'P', I),
                  {I, {M, P}}
              end
             || I <- lists:seq(0, scenario_size(Scenario))
            ]
           ),

    [ logging:remember(SessionId, 'S', 0)
     || {SessionId, _} <- Scenario
    ],

    [ error({init_on_router, SessionId, SInit})
     || {SessionId, {SInit, _}} <- Scenario,
        proplists:is_defined(SInit, Routers)
    ],

    FScenario = fix_scenario(ProcMap, Scenario),

    FullProcList =
        maps:fold(
          fun(I, {M, P}, Acc) ->
                  SubM = gen_server:call(P, '$get_workers'),
                  SubP = [gen_server:call(SM, '$get_child') || SM <- SubM],
                  Sub = lists:zip(SubM, SubP),
                  [ begin
                        logging:remember(SM, {'M', SI}, I),
                        logging:remember(SP, {'P', SI}, I)
                    end
                   || {SI, {SM, SP}} <- lists:enumerate(Sub)
                  ],
                  [{M, P} | Sub ++ Acc]
          end,
          [],
          ProcMap
         ),

    Timeout = case proplists:get_value(timeout, Opts, 0) of
                  0 -> scenario_time(FScenario) + 2000;
                  N when is_integer(N) -> N
              end,
    logging:log_scenario(FScenario, Timeout),

    Tracer = tracer:start_link(FullProcList, [{logging_ets_known, LogKnown}, {logging_ets_fresh, LogFresh} | Opts]),
    InitTime = erlang:monotonic_time(),

    Folder = fun({_SessionId, []}, ReqIds) -> ReqIds;
                ({SessionId, {SessionInit, Session}}, ReqIds) ->
                     R = gen_statem:send_request(SessionInit, {SessionId, Session}),
                     gen_statem:reqids_add(R, SessionId, ReqIds)
             end,
    Reqs = lists:foldl(Folder, gen_statem:reqids_new(), FScenario),

    Result = receive_responses(Reqs, Timeout),

    Log = tracer:finish(Tracer, [M || {_, {M, _P}} <- maps:to_list(ProcMap)]),

    [begin
         logging:log_trace(InitTime, lists:sort(Log)),
         logging:print_log_stats(Log)
     end
     || not proplists:get_value(live_log, Opts, false)],

    case proplists:get_value(csv, Opts, false) of
        false -> ok;
        CsvPath ->
            Csv = logging:trace_csv(InitTime, lists:sort(Log)),
            file:write_file(CsvPath, Csv)
    end,

    case Result of
        ok ->
            logging:log_terminate();
        timeout ->
            logging:log_timeout()
    end,

    {Log, Result}.


%% Wait for all sessions to terminate, or timeout
receive_responses(Reqs0, Time) ->
    case gen_statem:receive_response(Reqs0, Time, true) of
        no_request ->
            ok;
        timeout ->
            timeout;
        {{reply, _}, _Session, Reqs1} ->
            receive_responses(Reqs1, Time)
    end.

%% Parse scenario together with in-file options
parse_scenario(Test) ->
    Sessions = proplists:get_value(sessions, Test),
    Opts = proplists:delete(sessions, Test),

    {Sessions, Opts}.


%% Set RNG seed according to the config
set_seed(Opts) ->
    case proplists:get_value(seed, Opts) of
        undefined ->
            rand:seed(exs1024s);
        I when is_integer(I) ->
            rand:seed(exs1024s, I)
    end.


%% Reads and evaluates a scenario file
run(Filename) ->
    run(Filename, []).

run(dFilename, Opts) ->
    Gens =
        [ {lurker, x, lists:seq(0, 20)}
        , {lurker, x, lists:seq(21, 30)}
        , {lurker, x, lists:seq(31, 40)}
        , {lurker, x, [51|lists:seq(0, 50)]}
        ],
    S = scenario_gen:generate(Gens),
    S1 = scenario_gen:generate(Gens),
    %% io:format("SCEN: ~p\n\n", [S]),
    %% run_many([S, S1], Opts).
    run_scenario(S, Opts);

run(Filename, Opts) ->
    case file:consult(Filename) of
        {ok, File} ->
            logging:conf(Opts),
            set_seed(Opts),
            {Scenario, FileOpts} = parse_scenario(File),
            run_scenario(Scenario, Opts ++ FileOpts);

        {error, Err} ->
            io:format(standard_error, "Can't read ~s: ~s\n", [Filename, file:format_error(Err)]),
            halt(2)
    end.

%% gen_gen(Size) ->
%%     [

%%     ]

run_many(Scenarios, Opts) ->
    Self = self(),
    Workers =
        [ begin
              R = rand:uniform(2137),
              spawn_link(
                fun() ->
                        {Log, Result} = run_scenario(Scenario, [logging_silent,{seed, R}|Opts]),
                        Stats = logging:log_stats(Log),
                        Self ! {self(), Stats, Result}
                end)
          end
                  || Scenario <- Scenarios
          ],
    Stats = [ receive {W, S} -> S end
              || W <- Workers
            ],

    {ok, Log} = file:open("log.csv", [write]),
    erlang:group_leader(Log, self()),
    io:format(Log, "run,total,sent,queries,replies,probes,success\n", []),
    [ io:format(Log, "~p,~p,~p,~p,~p,~p,~p\n", [I,Total,Sent,Queries,Replies,Probes,if Result == ok -> 1; true -> 0 end])
      || {I, #{total := Total,
               sent := Sent,
               queries := Queries,
               replies := Replies,
               probes := Probes,
               picks := _Picks
              }, Result} <- lists:enumerate(Stats)
    ],
    file:close(Log).
