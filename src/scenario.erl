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

    Tracer = tracer:start_link(FullProcList, Opts),
    InitTime = erlang:monotonic_time(),

    Folder = fun({_SessionId, []}, ReqIds) -> ReqIds;
                ({SessionId, {SessionInit, Session}}, ReqIds) ->
                     R = gen_statem:send_request(SessionInit, {SessionId, Session}),
                     gen_statem:reqids_add(R, SessionId, ReqIds)
             end,
    Reqs = lists:foldl(Folder, gen_statem:reqids_new(), FScenario),

    Result = receive_responses(Reqs, Timeout),

    Log = tracer:finish(Tracer, [M || {_, {M, _P}} <- maps:to_list(ProcMap)]),

    [logging:log_trace(InitTime, lists:sort(Log)) || not proplists:get_value(live_log, Opts, false)],

    case Result of
        ok ->
            logging:log_terminate();
        timeout ->
            logging:log_timeout()
    end,

    ok.


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


%% Reads and evaluates a scenario file
run(Filename) ->
    run(Filename, []).

run(Filename, Opts) ->
    case file:consult(Filename) of
        {ok, File} ->
            logging:conf(Opts),
            rand:seed(exs1024s),
            {Sessions, FileOpts} = parse_scenario(File),
            run_scenario(Sessions, Opts ++ FileOpts);

        {error, Err} ->
            io:format(standard_error, "Can't read ~s: ~s\n", [Filename, file:format_error(Err)]),
            halt(2)
    end.
