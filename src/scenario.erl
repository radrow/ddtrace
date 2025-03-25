-module(scenario).

-export([grid_printer/2]).
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
    T = TimeMin + rand:uniform(TimeMax + 1 - TimeMin) - 1,
    {wait, T};
fix_session(_Map, Session) when is_atom(element(1, Session)) ->
    Session;
fix_session(Map, Session) when is_tuple(Session) ->
    L = tuple_to_list(Session),
    list_to_tuple([fix_session(Map, S) || S <- L]);
fix_session(Map, Session) when is_map(Session) ->
    maps:map(fun (_, S) -> fix_session(Map, S) end, Session).

fix_scenario(Map, Scenario) ->
    [ {SessionId,
       {case Init of
           _ when is_integer(Init) ->
               element(1, maps:get(Init, Map));
            {Wait, I} ->
                { fix_session(Map, Wait)
                , element(1, maps:get(I, Map))
                }
        end,
        fix_session(Map, Session)}}
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
    lists:max([0 | [ case Init of
                         _ when is_integer(Init) -> max(Init, session_size(Session));
                         {_, I} when is_integer(I) -> max(I, session_size(Session))
                     end
                     || {_SessionId, {Init, Session}} <- Scenario
                   ]]).


%% Estimates (poorly) what timeout should be set for a scenario
session_time([]) -> 0;
session_time({wait, Time}) ->
    Time;
session_time({'let', _Name, Expr}) ->
    session_time(Expr);
session_time(I) when is_integer(I) orelse is_pid(I) orelse is_atom(I) ->
    1; % Super precise estimate on how long a call takes
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

    {ok, Supervisor} = scenario_supervisor:start_link(),

    GsModule = 'Elixir.Dlstalk.TestServer',
    {module, _} = code:ensure_loaded(GsModule),

    ProcMap = maps:from_list(
            [ begin
                  Args = case proplists:lookup(I, Routers) of
                             {I, N} -> {router, N};
                             none -> worker
                         end,
                  ChildSpec =
                      #{id => I,
                        start => {GsModule, start_link, [I, Args, [{dlstalk_opts, Opts}]]},
                        restart => transient,
                        shutdown => 5000,
                        type => worker},
                  {ok, M} = supervisor:start_child(Supervisor, ChildSpec),
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
                  0 -> scenario_time(FScenario)
                           + proplists:get_value(probe_delay, Opts, 0)
                           + 2000;
                  N when is_integer(N) -> N
              end,
    %% io:format("TT: ~p\n", [Timeout]),
    logging:log_scenario(FScenario, Timeout),

    Tracer = tracer:start_link(FullProcList, [{logging_ets_known, LogKnown}, {logging_ets_fresh, LogFresh} | Opts]),
    InitTime = erlang:monotonic_time(),

    Folder = fun({_SessionId, []}, ReqIds) -> ReqIds;
                ({SessionId, {SessionInit, Session}}, ReqIds) ->
                     SessionInitProc =
                         case SessionInit of
                             _ when is_integer(SessionInit) orelse is_pid(SessionInit) -> SessionInit;
                             {{wait, 0}, I} -> I;
                             {{wait, T}, I} ->
                                 % TODO this initial waiting has to be programmed better (and definitely not sequential)
                                 timer:sleep(T),
                                 I
                         end,
                     R = gen_statem:send_request(SessionInitProc, {SessionId, Session}),
                     gen_statem:reqids_add(R, SessionId, ReqIds)
             end,
    Reqs = lists:foldl(Folder, gen_statem:reqids_new(), FScenario),

    Result = receive_responses(Reqs, Timeout),
    timer:sleep(500),

    %% Log = [{{1, 1}, self(), {wait, 0}}],
    Log = tracer:finish(Tracer, [M || {_, {M, _P}} <- maps:to_list(ProcMap)]),

    case proplists:get_value(stats_csv, Opts, false) of
        false -> ok;
        SLogFile ->
            file:write_file(SLogFile, [logging:log_stats(Log)])
    end,
    [begin
         logging:log_trace(InitTime, lists:sort(Log)),
         logging:print_log_stats(Log)
     end
     || not proplists:get_value(live_log, Opts, false),
        not proplists:get_value(silent, Opts, false)
    ],

    case proplists:get_value(csv, Opts, false) of
        false -> ok;
        CsvPath ->
            %% io:format("Preparing csv ~p\n", [CsvPath]),
            Csv = logging:trace_csv(InitTime, lists:sort(Log)),
            %% io:format("Done csv\n"),
            ok = file:write_file(CsvPath, Csv)
    end,

    case Result of
        ok ->
            logging:log_terminate();
        {deadlock, Deadlocks} ->
            logging:log_deadlock(Deadlocks);
        timeout ->
            logging:log_timeout()
    end,

    unlink(Supervisor),
    exit(Supervisor, shutdown),

    {Log, Result}.


%% Wait for all sessions to terminate, or timeout
receive_responses(Reqs0, Time) ->
    receive_responses(Reqs0, Time, 0).
receive_responses(Reqs0, Time, Deadlocks) ->
    case gen_statem:receive_response(Reqs0, Time, true) of
        no_request when Deadlocks =:= 0 ->
            ok;
        no_request ->
            {deadlock, Deadlocks};
        timeout ->
            timeout;
        {{reply, R}, _Session, Reqs1} ->
            receive_responses(Reqs1, Time, Deadlocks + case R of {?YOU_DIED, _} -> 1; _ -> 0 end)
    end.

%% Parse scenario together with in-file options
parse_scenario(Test) ->
    case {proplists:get_value(sessions, Test), proplists:get_value(bench, Test), proplists:get_value(gen, Test)} of
        {undefined, undefined, undefined} ->
            error(what_to_do);
        {Sessions, undefined, undefined} ->
            Opts = proplists:delete(sessions, Test),
            {one, Sessions, Opts};
        {undefined, Bench, undefined} ->
            Opts = proplists:delete(bench, Test),
            {bench, Bench, Opts};
        {undefined, undefined, Gen} ->
            Opts = proplists:delete(gen, Test),
            {gen, Gen, Opts};
        _ ->
            error(decide_bench_or_sessions)
    end.


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

run(Filename, Opts) ->
    io:format("Node: ~p\n", [node()]),

    case file:consult(Filename) of
        {ok, File} ->
            logging:conf(Opts),
            set_seed(Opts),
            case parse_scenario(File) of
                {one, Scenario, FileOpts} ->
                    run_scenario(Scenario, Opts ++ FileOpts);
                {bench, Bench, FileOpts} ->
                    run_bench(Bench, Opts ++ FileOpts);
                {gen, Gen, FileOpts} ->
                    {Type, _Rep, Size} = Gen,
                    Scen = gen_gen(Type, Size),
                    %% io:format("GEN: ~p\n\n", [Scen]),
                    run_scenario(Scen, Opts ++ FileOpts)
            end;

        {error, Err} ->
            io:format(standard_error, "Can't read ~s: ~s\n", [Filename, file:format_error(Err)]),
            halt(2)
    end.


gen_gen(nodead, Size) ->
    TreeSize = ceil(math:sqrt(Size)),
    Trees = Size div TreeSize,
    scenario_gen:generate(
      [ {tree, I, lists:seq(I * TreeSize, I * TreeSize + TreeSize - 1)}
        || I <- lists:seq(1, Trees)
      ]);
gen_gen(dead, Size) ->
    GroupSize = ceil(math:sqrt(Size)) * 2,
    Groups = Size div GroupSize,
    scenario_gen:generate(
      [
        {envelope, I,
         { lists:seq(I * GroupSize, I * GroupSize + GroupSize - 1)
         , [ {safe, 0.05 + rand:uniform() / 20}
           ]
         }
        }
        || I <- lists:seq(0, Groups - 1)
      ]
      ++
          [ begin
                %% Space = lists:seq(0, Size - 1),
                %% Space = lists:seq(I * GroupSize, I * GroupSize + GroupSize - 1),
                Space = element(1, lists:split(GroupSize * 2, scenario_gen:shuffle(lists:seq(1, Size)))),
                %% io:format("PROVO: ~p ~p ~p", [I, Size + I, Space]),
                {tree, provocator,
                 { [Groups * GroupSize + I | Space]
                 , [{spread, 3}]
                 }
                }
            end
            || I <- lists:seq(1, Size - GroupSize * Groups + 1)
          ]
     );
%% gen_gen(dead, Size) ->
%%     Trees = ceil(math:sqrt(Size)),
%%     scenario_gen:generate(
%%       [ {tree, I, lists:seq(0, Size)}
%%         || I <- lists:seq(1, Trees)
%%       ] ++ [{loop, deadlocker, {lists:seq(0, Size), [{delay, 2 * Size}]}}]
%%      );
gen_gen(conditional, Size) ->
    GroupSize = ceil(math:sqrt(Size)),
    Groups = Size div GroupSize,
    %% Provos = max(1, Size - GroupSize * Groups),
    %% MinLen = ceil(math:sqrt(GroupSize)),
    scenario_gen:generate(
      [
        {envelope, I,
         { lists:seq(I * GroupSize, I * GroupSize + GroupSize - 1)
         , [ {safe, 0.8 + rand:uniform() / 5}
           ]
         }
        }
        || I <- lists:seq(0, Groups - 1)
      ]
      ++
          [ begin
                Space = lists:seq(0, Size),
                %% Space = lists:seq(I * GroupSize, I * GroupSize + GroupSize - 1),
                %% Space = element(1, lists:split(GroupSize div 2, scenario_gen:shuffle(lists:seq(1, Size)))),
                %% io:format("PROVO: ~p ~p ~p", [I, Size + I, Space]),
                {tree, provocator,
                 { [Size + I | Space]
                 , [{spread, rand:uniform(2)}]
                 }
                }
            end
            || I <- lists:seq(1, 1)
          ]
     ).
    %% scenario_gen:generate(
    %%   [ {envelope, I,
    %%      { lists:seq(I * GroupSize, I * GroupSize + GroupSize - 1)
    %%      , [{target, Target}, {min_len, MinLen}]
    %%      }
    %%     }
    %%     || I <- lists:seq(1, Groups)
    %%   ]).

run_bench(Bench, Opts) ->
    %% io:format("\n\nGEN: ~p\n\n", [Scens]),
    run_many(Bench, Opts).


-define(GRID_WIDTH, 50).
grid_printer() ->
    receive {workers, Ws, MaxSize} -> grid_printer(Ws, MaxSize) end.
grid_printer(Ws, MaxSize) ->
    State = maps:from_list([{W, init} || W <- Ws]),
    print_size(0, MaxSize),
    print_grid(State),
    grid_printer_loop(State, 0, MaxSize).
print_grid(State) ->
    S = "[" ++
        print_grid(lists:sort(maps:to_list(State)), 0) ++
        " ]\n",
    io:format("~s", [S]).

print_grid([], _) -> "";
print_grid(Ws, ?GRID_WIDTH) ->
    "\n " ++
        print_grid(Ws, 0);
print_grid([{_W, S}|Ws], I) ->
    io_lib:format(" ~s", [case S of
                              init -> ansi_color:render({[black_l, bold], "."});
                              woke -> ansi_color:render({[yellow, bold, dim], "o"});
                              busy -> ansi_color:render({[yellow_l, bold], "O"});
                              done -> ansi_color:render({[green, bold], "@"});
                              dead -> ansi_color:render({[violet, bold, dim], "#"});
                              down -> ansi_color:render({[red, blink, bold], "!"});
                              time -> ansi_color:render({[blue_l, bold], "T"})
                          end]) ++
        print_grid(Ws, I + 1).
print_size(Size, MaxSize) ->
    Width = ?GRID_WIDTH * 2,
    Percent = (Width * Size) div (MaxSize * 2),
    Free = Width - Percent,
    io:format("\r~s\r>>~s~s<<\n", [ lists:flatten([" " || _ <- lists:seq(1, Width + 4)])
                                , lists:flatten(["-" || _ <- lists:seq(1, Percent)])
                                , lists:flatten([if Free < 0 -> "~"; true -> " " end || _ <- lists:seq(1, abs(Free))])
                                ]).
clean_grid(Ws) ->
    Height = ((maps:size(Ws) - 1) div ?GRID_WIDTH) + (_CompensateMinus1 = 1) + (_SizeRow = 1),
    io:format("\e[~pA\r", [Height]).
grid_printer_loop(State, Size, MaxSize) ->
    receive
        {update, W, WSize, S} ->
            State1 = State#{W => S},
            Size1 = Size + WSize,
            clean_grid(State1),
            print_size(Size1, MaxSize),
            print_grid(State1),
            grid_printer_loop(State1, Size1, MaxSize);
        {From, finito} ->
            From ! {self(), ok}
    end.

get_results(Workers, Max, Printer) ->
    get_results([], Workers, 0, Max, Printer, []).
get_results([], [], _CurrSize, _Max, _Printer, Acc) ->
    Acc;
get_results(Current, Queue, CurrSize, Max, Printer, Acc) ->
    case CurrSize >= Max orelse Queue =:= [] of
        true ->
            receive
                {'DOWN', _, process, W, E} when E =/= normal ->
                    [Size] = [S || {Ww, S} <- Current, Ww =:= W],
                    Printer ! {update, W, -Size, down},
                    %% erlang:garbage_collect(W, [{type, minor}]),
                    get_results(Current -- [{W, Size}], Queue, CurrSize - Size, Max, Printer, Acc);
                {success, _Ref, W, Type, Size, Stats, Res} ->
                    %% erlang:garbage_collect(W, [{type, minor}]),
                    get_results(Current -- [{W, Size}], Queue, CurrSize - Size, Max, Printer, [{Type, Size, Stats, Res}|Acc])
            end;
        false ->
            [{T, S, W, _}|Ws] = Queue,
            W ! {work, T, S},
            get_results([{W, S}|Current], Ws, CurrSize + S, Max, Printer, Acc)
    end.

run_many(Bench, Opts) ->
    Self = self(),
    Ref = make_ref(),
    Printer = spawn(fun grid_printer/0),

    Workers =
        [ begin
              R = rand:uniform(21370000),
              {P, MonRef} = spawn_monitor(
                fun() ->
                        {Type, Size} = receive {work, T, S} -> {T, S} end,
                        Printer ! {update, self(), Size, woke},
                        Scenario = gen_gen(Type, Size),
                        SOpts = [ {seed, R}
                                , silent
                                | proplists:delete(stats_csv, proplists:delete(csv, Opts)) % TODO fix formatting
                                ] ++ case proplists:get_value(csv, Opts) of
                                         undefined -> [];
                                         Dir ->
                                             SDir = if is_binary(Dir) -> binary:bin_to_list(Dir); true -> Dir end,
                                             CsvName = atom_to_list(Type) ++ "__" ++ integer_to_list(Size) ++ "__" ++ integer_to_list(R) ++ ".csv",
                                             Path = SDir ++ "/" ++ CsvName,
                                             [{csv, Path}]
                                     end,
                        Printer ! {update, self(), 0, busy},
                        {Log, Result} = run_scenario(Scenario, SOpts),
                        Printer ! {update, self(), -Size, case Result of ok -> done; {deadlock, _} -> dead; timeout -> time end},
                        Stats = logging:log_stats(Log),
                        logging:delete(),
                        Self ! {success, Ref, self(), Type, Size, Stats, Result}
                end),
              {WType, WSize, P, MonRef}
          end
          || {WType, Reps, WSize} <- Bench,
             _ <- lists:seq(1, Reps)
        ],

    MAX_SIZE = 150000,

    Printer ! {workers, [W || {_, _, W, _} <- Workers], MAX_SIZE},
    Stats = get_results(scenario_gen:shuffle(Workers), MAX_SIZE, Printer),

    Printer ! {self(), finito},
    receive {Printer, ok} -> ok end,

    %% Appending = filelib:is_file(proplists:get_value(stats_csv, Opts, "")),
    LogFile = proplists:get_value(stats_csv, Opts, <<"/dev/stdout">>),
    io:format("Writing log to: ~s\n", [LogFile]),

    {ok, Log} = file:open(binary:bin_to_list(LogFile), [write]),

    io:format(Log, "run,time,type,size,total,sent,inits,mon_mon,proc_mon,mon_proc,proc_proc,queries,replies,probes,success,deadlock\n", []),
    [ io:format(Log, "~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p\n",
                [I,Time,Type,Size,Total,Sent,Inits,MonMon,ProcMon,MonProc,ProcProc,Queries,Replies,Probes,
                 case Result of timeout -> 2; {deadlock, _} -> 1; ok -> 0 end, Deadlocks])
      || {I, {Type, Size,
              #{total := Total,
                sent := Sent,
                inits := Inits,
                mon_mon := MonMon,
                proc_mon := ProcMon,
                mon_proc := MonProc,
                proc_proc := ProcProc,
                queries := Queries,
                replies := Replies,
                probes := Probes,
                unlocks := _Unlocks,
                locks := _Locks,
                deadlocks := Deadlocks,
                picks := _Picks,
                time := Time
               }, Result}} <- lists:enumerate(Stats)
    ],
    case LogFile of
        <<"/dev/stdout">> -> ok;
        _ -> file:close(Log)
    end.
