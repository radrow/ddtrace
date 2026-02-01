-module(scenario).
%% @doc """ Module to run SRPC scenarios for testing deadlock detection and
%% tracing. Not an official part of DDTrace. Not my finest code, either."""

-export([grid_printer/2]).
-export([run/1, run/2]).

-include_lib("ddtrace/include/ddtrace.hrl").


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
    {_M, P} = maps:get(I, Map),
    [P | fix_session(Map, Session)];
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
               element(2, maps:get(Init, Map));
            {Wait, I} ->
                { fix_session(Map, Wait)
                , element(2, maps:get(I, Map))
                }
        end,
        fix_session(Map, Session)}}
     || {SessionId, {Init, Session}} <- Scenario
    ].


%% Essentially the opposite of `fix_scenario`. Replaces PIDs with indices and
%% removes unnecessary bloat.
depidify(Map, Feed, Pid) when is_pid(Pid) ->
    case maps:find(Pid, Map) of
        {ok, I} -> {Map, Feed, I};
        error -> {maps:put(Pid, Feed, Map), Feed + 1, Feed}
    end;
depidify(Map, Feed, {{X}}) ->
    depidify(Map, Feed, {X});
depidify(Map, Feed, [H|T]) when H =:= [];
                                H =:= {};
                                H =:= {wait, 0} ->
    depidify(Map, Feed, T);
%% depidify(Map, Feed, [H|T]) when not is_list(T) ->
%%     depidify(Map, Feed, [H, T]);
depidify(Map, Feed, [H|T]) ->
    {Map1, Feed1, H1} = depidify(Map, Feed, H),
    {Map2, Feed2, T1} = depidify(Map1, Feed1, T),
    {Map2, Feed2, [H1|T1]};
depidify(Map, Feed, T) when is_tuple(T) ->
    L = tuple_to_list(T),
    {M1, F1, L1} = depidify(Map, Feed, L),
    {M1, F1, list_to_tuple(L1)};
depidify(Map, Feed, X) ->
    {Map, Feed, X}.


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

    OptDDMon = proplists:get_value(ddmon, Opts, false),
    
    case OptDDMon of
        false -> 
            TracerMod = srpc_tracer,
            GsModule = 'Elixir.DDTrace.TestServer';
        true -> 
            TracerMod = srpc_ddmon_tracer,
            GsModule = 'Elixir.DDTrace.TestServerInstrumented'
    end,

    {module, _} = code:ensure_loaded(GsModule),

    {ok, Supervisor} = scenario_supervisor:start_link(),
    {ok, MonReg} = mon_reg:start_link(),

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
                  ChildSpec =
                      #{id => I,
                        start => {GsModule, start_link, [I, Args, []]},
                        restart => temporary,
                        shutdown => 5000,
                        type => worker},
                  {ok, P} = supervisor:start_child(Supervisor, ChildSpec),
                  {ok, M} = ddtrace:start_link(P, MonReg, [{tracer_mod, TracerMod}]),

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
                     Mon = mon_reg:mon_of(MonReg, SessionInitProc),
                     RD = ddtrace:subscribe_deadlocks(Mon),
                     
                     case OptDDMon of
                         false ->
                             R = gen_server:send_request(SessionInitProc, {SessionId, Session});
                         true -> 
                             R = ddmon:send_request_report(SessionInitProc, {SessionId, Session})
                     end,
                     
                     ReqIds1 = gen_server:reqids_add(R, {reply, SessionId, SessionInitProc, Mon, RD}, ReqIds),
                     gen_server:reqids_add(RD, {deadlock, SessionId, R}, ReqIds1)
             end,
    Reqs = lists:foldl(Folder, gen_server:reqids_new(), FScenario),

    InitIdSet = [SessionId || {SessionId, _} <- FScenario],
    Result = 
        try receive_responses(Reqs, InitIdSet, Timeout, Opts)
        catch E:EE ->
                {_, _, FSS1} = depidify(#{}, 0, FScenario),
                io:format(" SCENARIO:\n ~p\n\n\n\n\n\n", [FSS1]),
                io:format("Error while receiving responses: ~p:~p\n", [E, EE]),
                error(E)
        end,
    timer:sleep(500),
    
    Log = try tracer:finish(Tracer, [M || {_, {M, _P}} <- maps:to_list(ProcMap)])
          catch Eee:EEee ->
              io:format("\n\n\n\n\n\n\nError while finishing tracing: ~p:~p\n", [Eee, EEee]),
              []
          end,

    case proplists:get_value(stats_csv, Opts, false) of
        false -> ok;
        SLogFile ->
            file:write_file(SLogFile, [logging:log_stats(Log)])
    end,
    [begin
         logging:log_trace(InitTime, lists:sort(Log))
         %% logging:print_log_stats(Log)
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

    logging:log_result(Result),
    
    [ begin
          catch ddtrace:stop_tracer(M),
          unlink(M)
      end
      ||  {_, {M, _P}} <- maps:to_list(ProcMap)
    ],
    unlink(Supervisor),
    exit(Supervisor, shutdown),

    [ io:format("BAD RESULT ~p: ~p~n\n\n\n\n\n\n", [SessionId, Mode])
     || {SessionId, Mode} <- ets:tab2list(get(status_ets)),
        Mode =/= reply, 
        Mode =/= confirmed_deadlock,
        (OptDDMon orelse Mode =/= deadlock),
        true
    ],
    ets:delete(get(status_ets)),
    
    case Result of
        {timeout, _} ->
            {_, _, FSS} = depidify(#{}, 0, FScenario),
            io:format("TIMEOUT FOR SCENARIO\n\n~p\n\n\n\n\n\n", [FSS]);
        _ -> ok
    end,

    {Log, Result}.

%% Wait for all sessions to terminate, or timeout
receive_responses(Reqs0, Remaining, Time, Opts) ->
    put(status_ets, ets:new(status_ets, [])),
    
    {Result, _Reqs1} = receive_responses(Reqs0, Remaining, Time, [], Opts),
    %% gen_server:wait_response(_Reqs1, 2000, true),

    Result.
receive_responses(Reqs, [], _, Deadlocks, _Opts) ->
    case Deadlocks of
        [] -> {ok, Reqs};
        _ -> {{deadlock, Deadlocks}, Reqs}
    end;
receive_responses(Reqs0, Remaining, Time, Deadlocks, Opts) when Time < 0 ->
    receive_responses(Reqs0, Remaining, 0, Deadlocks, Opts);
receive_responses(Reqs0, Remaining, Time, Deadlocks, Opts) ->
    Res = time_ms(fun() -> gen_server:wait_response(Reqs0, Time, true) end),
    case Res of
        {TimeD, {{error, {_Err, _Pid}}, {reply, _SessionId, _P, _M, _ReqDead}, Reqs1}} ->
            receive_responses(Reqs1, Remaining, Time - TimeD, Deadlocks, Opts);
        {TimeD, {{error, {normal, _Pid}}, {deadlock, _SessionId, _}, _Reqs1}} ->
            receive_responses(Reqs0, Remaining, Time - TimeD, Deadlocks, Opts);
        %% {TimeD, {{error, _E}, _L, Reqs1}} ->
        %%     receive_responses(Reqs1, Remaining, Time - TimeD, Deadlocks);
            
        {_, no_request} when Deadlocks =:= [] ->
            {ok, Reqs0};
        {_, no_request} ->
            {{deadlock, Deadlocks}, Reqs0};
        {_, timeout} ->
            {{timeout, Remaining}, Reqs0};
        {TimeD, {{reply, {'$ddmon_deadlock_spread', _DL}}, {reply, SessionId, _P, _M, _ReqDead}, Reqs1}} ->
            Remaining1 = reduce_remaining(Remaining, SessionId, ddmon_deadlock, Opts),
            receive_responses(Reqs1, Remaining1, Time - TimeD, Deadlocks, Opts);
        {TimeD, {{reply, _R}, {reply, SessionId, _P, M, _ReqDead}, Reqs1}} ->
            ddtrace:unsubscribe_deadlocks(M),
            Remaining1 = reduce_remaining(Remaining, SessionId, reply, Opts),
            receive_responses(Reqs1, Remaining1, Time - TimeD, Deadlocks, Opts);
        {TimeD, {{reply, {deadlock, DL}}, {deadlock, SessionId, _ReqReply}, Reqs1}} ->
            Remaining1 = reduce_remaining(Remaining, SessionId, deadlock, Opts),
            receive_responses(Reqs1, Remaining1, Time - TimeD, [DL | Deadlocks], Opts)
    end.

reduce_remaining(Remaining, SessionId, Mode, Opts) ->
    OptDDMon = proplists:get_value(ddmon, Opts, false),
    PrevMode = case ets:lookup(get(status_ets), SessionId) of [] -> none; [{_, M}] -> M end,
    NewMode = 
        case {PrevMode, Mode} of
            {none, _} ->
                Mode;
            {deadlock, ddmon_deadlock} ->
                confirmed_deadlock;
            {deadlock, reply} ->
                error(reply_deadlock);
            {deadlock, deadlock} ->
                deadlock;
                
            {ddmon_deadlock, deadlock} ->
                confirmed_deadlock;
            {ddmon_deadlock, reply} ->
                error(ddmon_deadlock_reply);
            {ddmon_deadlock, ddmon_deadlock} ->
                error(dup_deadlock);

            {reply, reply} ->
                error(dup_reply);
            {reply, deadlock} ->
                reply;
            {reply, ddmon_deadlock} ->
                error(dup_reply_deadlock);
            
            {confirmed_deadlock, reply} ->
                error(confirmed_deadlock_reply);
            {confirmed_deadlock, deadlock} ->
                confirmed_deadlock;
            {confirmed_deadlock, ddmon_deadlock} ->
                error(confirmed_deadlock_ddmon);

            _ -> error({wtf, PrevMode, Mode})
        end,
    ets:insert(get(status_ets), {SessionId, NewMode}),
    
    case NewMode of
        reply -> Remaining -- [SessionId];
        confirmed_deadlock -> Remaining -- [SessionId];
        deadlock when not OptDDMon -> Remaining -- [SessionId];
        _ ->
             Remaining
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

%% Run operation and measure time in ms
time_ms(Fun) ->
    Start = erlang:monotonic_time(),
    Res = Fun(),
    End = erlang:monotonic_time(),
    TimeMs = erlang:convert_time_unit(End - Start, native, millisecond),
    {TimeMs, Res}.


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

run_bench(Bench, Opts) ->
    %% io:format("\n\nGEN: ~p\n\n", [Scens]),
    run_many(Bench, Opts).

-define(GRID_WIDTH, 39).
grid_printer() ->
    receive {workers, Ws, MaxSize} -> grid_printer(Ws, MaxSize) end.
grid_printer(Ws, MaxSize) ->
    print_legend(Ws),
    State = maps:from_list([{W, init} || W <- Ws]),
    %% print_size(0, MaxSize),
    print_grid(State),
    grid_printer_loop(State, 0, MaxSize).
print_grid(State) ->
    S = "[" ++
        print_grid(lists:sort(maps:to_list(State)), 0) ++
        " ]\n",
    io:format("~s", [S]).

print_legend(Ws) ->
    Legend =
        [ begin
              ansi_color:render([blob_format(Blob), " ", blob_descr(Blob)])
          end
          || Blob <- [init, woke, busy, done, dead, down, time]
        ],
    S = string:join(Legend, ansi_color:render({bold, " | "})),
    Headline = ansi_color:render([{bold, "*** Running experiments on "},
                                  {[bold, italic], integer_to_list(length(Ws))},
                                  {bold, " scenarios. Experiment status legend:\n"}]),
    io:format("~s ~s\n", [Headline, S]).

blob_format(Blob) ->
    case Blob of
        init -> {[black_l, bold], "."};
        woke -> {[yellow, bold, dim], "o"};
        busy -> {[yellow_l, bold], "O"};
        done -> {[green, bold], "@"};
        dead -> {[violet, bold, dim], "D"};
        down -> {[red, blink, bold], "!"};
        time -> {[blue_l, bold], "T"}
    end.

blob_descr(Blob) ->
    case Blob of
        init -> "Waiting";
        woke -> "Preparing";
        busy -> "Working";
        done -> "Success";
        dead -> "Deadlock";
        down -> "Crash";
        time -> "Timeout"
    end.

print_grid([], _) -> "";
print_grid(Ws, ?GRID_WIDTH) ->
    "\n " ++
        print_grid(Ws, 0);
print_grid([{_W, S}|Ws], I) ->
    io_lib:format(" ~s", [ansi_color:render(blob_format(S))]) ++
        print_grid(Ws, I + 1).

clean_grid(Ws) ->
    Height = ((maps:size(Ws) - 1) div ?GRID_WIDTH)
        + (_CompensateMinus1 = 1)
        %% + (_SizeRow = 1)
        + (_StupidComma = 0),
    io:format("\e[~pA\r", [Height]).

grid_printer_loop(State, Size, MaxSize) ->
    receive
        {update, W, WSize, S} ->
            State1 = State#{W => S},
            Size1 = Size + WSize,
            clean_grid(State1),
            %% print_size(Size1, MaxSize),
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
                    io:format(standard_error, "Worker ~p crashed: ~p\n\n\n\n\n\n", [W, E]),
                    get_results(Current -- [{W, Size}], Queue, CurrSize - Size, Max, Printer, Acc);
                {success, _Ref, W, Type, Size, Stats, Res} ->
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
                        Printer ! {update, self(), -Size, case Result of ok -> done; {deadlock, _} -> dead; {timeout, _} -> time end},
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

    %% Stats = get_results(scenario_gen:shuffle(Workers), MAX_SIZE, Printer),
    Stats = get_results(Workers, MAX_SIZE, Printer),

    Printer ! {self(), finito},
    receive {Printer, ok} -> ok end,

    %% Appending = filelib:is_file(proplists:get_value(stats_csv, Opts, "")),
    LogFile = proplists:get_value(stats_csv, Opts, <<"/dev/stdout">>),
    io:format("Writing log to: ~s\n", [LogFile]),

    {ok, Log} = file:open(binary:bin_to_list(LogFile), [write]),

    Cols = [run,time,type,size,total,sent,recv,inits,queries,replies,heralds,probes,success,deadlock],
    ColsFormat = string:join([atom_to_list(C) || C <- Cols], ",") ++ "\n",
    ValsFormat = string:join(["~p" || _ <- Cols], ",") ++ "\n",
    io:format(Log, ColsFormat, []),
    [begin 
         Vals = [ case Col of 
                      success ->  case Result of {timeout, _} -> 2; {deadlock, _} -> 1; ok -> 0 end;
                      _ -> maps:get(Col, ValMap#{type=>Type, size=>Size, run=>I})
                  end 
                  || Col <- Cols
                ],
         io:format(Log, ValsFormat, Vals)
     end
     || {I, {Type, Size, ValMap, Result}} <- lists:enumerate(Stats)
    ],
    case LogFile of
        <<"/dev/stdout">> -> ok;
        _ -> file:close(Log)
    end.
