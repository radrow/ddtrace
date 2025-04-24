-module(logging).

-include("dlstalk.hrl").

-export([conf/1, mk_ets/0, delete/0, remember/2, remember/3]).

-export([type/1]).

-export([ log_terminate/0, log_deadlock/1
        , log_scenario/2
        , log_timeout/0
        , log_trace/1, log_trace/2
        , print_log_stats/1, log_stats/1, trace_csv/2
        ]).

-define(LOG_SILENT, '$logging_silent').
-define(LOG_TIMESTAMP, '$logging_timestamp').
-define(KNOWN_ETS, '$logging_known').
-define(FRESH_ETS, '$logging_fresh').

-define(KNOWN, get(?KNOWN_ETS)).
-define(FRESH, get(?FRESH_ETS)).


mk_ets() ->
    Known = ets:new(?KNOWN_ETS, [public]),
    Fresh = ets:new(?FRESH_ETS, [public]),

    put(?KNOWN_ETS, Known),
    put(?FRESH_ETS, Fresh),
    {Known, Fresh}.

delete() ->
    ets:delete(get(?KNOWN_ETS)),
    ets:delete(get(?FRESH_ETS)).

conf(LogConf) ->
    case proplists:get_value(logging_ets_known, LogConf) of
        undefined -> ok;
        KnRef -> put(?KNOWN_ETS, KnRef)
    end,
    case proplists:get_value(logging_ets_fresh, LogConf) of
        undefined -> ok;
        FrRef -> put(?FRESH_ETS, FrRef)
    end,
    put(?LOG_SILENT, proplists:get_value(silent, LogConf)),
    put(?LOG_TIMESTAMP, proplists:get_value(log_timestamp, LogConf, true)),
    put(?LOG_INDENT_SIZE, proplists:get_value(indent, LogConf, 4)),
    ok.


last_i(Prefix) ->
    case ets:lookup(?FRESH, Prefix) of
        [{_, I}] -> I;
        _ -> 0
    end.

fresh_i(Prefix) ->
    Idx = last_i(Prefix),
    ets:insert(?FRESH, {Prefix, Idx + 1}),
    Idx.

remember(Thing, Type, Idx) ->
    ets:insert(?KNOWN, {Thing, {Type, Idx}}).

remember(Thing, Type) ->
    Idx = fresh_i(Type),
    remember(Thing, Type, Idx),
    Idx.

index(Thing, Type) ->
    case lists:search(
           fun({_, {Type0, _}}) -> Type =:= Type0 end,
           ets:lookup(?KNOWN, Thing)
          ) of
        false ->
            remember(Thing, Type);
        {value, {_, {_, Idx}}} ->
            Idx
    end.

known(Thing) ->
    case ets:lookup(?KNOWN, Thing) of
        [{_, Info}] -> Info;
        _ ->
            Idx = remember(Thing, x),
            {x, Idx}
    end.

index(Thing) ->
    {_, Idx} = known(Thing),
    Idx.

type(Thing) ->
    {Type, _} = known(Thing),
    Type.

name(Thing) ->
    {Type, Idx} = known(Thing),
    [c_type(Type), integer_to_list(Idx)].

name(Thing, Type) ->
    Idx = index(Thing, Type),
    [c_type(Type), integer_to_list(Idx)].

c_type(A) when is_atom(A) ->
    atom_to_list(A);
c_type({A, _SubId}) when is_atom(A) andalso is_integer(_SubId) ->
    atom_to_list(A).


c_mon(Mon) when is_pid(Mon) ->
    {cyan, name(Mon)};
c_mon(Mon) when is_atom(Mon) ->
    {cyan, atom_to_list(Mon)}.

c_proc(Proc) when is_pid(Proc) ->
    {violet, name(Proc)};
c_proc(Proc) when is_atom(Proc) ->
    {violet, atom_to_list(Proc)}.

c_proc(Proc, SubId) ->
    [c_proc(Proc), "(", c_thing(SubId), ")"].
c_mon(Mon, SubId) ->
    [c_mon(Mon), "(", c_thing(SubId), ")"].

c_init(Pid) when is_pid(Pid) ->
    {[blue, bold, invert], name(Pid)}.

c_who(Thing) ->
    case type(Thing) of
        'M' -> c_mon(Thing);
        'P' -> c_proc(Thing);
        'I' -> c_init(Thing);
        {'M', N} ->
            c_mon(Thing, N);
        {'P', N} -> c_proc(Thing, N);
        _ -> c_thing(Thing)
    end.

c_query() ->
    {blue_l, "Q"}.

c_query(Msg) ->
    [c_query(), "(", c_msg(Msg), ")"].

c_reply() ->
    {green_l, "R"}.

c_reply(Msg) ->
    [c_reply(), "(", c_msg(Msg), ")"].

c_probe(Probe) ->
    {yellow, name(Probe, 'P')}.

%% c_query() ->
%%     {blue_l, "query"}.

%% c_query(Msg) ->
%%     [c_query(), "(", c_msg(Msg), ")"].

%% c_reply() ->
%%     {green_l, "reply"}.

%% c_reply(Msg) ->
%%     [c_reply(), "(", c_msg(Msg), ")"].

%% c_probe(Probe) ->
%%     {yellow, name(Probe, 'probe_')}.


c_terminate() ->
    {[green_l, bold, underline, invert], "### TERMINATED ###"}.

c_deadlock(N) ->
    {[red_l, bold, underline, invert], "### DEADLOCKS (" ++ integer_to_list(N) ++ ") ###"}.

c_timeout() ->
    {[white, bold, underline, invert], "### TIMEOUT ###"}.


c_msg(Msg) when is_tuple(Msg) andalso size(Msg) > 0 ->
    c_msg(element(1, Msg));
c_msg(?YOU_DIED) ->
    c_deadlock_msg();
c_msg(Msg) ->
    c_thing(Msg).

c_deadlock_msg() ->
    {[red, dim], "D"}.

c_thing(Thing) ->
    {[white_l, bold, italic], lists:flatten(io_lib:format("~p", [Thing]))}.

print(none) -> ok;
print(Span) ->
    case get(?LOG_SILENT) of
        true -> ok;
        undefined -> io:format("~s\n", [ansi_color:render(Span)])
    end.


c_by(Who, Span) ->
    [ c_indent(Who), c_who(Who), ":\t " | Span].

c_indent() ->
    [$\t || _ <- lists:seq(1, get(?LOG_INDENT_SIZE))].
c_indent(I) when is_integer(I) ->
    case get(?LOG_INDENT_SIZE) of
        0 -> "";
        _ ->
            [c_indent() ++ "| " || _ <- lists:seq(1, I)]
    end;
c_indent(Thing) ->
    case get(?LOG_INDENT_SIZE) of
        0 -> "";
        _ ->
            "| " ++ c_indent(index(Thing))
    end.

log_scenario(Scenario, Time) ->
    print({italic, io_lib:format("Timeout: ~pms ", [Time])}),
    print([ {italic, "Sessions: "}
          , [ [ c_thing(SessionId), " "
              ]
              || {SessionId, _Sc} <- Scenario
            ]
          ]).

log_terminate() ->
    print(c_terminate()).

log_deadlock(N) ->
    print(c_deadlock(N)).

log_timeout() ->
    print(c_timeout()).

c_release(Pid) ->
    [{dim, "release: "}, c_who(Pid)].

c_from({Tag, From, _Msg}) when is_atom(Tag) ->
    c_who(From);
c_from({Tag, _Msg}) when is_atom(Tag) ->
    "".

c_state(unlocked) ->
    {[green, bold, invert], " UNLOCK "};
c_state({locked, On}) ->
    [{[red_l, bold, invert], " LOCK "}, " (", c_probe(On), ")"];
c_state({deadlocked, foreign}) ->
    {[red, bold, underline, dim], "foreign deadlock"};
c_state({deadlocked, [First|DL]}) ->
    [ {[red, bold, underline, invert], "### DEADLOCK ###\t"}
    , "("
    , c_who(First)
    , [ [" -> ", c_who(Who)] || Who <- DL]
    , ")"
    ].

c_ev_data({query, _From, Msg}) ->
    [c_query(Msg)];
c_ev_data({query, Msg}) ->
    c_query(Msg);
c_ev_data({reply, _From, Msg}) ->
    [c_reply(Msg)];
c_ev_data({reply, Msg}) ->
    c_reply(Msg);
c_ev_data({proc_query, To, Msg}) ->
    [c_who(To), " ?! ", c_query(Msg)];
c_ev_data({proc_reply, Msg}) ->
    ["?! ", c_reply(Msg)];
c_ev_data({probe, Probe}) ->
    c_probe(Probe);
c_ev_data({release, Pid}) ->
    c_release(Pid).

c_event({recv, EvData}) ->
    [c_from(EvData), " ? ", c_ev_data(EvData)];
c_event({send, To, EvData}) ->
    [c_who(To), " ! ", c_ev_data(EvData)];
c_event({pick, Event}) ->
    ["%[ ", c_ev_data(Event), " ]"];
c_event({wait, Time}) ->
    {[italic, dim], io_lib:format("waiting ~pms", [Time])};
c_event({state, State}) ->
    ["=> ", c_state(State)];
c_event({unhandled, T}) ->
    ["unhandled trace: ", c_thing(T)].

class_who(E) ->
    case type(E) of
        A when is_atom(A) ->
            A;
        {A, _I} when is_atom(A) -> A;
        _X -> unknown
    end.

ev_class({recv, E}) ->
    [recv, comm | ev_class(E)];
ev_class({send, To, E}) ->
    [{to, class_who(To)}, send, comm | ev_class(E)];
ev_class({pick, E}) ->
    [pick | ev_class(E)];
ev_class(E) when element(1, E) =:= query ->
    [query];
ev_class(E) when element(1, E) =:= reply ->
    [reply];
ev_class(E) when element(1, E) =:= proc_query ->
    [query, proc];
ev_class(E) when element(1, E) =:= proc_reply ->
    [reply, proc];
ev_class(E) when element(1, E) =:= probe ->
    [probe];
ev_class({{_Time, _TimeIdx}, Who, E}) -> % TODO distinguish trace events...
    [{by, class_who(Who)} | ev_class(E)];
ev_class({state, S}) ->
    [state | ev_class(S)];
ev_class({deadlocked, _}) ->
    [deadlocked];
ev_class({locked, _}) ->
    [locked];
ev_class(unlocked) ->
    [unlocked];
ev_class(E) when element(1, E) =:= wait;
                 element(1, E) =:= release;
                 element(1, E) =:= state
                 ->
    [misc];
ev_class(_) ->
    [].

log_stats(Log0) ->
    Log = [L || L <- Log0, L =/= ignore],
    Classes = lists:map(fun ev_class/1, Log),

    Count = fun FCount(L) when is_list(L) ->
                    CF = lists:filter(fun(Cs) -> lists:all(fun(C) -> lists:member(C, Cs) end, L) end, Classes),
                    length(CF);
                FCount(A) when is_atom(A) ->
                    FCount([A])
            end,

    #{total => length(Classes),
      sent => Count([send, comm]),
      inits => Count([send, comm, {by, 'I'}]) + Count([send, comm, {to, 'I'}]),
      mon_mon => Count([send, comm, {by, 'M'}, {to, 'M'}]),
      mon_proc => Count([send, comm, {by, 'M'}, {to, 'P'}]),
      proc_mon => Count([send, comm, {by, 'P'}, {to, 'M'}]),
      proc_proc => Count([send, comm, {by, 'P'}, {to, 'P'}]),
      queries => Count([send, comm, query]),
      replies => Count([send, comm, reply]),
      probes => Count([send, comm, probe]),
      locks => Count([state, locked]),
      unlocks => Count([state, unlocked]),
      deadlocks => Count([state, deadlocked]),
      picks => Count([pick]),
      time => case lists:last(Log) of {{LTime, _}, _, _} -> LTime end -
          case hd(Log) of {{FTime, _}, _, _} -> FTime end
     }.


print_log_stats(Log) ->
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
      unlocks := Unlocks,
      locks := Locks,
      deadlocks := Deadlocks,
      picks := Picks,
      time := Time
     } = log_stats(Log),

    print(io_lib:format("Registered ~p events:\n"
                        "\t~p\tmessages sent\n"
                        "\t~p\tqueries\n"
                        "\t~p\treplies\n"
                        "\t~p\tprobes\n"
                        "\t~p\tmque picks\n"
                        "Directions:\n"
                        "\t~p\tinvolving Init\n"
                        "\t~p\tMon -> Mon\n"
                        "\t~p\tMon -> Proc\n"
                        "\t~p\tProc -> Mon\n"
                        "\t~p\tProc -> Proc\n"
                        "State changes:\n"
                        "\t~p\tunlocks\n"
                        "\t~p\tlocks\n"
                        "\t~p\tdeadlocks\n"
                        "Time: ~p\n",
                        [Total,
                         Sent, Queries, Replies, Probes, Picks,
                         Inits, MonMon, ProcMon, MonProc, ProcProc,
                         Unlocks, Locks, Deadlocks, Time])).


trace_csv_header() ->
    {"timestamp","who_type","who", "event_type", "other_type", "other", "data_type", "data"}.


ev_data_csv({query, From, Msg}) ->
    #{data_type=>query, other=>From, data=>Msg};
ev_data_csv({query, Msg}) ->
    #{data_type=>query, data=>Msg};

ev_data_csv({reply, From, {?YOU_DIED, DL}}) ->
    #{data_type=>dl_notif, other=>From, data=>[index(X) || X <- DL]};
ev_data_csv({reply, {?YOU_DIED, DL}}) ->
    #{data_type=>dl_notif, data=>[index(X) || X <- DL]};

ev_data_csv({reply, From, Msg}) ->
    #{data_type=>reply, other=>From, data=>Msg};
ev_data_csv({reply, Msg}) ->
    #{data_type=>reply, data=>Msg};

ev_data_csv({proc_query, To, Msg}) ->
    #{data_type=>proc_query, other=>To, data=>Msg};
ev_data_csv({proc_reply, Msg}) ->
    #{data_type=>proc_reply, data=>Msg};

ev_data_csv({probe, Probe}) ->
    #{data_type=>probe, data=>Probe};

ev_data_csv({release, Pid}) ->
    #{data_type=>release, data=>Pid};
ev_data_csv(unlocked) ->
    #{data_type=>unlocked};
ev_data_csv({deadlocked, foreign}) ->
    #{data_type=>deadlocked, data=>foreign};
ev_data_csv({deadlocked, L}) ->
    #{data_type=>deadlocked, data=>[index(X) || X <- L]};
ev_data_csv({locked, On}) ->
    #{data_type=>locked, other=>On}.

ev_csv({recv, EvData}) ->
    maps:merge(#{ev_type=>recv}, ev_data_csv(EvData));
ev_csv({send, To, EvData}) ->
    maps:merge(#{ev_type=>send,other=>To}, ev_data_csv(EvData));
ev_csv({pick, Event}) ->
    maps:merge(#{ev_type=>pick}, ev_data_csv(Event));
ev_csv({wait, Time}) ->
    #{ev_type=>wait, data=>Time};
ev_csv({state, State}) ->
    maps:merge(#{ev_type=>state}, ev_data_csv(State));
ev_csv(_) -> nil.

trace_csv(InitT, Trace) ->
    Rows = [ begin
                 Timestamp = erlang:convert_time_unit(Time - InitT, native, microsecond),
                 {WhoType, WhoIdx} = known(Who),
                 {OtherType, OtherIdx} =
                     case maps:get(other, EvCsv, nil) of
                         nil -> {nil, nil};
                         Pid -> known(Pid)
                     end,
                 { integer_to_list(Timestamp)
                 , case WhoType of
                       {A_WT, _} when is_atom(A_WT) -> atom_to_list(A_WT);
                       A_WT when is_atom(A_WT) -> atom_to_list(A_WT)
                   end
                 , case WhoType of
                       {A_W, I_W} when is_atom(A_W), is_integer(I_W) -> integer_to_list(WhoIdx) ++ "_" ++ integer_to_list(I_W);
                       A_W when is_atom(A_W) -> integer_to_list(WhoIdx)
                   end
                 , atom_to_list(maps:get(ev_type, EvCsv))
                 , case OtherType of
                       nil -> "";
                       {A_OT, _} when is_atom(A_OT) -> atom_to_list(A_OT);
                       A_OT when is_atom(A_OT) -> atom_to_list(A_OT)
                   end
                 , case OtherType of
                       nil -> "";
                       {A_T, I_T} when is_atom(A_T), is_integer(I_T) -> integer_to_list(OtherIdx) ++ "_" ++ integer_to_list(I_T);
                       A_T when is_atom(A_T) -> integer_to_list(OtherIdx)
                   end
                 , atom_to_list(maps:get(data_type, EvCsv, ''))
                 , case maps:get(data, EvCsv, nil) of
                       nil -> "";
                       D when is_atom(D); is_integer(D); is_list(D) ->
                           "\"" ++
                               lists:flatten(string:replace(lists:flatten(io_lib:format("~w", [D])), "\"", "\"\"", all))
                               ++ "\"";
                       _D -> "msg"
                   end
                 }
             end
             || {{Time, _TimeIdx}, Who, What} <- Trace,
                EvCsv <- [ev_csv(What)], is_map(EvCsv)
           ],

    Lines = [ string:join(tuple_to_list(T), ",") ++ "\n" || T <- [trace_csv_header()|Rows] ],
    lists:flatten(Lines).


c_timestamp(InitT, {T, _TimeIdx}) ->
    MicrosTotal = erlang:convert_time_unit(T - InitT, native, microsecond),
    Micros = MicrosTotal rem 1000,
    MillisTotal = MicrosTotal div 1000,
    Millis = MillisTotal rem 1000,
    SecondsTotal = Millis div 1000,
    Seconds = SecondsTotal rem 60,
    Minutes = SecondsTotal div 60,
    Style = [italic, dim],
    case Minutes of
        0 -> [{Style, io_lib:format("~2..0w:~3..0w:~3..0w", [Seconds, Millis, Micros])}];
        _ -> [{Style, io_lib:format("~w~2..0w:~3..0w:~3..0w", [Minutes,Seconds, Millis, Micros])}]
    end.

c_trace(InitT, Time, Who, What) ->
    [ case get(?LOG_TIMESTAMP) of true -> [c_timestamp(InitT, Time), "\t"]; _ -> "" end
    , c_by(Who, c_event(What))
    ].

log_trace(T) ->
    log_trace(0, T).

log_trace(_InitT, ignore) ->
    ok;
log_trace(InitT, {Time, Who, What}) ->
    print(c_trace(InitT, Time, Who, What));

log_trace(InitT, Trace) when is_list(Trace) ->
    [ log_trace(InitT, T) || T <- Trace, T =/= ignore ],
    ok.
