-module(logging).

-include_lib("ddtrace/include/ddtrace.hrl").

-export([conf/1, mk_ets/0, delete/0, remember/2, remember/3]).

-export([type/1]).

-export([ log_terminate/0, log_deadlocks/1, log_scenario/2, log_timeout/1
        , log_result/1
        , log_trace/1, log_trace/2, log_stats/1
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

known(Thing, Type) ->
    case ets:lookup(?KNOWN, Thing) of
        [{_, Info}] -> Info;
        _ ->
            Idx = remember(Thing, Type),
            {Type, Idx}
    end.
known(Thing) ->
    known(Thing, x).

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
    {[blue, bold], name(Pid)}.

c_who({global, Term}) ->
    case global:whereis_name(Term) of
        undefined -> c_thing(Term);
        Pid -> c_who(Pid)
    end;
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
    [c_query(), "(", c_reqid(Msg), ")"].

c_reply() ->
    {green_l, "R"}.

c_reply(Msg) ->
    [c_reply(), "(", c_reqid(Msg), ")"].

c_probe(Probe) ->
    {yellow, name(Probe, 'p')}.


c_terminate() ->
    {[green_l, bold, underline, invert], "### TERMINATED ###"}.

c_deadlocks(DLs) ->
    [ {[red_l, bold, underline, invert], "### DEADLOCKS (" ++ integer_to_list(length(DLs)) ++ ") ###"}
    , case DLs of
          [] -> "";
          [DL|_] -> ["\t", c_lock_list(DL)]
      end
    ].

c_timeout(Remaining) ->
    [ {[white, bold, underline, invert], "### TIMEOUT ###"},
      "  ",
      [c_who(SessionId) || SessionId <- Remaining]
    ].

c_reqid(ReqId) ->
    I = index(ReqId, reqid),
    {[white_l, bold, italic], "i" ++ integer_to_list(I)}.

c_thing(Thing) ->
    {[white_l, bold, italic], lists:flatten(io_lib:format("~p", [Thing]))}.

print(none) -> ok;
print(Span) ->
    case get(?LOG_SILENT) of
        true -> ok;
        undefined -> io:format("~s\n", [ansi_color:render(Span)])
    end.


c_by(Who, Span) ->
    [ c_indent(Who), c_who(Who), ": " | Span].

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

log_result(ok) ->
    log_terminate();
log_result({deadlock, DLs}) ->
    log_deadlocks(DLs);
log_result({timeout, Remaining}) ->
    log_timeout(Remaining).

log_terminate() ->
    print(c_terminate()).

log_deadlocks(DLs) ->
    print(c_deadlocks(DLs)).

log_timeout(Remaining) ->
    print(c_timeout(Remaining)).


c_lock_list({foreign, DL}) ->
    ["&", c_lock_list(DL)];
c_lock_list([]) ->
    "";
c_lock_list([First|L]) ->
    [ "("
    , c_who(First)
    , [ [" -> ", c_who(Who)] || Who <- L]
    , ")"
    ].

c_state(?synced) ->
    {[green, bold, invert], " S "};
c_state(?wait_proc(From, MsgInfo)) -> 
    [{[yellow, bold, invert], " P "}, "(", c_who(From), " @ ", c_msg_info(MsgInfo), ")"];
c_state(?wait_mon(MsgInfo)) -> 
    [{[violet, bold, invert], " N "}, " (", c_msg_info(MsgInfo), ")"];
c_state(?wait_mon_proc(MsgInfoMon, FromProc, MsgInfoProc)) -> 
    [{[violet, bold, invert], " N "}, " (", c_msg_info(MsgInfoMon), 
     "|", c_who(FromProc), " @ ", c_msg_info(MsgInfoProc),
     ")"].

c_instate({deadlock, DL}) ->
    [ {[red, bold, invert, blink], " D "}, "(", c_lock_list(DL), ")"];
c_instate({lock, ReqId}) ->
    [ {[red, bold, invert], " L "}, "(", c_probe(ReqId), ")"];
c_instate(unlock) ->
    [ {[green, bold, invert], " U "} ];
c_instate({wait, Who}) ->
    [ {[yellow, bold, invert], " +W "}, "(", c_probe(Who), ")" ];
c_instate({unwait, Who}) ->
    [ {[cyan, bold, invert], " -W "}, "(", c_probe(Who), ")"].


c_msg_info(?QUERY_INFO(ReqId)) ->
    c_query(ReqId);
c_msg_info(?RESP_INFO(ReqId)) ->
    c_reply(ReqId).

c_event({send, {herald, To, MsgInfo}}) ->
    [c_who(To), " $! ", c_msg_info(MsgInfo)];
c_event({Kind, Ev, State}) when Kind =:= internal; Kind =:= cast ->
    ["[", c_state(State), "]\t", c_event(Ev)];
c_event(?RECV_INFO(MsgInfo)) ->
    ["? ", c_msg_info(MsgInfo)];
c_event(?SEND_INFO(To, MsgInfo)) ->
    [c_who(To), " ! ", c_msg_info(MsgInfo)];
c_event(?HERALD(From, MsgInfo)) ->
    [c_who(From), " $? ", c_msg_info(MsgInfo)];
c_event(?PROBE(Probe, _L)) ->
    ["? ", c_probe(Probe)];
c_event({enter, OldState, NewState}) ->
    ["[", c_state(OldState), "]\t=> ", c_state(NewState)];
c_event({'DOWN', _, process, _Pid, Reason}) ->
    [{[red, bold, invert], " DOWN "}, lists:flatten(io_lib:format("~p", [Reason]))];
c_event({info, Ev, _State}) when element(1, Ev) =:= trace_ts ->
    "trace";
c_event({{call, _}, _Ev, _State}) ->
    "call";
c_event(?DEADLOCK_PROP(DL)) ->
    [{[red, bold], " D "}, c_lock_list(DL)];
c_event({state, St}) ->
    ["~>" , c_instate(St)];
c_event(_T) ->
    silent.
    %% c_thing(T).

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
    case c_event(What) of
        silent -> [];
        F ->
            [ case get(?LOG_TIMESTAMP) of true -> [c_timestamp(InitT, Time), " "]; _ -> "" end
            , c_by(Who, F)
            ]
    end.


class_who(E) ->
    case type(E) of
        A when is_atom(A) ->
            A;
        {A, _I} when is_atom(A) -> A;
        _X -> unknown
    end.

ev_class({{Time, TimeX}, Who, Ev}) when is_integer(Time), is_integer(TimeX) ->
    [{by, class_who(Who)} | ev_class(Ev)];
ev_class({cast, Msg, _State}) ->
    ev_class(Msg);
ev_class({send, {herald, To, MsgInfo}}) ->
    [send, {to, class_who(To)}, herald | ev_class(MsgInfo)];
ev_class({Kind, _Ev, _State}) when Kind =:= internal; Kind =:= cast ->
    [];
ev_class(?RECV_INFO(MsgInfo)) ->
    [recv, trace | ev_class(MsgInfo)];
ev_class(?SEND_INFO(To, MsgInfo)) ->
    [send, {to, class_who(To)}, trace | ev_class(MsgInfo)];
ev_class(?QUERY_INFO(_)) ->
    [query];
ev_class(?RESP_INFO(_)) ->
    [reply];
ev_class(?HERALD(_From, MsgInfo)) ->
    [recv, herald | ev_class(MsgInfo)];
ev_class(?PROBE(_Probe, _L)) ->
    [recv, probe];
ev_class({enter, _OldState, _NewState}) ->
    [state];
ev_class({'DOWN', _, process, _Pid, _Reason}) ->
    [];
ev_class({info, Ev, _State}) when element(1, Ev) =:= trace_ts ->
    [];
ev_class({{call, _}, _Ev, _State}) ->
    [];
ev_class(?DEADLOCK_PROP(_DL)) ->
    [];
ev_class({state, St}) ->
    [state | ev_class(St)];
ev_class({deadlock, _}) ->
    [deadlocked];
ev_class({locked, _}) ->
    [locked];
ev_class(unlocked) ->
    [unlocked];
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
      sent => Count([send, trace]),
      recv => Count([recv, trace]),
      inits => Count([send, trace, {by, 'I'}]) + Count([send, trace, {to, 'I'}]),
      queries => Count([recv, trace, query]),
      replies => Count([recv, trace, reply]),
      heralds => Count([recv, herald]),
      probes => Count([recv, probe]),
      locks => Count([state, locked]),
      unlocks => Count([state, unlocked]),
      deadlock => Count([state, deadlocked]),
      time => 
          case lists:last(Log) of {{LTime, _}, _, _} -> LTime end -
          case hd(Log) of {{FTime, _}, _, _} -> FTime end
     }.

log_trace(T) ->
    log_trace(0, T).

log_trace(_InitT, ignore) ->
    ok;
log_trace(InitT, {Time, Who, What}) ->
    print(c_trace(InitT, Time, Who, What));

log_trace(InitT, Trace) when is_list(Trace) ->
    [ log_trace(InitT, T) || T <- Trace, T =/= ignore ],
    ok.
