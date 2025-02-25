-module(logging).

-include("dlstalk.hrl").

-export([conf/1, remember/2, remember/3]).

-export([ log_terminate/0
        , log_scenario/2
        , log_timeout/0
        , log_trace/1, log_trace/2
        ]).

-define(KNOWN, '$logging_known').
-define(FRESH, '$logging_fresh').


conf(LogConf) ->
    case ets:whereis(?KNOWN) of
        undefined -> ets:new(?KNOWN, [named_table, public]);
        _ -> ok
    end,

    case ets:whereis(?FRESH) of
        undefined -> ets:new(?FRESH, [named_table, public]);
        _ -> ok
    end,

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
    {blue_l, "query"}.

c_query(Msg) ->
    [c_query(), "(", c_msg(Msg), ")"].

c_reply() ->
    {green_l, "reply"}.

c_reply(Msg) ->
    [c_reply(), "(", c_msg(Msg), ")"].

c_probe(Probe) ->
    {yellow, name(Probe, 'probe_')}.


c_terminate() ->
    {[green_l, bold, underline, invert], "### TERMINATED ###"}.

c_timeout() ->
    {[white, bold, underline, invert], "### TIMEOUT ###"}.

c_msg(Msg) when is_tuple(Msg) andalso size(Msg) > 0 ->
    c_msg(element(1, Msg));
c_msg(Msg) ->
    c_thing(Msg).

c_thing(Thing) ->
    {[white_l, bold, italic], lists:flatten(io_lib:format("~p", [Thing]))}.

print(none) -> ok;
print(Span) ->
    io:format("~s\n", [ansi_color:render(Span)]).


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
    [ c_timestamp(InitT, Time), "\t"
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
