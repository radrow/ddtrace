-module(scenario_gen).

-export([shuffle/1]).
-export([generate/1, generate/2]).

-export([ loop/2
        , envelope/2
        , braid/2
        , lurker/2
        , tree/2
        ]).

generate(G) ->
    generate(G, #{}).
generate(G, Opts) ->
    generate(G, 0, Opts).
generate([], _I, _Opts) ->
    [];
generate([{Fun, Name, {Size, Args}}|G], I, Opts) when is_integer(Size) ->
    {Feed, NextI} =
        case maps:get(mode, Opts, sep) of
            sep -> {lists:seq(I, I + Size), I + Size};
            overlap ->
                II = max(0, I - rand:uniform(Size)),
                {lists:seq(II, II + Size), II + Size}
        end,
    generate([{Fun, Name, {Feed, Args}}|G], NextI, Opts);
generate([{Fun, Name, {Feed, Args}}|G], I, Opts) when is_list(Feed) ->
    apply(?MODULE, Fun, [Name, {Feed, Args}]) ++ generate(G, I, Opts);
generate([{Fun, Name, Args}|G], I, Opts) when not is_tuple(Args) ->
    generate([{Fun, Name, {Args, []}}|G], I, Opts).

%% Plain looping deadlock
loop(Name, {[First|Feed], Opts}) ->
    Delay = proplists:get_value(delay, Opts, 0),
    Wrap = rand:uniform(length(Feed)),
    [ { Name
      , {First, Feed ++ [{wait, Delay}, Wrap]}
      }
    ].

%% Sequence of threads; each eventually locks on the next one in line
envelope(Name, {Feed, Opts}) ->
    SplitRate = proplists:get_value(split_rate, Opts, 0.2),
    Splits = random_splits(Feed, SplitRate),
    Splits0 = case proplists:get_value(min_len, Opts) of
                  undefined -> Splits;
                  N -> stretch_splits(Splits, N)
              end,
    %% io:format("SPLITS: ~p\n\n", [Splits0]),
    gen_envelope(Name, Splits0, Splits0, Opts).

gen_envelope(_Name, [], _, _Opts) ->
    [];
gen_envelope(Name, [[Init|Last]], [First|_], Opts) ->
    Loop = rand:uniform() > proplists:get_value(safe, Opts, 0.5),
    [{ {envlp, Name, Init},
       { Init,
         [{wait, 0, 10} | Last] ++
             case Loop of
                 true ->
                     [lists:nth(
                        rand:uniform(
                          min(case proplists:get_value(target, Opts) of
                                  undefined -> length(First);
                                  Target -> max(1, length(First) - Target)
                              end,
                              length(First)
                             )),
                        %% min(case Target of undefined -> length(First); _ -> length(First) - Target end,
                        %%     length(First)
                        %%    )),
                        First
                       )];
                 false -> {wait, 10, 20}
             end
       }
     }];
gen_envelope(Name, [[Init|Nth], Next | Rest], All, Opts) ->
    [ { {envlp, Name, Init},
        { Init,
          [{wait, 0, 40} | Nth] ++ [lists:nth(
                    rand:uniform(
                      min(case proplists:get_value(target, Opts) of
                              undefined -> length(Next);
                              Target -> max(1, length(Next) - Target) end,
                          length(Next)
                         )),
                    Next
                   )]
        }
      }
    | gen_envelope(Name, [Next|Rest], All, Opts)
    ].


%% Sequence of threads; each eventually locks on any other
braid(Name, {Feed, Opts}) ->
    Splits = random_splits(Feed),
    Splits0 = case proplists:get_value(min_len, Opts) of
                  undefined -> Splits;
                  N -> stretch_splits(Splits, N)
              end,
    gen_braid(Name, Splits0, Splits0, Opts).


gen_braid(_Name, [], _, _Opts) ->
    [];
gen_braid(Name, [[Init|Nth] | Rest], All, Opts) ->
    NthWaits = lists:flatmap(fun(X) -> [X, {wait, 1}] end, Nth),
    Next = case rand:uniform() > proplists:get_value(safe, Opts, 0.5) of
               false -> lists:nth(rand:uniform(length(All)), All);
               true ->
                   Higher = [X || X <- All, X > lists:max([Init|Nth])],
                   case Higher of
                       [] -> none;
                       _ -> lists:nth(rand:uniform(length(Higher)), Higher)
                   end
           end,
    [ { {Name, Init},
        { Init,
          NthWaits ++
              [lists:nth(
                 rand:uniform(
                   min(case proplists:get_value(target, Opts) of
                           undefined -> length(Next); Target -> Target end,
                       length(Next)
                      )),
                 Next
                ) || Next =/= none]
        }
      }
    | gen_braid(Name, Rest, All, Opts)
    ].

%% Lurker never causes a deadlock on its own; just starts a couple of sessions
%% from a single entrypoint
lurker(Name, {Feed, Opts}) ->
    Spread = case proplists:get_value(spread, Opts) of
                 undefined -> rand:uniform(5);
                 {random, N} -> rand:uniform(N) + 1;
                 N -> N
             end,
    gen_lurker(Name, Spread, Feed).

gen_lurker(Name, Spread, [Init|Feed]) ->
    Sessions = [ begin
                     Shuffled = [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- Feed])],
                     {Cut, _} = lists:split(rand:uniform(length(Feed)), Shuffled),
                     intersperse(Cut, {wait, 1, 5}) ++ {wait, 5, 15}
                 end
                 || _ <- lists:seq(0, Spread)
               ],
    [{Name, {Init, list_to_tuple(Sessions)}}].

%% Tree-like lurker. Never deadlocks on its own.
tree(Name, {[Init|Feed], Opts}) ->
    Spread = proplists:get_value(spread, Opts, 2),
    Rate = proplists:get_value(rate, Opts, 0.3),
    Ordered = proplists:get_value(ordered, Opts, false),
    Depth = proplists:get_value(depth, Opts, ceil(math:sqrt(length(Feed)))),
    [{Name, {Init, gen_tree(Feed, Spread, Rate, Ordered, Depth)}}].

gen_tree([], _Spread, _Rate, _Ordered, _) ->
    [];
gen_tree(_Feed, _Spread, _Rate, _Ordered, 0) ->
    [];
gen_tree(Feed, Spread, Rate, Ordered, Depth) ->
    Sessions = [ begin
                     Shuffled = if Ordered -> Feed;
                                   true -> shuffle(Feed)
                                end,
                     Travel = rand:uniform(min(Depth, length(Feed))),
                     {Left, Right} = lists:split(Travel, Shuffled),
                     Left ++
                         case rand:uniform() < Rate of
                             false -> [{wait, 2}];
                             _ when Right =:= [] -> [{wait, 2}];
                             true ->
                                 list_to_tuple([gen_tree(Right, Spread, Rate, Ordered, Depth - Travel)
                                                || _ <- lists:seq(1, Spread)])
                         end
                 end
                 || _ <- lists:seq(0, Spread)
               ],
    list_to_tuple(Sessions).

%%% Utils
shuffle(L) ->
    [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

stretch_splits([], _L) ->
    [];
stretch_splits([S], _L) ->
    [S];
stretch_splits([S0,S1|Ss], L) ->
    case length(S0) > L of
        true ->
            [S0 | stretch_splits([S1|Ss], L)];
        false ->
            stretch_splits([lists:droplast(S0) ++ S1 | Ss], L)
    end.


random_splits(L) ->
    random_splits(L, 0.2).

random_splits(L, Rate) when is_float(Rate) ->
    Splits = [0 | [I || I <- lists:seq(1, length(L)), rand:uniform() < Rate]],
    random_splits(Splits, 0, L);

random_splits(L, {Min, Max}) ->
    SplitsNo = rand:uniform(Max + 1 - Min) - 1 + Min - 1,
    {Splits, _} = lists:split(SplitsNo, shuffle(lists:seq(1, length(L)))),
    random_splits(lists:sort(Splits), 0, L).

random_splits([], _Base, []) ->
    [];
random_splits([], _Base, X) ->
    [X];
random_splits([I|Is], Base, X) ->
    {L, R} = lists:split(I - Base, X),
    case {L, random_splits(Is, I, R)} of
        {[], S} -> S; % Should not happen
        {_, S} -> [L|S]
    end.

intersperse(L, X) ->
    lists:flatmap(fun(E) -> [E, X] end, L).
