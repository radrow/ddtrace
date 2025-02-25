-module(ansi_color).

-export([render/1]).

-type color() :: black
               | red   | green   | yellow   | blue   | violet   | cyan   | white
               | red_l | green_l | yellow_l | blue_l | violet_l | cyan_l | white_l.


-type prop() :: bold | dim | italic | underline | blink | invert | hidden | strike
              | reset | nobold | noitalic | nounderline | noblink | noinvert | nohidden | nostrike | nofg | nobg
              | color() | {bg, color()}.


-type span() :: string() | binary() | list(span()) | {prop()} | {prop() | list(prop()), string()}.


-spec str(prop()) -> string().
str(black) -> "30";
str(red) -> "31";
str(green) -> "32";
str(yellow) -> "33";
str(blue) -> "34";
str(violet) -> "35";
str(cyan) -> "36";
str(white) -> "37";
str(black_l) -> "90";
str(red_l) -> "91";
str(green_l) -> "92";
str(yellow_l) -> "93";
str(blue_l) -> "94";
str(violet_l) -> "95";
str(cyan_l) -> "96";
str(white_l) -> "97";

str({bg, black}) -> "40";
str({bg, red}) -> "41";
str({bg, green}) -> "42";
str({bg, yellow}) -> "43";
str({bg, blue}) -> "44";
str({bg, violet}) -> "45";
str({bg, cyan}) -> "46";
str({bg, white}) -> "47";
str({bg, black_l}) -> "100";
str({bg, red_l}) -> "101";
str({bg, green_l}) -> "102";
str({bg, yellow_l}) -> "103";
str({bg, blue_l}) -> "104";
str({bg, violet_l}) -> "105";
str({bg, cyan_l}) -> "106";
str({bg, white_l}) -> "107";

str(bold) -> "1";
str(dim) -> "2";
str(italic) -> "3";
str(underline) -> "4";
str(blink) -> "5";
str(invert) -> "7";
str(hidden) -> "8";
str(strike) -> "9";


str(nobold) -> "22";
str(noitalic) -> "23";
str(nounderline) -> "24";
str(noblink) -> "25";
str(noinvert) -> "27";
str(nohidden) -> "28";
str(nostrike) -> "29";
str(nofg) -> "39";
str(nobg) -> "49";

str(reset) -> "0".


-spec render(span()) -> string().
render(B) when is_binary(B) ->
    binary:bin_to_list(B);
render(I) when is_integer(I) ->  % char
    I;
render([I | Rest]) when is_integer(I) ->  % char
    [I | render(Rest)];
render([]) ->
    [];
render([S | Rest]) ->
    render(S) ++ render(Rest);
render({Prop}) ->
    render_prop(Prop);
render({Props, S}) ->
    render_prop(Props) ++ render(S) ++ render_prop(reset).


render_prop([]) -> "";
render_prop(Props) when is_list(Props) ->
    "\e[" ++ string:join(lists:map(fun str/1, Props), ";") ++ "m";
render_prop(Prop) ->
    render_prop([Prop]).
