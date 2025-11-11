-module(srpc_validator).
%% @doc """
%% Simple module to validate consistency of SRPC states.
%% """

-behaviour(gen_statem).

-export([start_link/0]).
-export([init/1, callback_mode/0]).
-export([unlocked/3, locked/3]).

-export([send_reply/0, send_query/0, recv_reply/0, recv_query/0]).

start_link() ->
    {ok, Pid} = gen_statem:start_link(?MODULE, [], []),
    put(?MODULE, Pid),
    Pid.

callback_mode() ->
    state_functions.

init([]) ->
    {ok, unlocked, #{waits => 0, trace => []}}.


unlocked(cast, recv_query, Data = #{waits := Waits, trace := Trace}) ->
    {keep_state, Data#{waits => Waits + 1, trace => [recv_query | Trace]}};

unlocked(cast, recv_reply, _Data) ->
    error({srpc, recv_reply_unlocked});

unlocked(cast, send_reply, _Data = #{waits := 0}) ->
    error({srpc, send_reply_no_waits});

unlocked(cast, send_reply, Data = #{waits := Waits, trace := Trace}) ->
    {keep_state, Data#{waits => Waits - 1, trace => [send_reply | Trace]}};

unlocked(cast, send_query, Data = #{trace := Trace}) ->
    {next_state, locked, Data#{trace => [send_query|Trace] }}.


locked(cast, send_reply, _Data = #{waits := 0}) ->
    error({srpc, send_reply_locked_no_waits});

locked(cast, send_reply, _Data) ->
    error({srpc, send_reply_locked});

locked(cast, recv_reply, Data = #{trace := Trace}) ->
    {next_state, unlocked, Data#{trace => [recv_reply | Trace]}};

locked(cast, recv_query, Data = #{waits := Waits, trace := Trace}) ->
    {keep_state, Data#{waits => Waits + 1, trace => [recv_query | Trace]}};

locked(cast, send_query, _Data) ->
    error({srpc, send_query_locked}).

%%======================
%% API
%%======================

send_reply() ->
    Pid = get(?MODULE),
    gen_statem:cast(Pid, send_reply).

send_query() ->
    Pid = get(?MODULE),
    gen_statem:cast(Pid, send_query).

recv_reply() ->
    Pid = get(?MODULE),
    gen_statem:cast(Pid, recv_reply).

recv_query() ->
    Pid = get(?MODULE),
    gen_statem:cast(Pid, recv_query).

%%======================
%% Internal functions
%%======================
