%%%-------------------------------------------------------------------
%%% @doc Monitor registry
%%%-------------------------------------------------------------------
-module(mon_reg).
-behaviour(gen_server).

%% API
-export([start_link/0, mon_of/2, set_mon/3, unset_mon/2, send_mon/3, via/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {map = #{}}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

mon_of(Reg, Pid) ->
    gen_server:call(Reg, {whereis, Pid}).

set_mon(Reg, Pid, Mon) ->
    gen_server:call(Reg, {register, Pid, Mon}).

unset_mon(Reg, Pid) ->
    gen_server:call(Reg, {unregister, Pid}).

send_mon(Reg, Pid, Msg) ->
    case mon_of(Reg, Pid) of
        undefined -> exit({badarg, {Pid, Msg}});
        Pid when is_pid(Pid) -> Pid ! Msg, Pid
    end.

via(Reg, Pid) ->
    {via, Reg, Pid}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({whereis, Name}, _From, #state{map = M} = S) ->
    {reply, maps:get(Name, M, undefined), S};

handle_call({register, Name, Pid}, _From, #state{map = M} = S) ->
    case maps:is_key(Name, M) of
        true ->
            {reply, {error, already_registered}, S};
        false ->
            Ref = erlang:monitor(process, Pid),
            {reply, ok, S#state{map = M#{Name => {Pid, Ref}}}}
    end;

handle_call({unregister, Name}, _From, #state{map = M} = S) ->
    case maps:take(Name, M) of
        error -> {reply, ok, S};
        {{Pid, Ref}, M2} ->
            erlang:demonitor(Ref, [flush]),
            {reply, ok, S#state{map = M2}}
    end;

handle_call(_, _, S) ->
    {reply, {error, bad_call}, S}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{map = M} = S) ->
    %% Remove any entry whose monitor triggered
    M2 = maps:filter(fun(_K, {__Pid, R}) -> R =/= Ref end, M),
    {noreply, S#state{map = M2}};
handle_info(_, S) ->
    {noreply, S}.

handle_cast(_, S) -> {noreply, S}.
terminate(_, _) -> ok.
code_change(_, S, _) -> {ok, S}.

