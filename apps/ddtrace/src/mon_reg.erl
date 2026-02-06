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
-define(PG_SCOPE, mon_reg_scope).

-record(state, {monitors = #{}}).  % Track monitors for cleanup on DOWN

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Get monitor PID for a process (supports both PIDs and global names)
mon_of(_Reg, Key) ->
    case pg:get_members(?PG_SCOPE, Key) of
        [] -> undefined;
        [MonPid | _] -> MonPid
    end.

%% @doc Register a monitor for a process
set_mon(Reg, Key, Mon) ->
    gen_server:call(Reg, {register, Key, Mon}).

%% @doc Unregister a monitor for a process
unset_mon(Reg, Key) ->
    gen_server:call(Reg, {unregister, Key}).

%% @doc Send message to monitor of a process
send_mon(Reg, Key, Msg) ->
    case mon_of(Reg, Key) of
        undefined -> exit({badarg, {Key, Msg}});
        MonPid when is_pid(MonPid) -> MonPid ! Msg, MonPid
    end.

via(Reg, Pid) ->
    {via, Reg, Pid}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Ensure pg scope is started (idempotent - handles already_started)
    case pg:start_link(?PG_SCOPE) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> 
            io:format("Warning: Failed to start pg scope: ~p~n", [Reason])
    end,
    {ok, #state{}}.

handle_call({register, Key, MonPid}, _From, #state{monitors = Monitors} = S) ->
    %% Check if already registered in pg
    case pg:get_members(?PG_SCOPE, Key) of
        [] ->
            %% Join the monitor to the process group
            %% Must call pg:join on the same node where MonPid lives
            case node(MonPid) of
                Node when Node =:= node() ->
                    %% Local process - join directly
                    pg:join(?PG_SCOPE, Key, MonPid);
                Node ->
                    %% Remote process - use RPC to join on remote node
                    rpc:call(Node, pg, join, [?PG_SCOPE, Key, MonPid])
            end,
            
            %% Monitor the monitor process for cleanup
            Ref = erlang:monitor(process, MonPid),
            
            {reply, ok, S#state{monitors = Monitors#{MonPid => {Key, Ref}}}};
        [_Existing | _] ->
            {reply, {error, already_registered}, S}
    end;

handle_call({unregister, Key}, _From, #state{monitors = Monitors} = S) ->
    case pg:get_members(?PG_SCOPE, Key) of
        [] ->
            {reply, ok, S};
        [MonPid | _] ->
            %% Leave the process group
            %% Must call pg:leave on the same node where MonPid lives
            case node(MonPid) of
                Node when Node =:= node() ->
                    %% Local process - leave directly
                    pg:leave(?PG_SCOPE, Key, MonPid);
                Node ->
                    %% Remote process - use RPC to leave on remote node
                    rpc:call(Node, pg, leave, [?PG_SCOPE, Key, MonPid])
            end,
            
            %% Remove from monitors map and demonitor
            case maps:take(MonPid, Monitors) of
                error -> 
                    {reply, ok, S};
                {{_Key, Ref}, Monitors2} ->
                    erlang:demonitor(Ref, [flush]),
                    {reply, ok, S#state{monitors = Monitors2}}
            end
    end;

handle_call(_, _, S) ->
    {reply, {error, bad_call}, S}.

handle_info({'DOWN', Ref, process, MonPid, _Reason}, #state{monitors = Monitors} = S) ->
    %% Monitor process died, clean up pg membership
    case maps:take(MonPid, Monitors) of
        error ->
            {noreply, S};
        {{Key, Ref}, Monitors2} ->
            %% Remove from process group
            %% Must call pg:leave on the same node where MonPid lived
            case node(MonPid) of
                Node when Node =:= node() ->
                    %% Local process - leave directly
                    pg:leave(?PG_SCOPE, Key, MonPid);
                Node ->
                    %% Remote process - use RPC to leave on remote node
                    %% (may fail if node is down, ignore errors)
                    catch rpc:call(Node, pg, leave, [?PG_SCOPE, Key, MonPid])
            end,
            {noreply, S#state{monitors = Monitors2}}
    end;
handle_info(_, S) ->
    {noreply, S}.

handle_cast(_, S) -> {noreply, S}.

terminate(_, _State) ->
    %% pg scope will be cleaned up automatically
    ok.

code_change(_, S, _) -> {ok, S}.

