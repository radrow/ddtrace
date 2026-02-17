%%%-------------------------------------------------------------------
%%% @doc Monitor registry using pg process groups.
%%%
%%% Maps `gen_server` identities (PIDs or global names) to their monitor
%%% processes. Uses a custom pg scope for distributed lookups.
%%% -------------------------------------------------------------------
-module(mon_reg).

-export([ensure_started/0, mon_of/1, set_mon/2, unset_mon/1]).

-define(PG_SCOPE, mon_reg_scope).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Ensure the pg scope is running. Idempotent — safe to call multiple times.
-spec ensure_started() -> ok.
ensure_started() ->
    case pg:start(?PG_SCOPE) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

%% @doc Get monitor PID for a worker.
-spec mon_of(term()) -> pid() | undefined.
mon_of(Key) ->
    case pg:get_members(?PG_SCOPE, Key) of
        [MonPid | _] -> MonPid;
        [] -> undefined
    end.

%% @doc Register a monitor for a worker key.
-spec set_mon(term(), pid()) -> ok | {error, already_registered}.
set_mon(Key, MonPid) ->
    Callback =
        fun() ->
                case pg:get_members(?PG_SCOPE, Key) of
                    [] ->
                        pg:join(?PG_SCOPE, Key, MonPid),
                        ok;
                    [_ | _] ->
                        {error, already_registered}
                end
        end,
    exec_on_pid_node(MonPid, Callback).

%% @doc Unregister a monitor for a worker key.
-spec unset_mon(term()) -> ok.
unset_mon(Key) ->
    case pg:get_members(?PG_SCOPE, Key) of
        [MonPid | _] -> pg:leave(?PG_SCOPE, Key, MonPid), ok;
        [] -> ok
    end.


%%%===================================================================
%%% Helper functions
%%%===================================================================

-spec exec_on_pid_node(pid(), fun(() -> T)) -> T.
exec_on_pid_node(Pid, Fun) when is_pid(Pid), is_function(Fun, 0) ->
    Node = node(Pid),
    case Node of
        N when N =:= node() ->
            Fun();
        N ->
            rpc:call(N, erlang, apply, [Fun, []])
    end.
