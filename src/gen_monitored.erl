-module(gen_monitored).
-behaviour(gen_server).

-include("dlstalk.hrl").

%% API
-export([start_link/3]).

%% gen_server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Start the server
start_link(Module, Args0, Options) ->
    Args1 = [{monitor, self()}, {module, Module} | Args0],
    gen_server:start_link(?MODULE, Args1, Options).

%% gen_server callbacks

init([{monitor, Monitor}, {module, Module}|Args]) ->
    put(?MON_PID, Monitor),
    put(?CALLBACK_MOD, Module),
    Module:init(Args).


handle_call(Msg, From, State) ->
    Module = get(?CALLBACK_MOD),
    Module:handle_call(Msg, From, State).


handle_cast(Msg, State) ->
    Module = get(?CALLBACK_MOD),
    Module:handle_cast(Msg, State).


handle_info(Info, State) ->
    Module = get(?CALLBACK_MOD),
    case erlang:function_exported(Module, handle_info, 2) of
        true -> Module:handle_info(Info, State);
        false -> ok
    end.


terminate(Reason, State) ->
    Module = get(?CALLBACK_MOD),
    case erlang:function_exported(Module, terminate, 2) of
        true -> Module:terminate(Reason, State);
        false -> ok
    end.


code_change(OldVsn, State, Extra) ->
    Module = get(?CALLBACK_MOD),
    case erlang:function_exported(Module, code_change, 3) of
        true -> Module:code_chane(OldVsn, State, Extra);
        false -> ok
    end.
