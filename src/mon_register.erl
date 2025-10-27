-module(mon_register).

-export([start_link/0, stop/0]).

-export([mon_of/2, via/2]).

start_link() ->
    register:start_link(?MODULE, [], []).

stop() ->
    register:stop(?MODULE).

mon_of(Pid, MonRegister) ->
    register:whereis(MonRegister, Pid).

via(Pid, MonRegister) ->
    {via, MonRegister, Pid}.
