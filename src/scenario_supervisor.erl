-module(scenario_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    Settings = #{ strategy => one_for_one
                , intensity => 5
                , period => 10
                , auto_shutdown => never
                },
    {ok, {Settings, []}}.
