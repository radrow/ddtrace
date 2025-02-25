-export([main/1]).


main([]) ->
    io:format(standard_error, "Provide a scenario file!\n", []),
    erlang:exit(1);
main([File]) ->
    scenario:run(File).
