-module(aoc).

-export([ timings/0
        ]).

timings() ->
  Loadable = lists:map(
               fun({Module, _, _}) ->
                   list_to_atom(Module)
               end, code:all_available()),
  Days =
    lists:filtermap(
      fun(Day) ->
          Module = list_to_atom(lists:flatten(io_lib:format("day~2..0w", [Day]))),
          case lists:member(Module, Loadable) of
            true ->
              %% Recompile and load it
              {ok, _} = c:c(Module),
              {true, Module};
            false -> false
          end
      end, lists:seq(1, 25)),
  erlperf_cli:main(
    ["-w", "5"] ++
      lists:map(fun(Day) ->
                    lists:flatten(io_lib:format("~s:solve().", [Day]))
                end, Days)),
  ok.
