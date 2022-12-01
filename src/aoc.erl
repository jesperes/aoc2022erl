-module(aoc).

-export([measure/2]).

measure(Fun, Times) ->
  Total =
    lists:foldl(
      fun(_, Acc) ->
          {Time, _} = timer:tc(Fun),
          Acc + Time
      end, 0, lists:seq(0, Times)),
  AverageUsecs = Total / Times,
  io:format("~s: ~p usecs~n", [erlang:fun_to_list(Fun), AverageUsecs]),
  AverageUsecs.
