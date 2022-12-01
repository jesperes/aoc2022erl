-module(aoc).

-export([measure/2]).

measure(Fun, Times) ->
  {Total, Value} =
    lists:foldl(
      fun(_, {OldTime, _}) ->
          {Time, Value} = timer:tc(Fun),
          {OldTime + Time, Value}
      end, {0, undefined}, lists:seq(0, Times)),
  AverageUsecs = Total / Times,
  io:format("~s: ~p usecs~n", [erlang:fun_to_list(Fun), AverageUsecs]),
  {AverageUsecs, Value}.
