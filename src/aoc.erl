-module(aoc).

-export([ timings/0
        , timings/1
        , timing/1
        , tabulate/1
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
            true -> {true, Module};
            false -> false
          end
      end, lists:seq(1, 25)),
  timings(Days).

timings(Days) ->
  lists:map(fun(Module) ->
                timing(Module)
            end, Days).

tabulate(Runs) ->
  io:setopts([{encoding, unicode}]),
  Str =
    [io_lib:format("Module,Avg (μs),Min (μs), Max (μs),Iter,σ~n", []),
     lists:map(
       fun({Module, Values}) ->
           {NumVals, Avg, Min, Max} = get_avg(Values),
           io_lib:format("~s,~w,~w,~w,~w~n",
                         [Module, Avg, Min, Max, NumVals])
       end, Runs)],
  case unicode:characters_to_binary(Str) of
    Bin when is_binary(Bin) ->
      ok = file:write_file("/tmp/tabulate", Bin),
      Output = os:cmd(
                 lists:flatten(io_lib:format("tabulate -1 -f github -s, /tmp/tabulate", []))),
      io:format("~n~ts~n", [Output])
  end.

-spec get_avg(Values :: [number()]) -> {integer(), number(), number(), number()}.
get_avg(Values) ->
  %% Remove max/min
  Min = lists:min(Values),
  Max = lists:max(Values),
  V0 = lists:delete(Min, Values),
  V1 = lists:delete(Max, V0),
  Avg = trunc(lists:sum(V1) / length(V1)),
  {length(Values), Avg, lists:min(V1), lists:max(V1)}.

-spec timing(Module :: module()) -> {module(), [number()]}.
timing(Module) ->
  MaxSecs = 5,
  MinIter = 5,
  MaxIter = 1000,
  %% warmup
  run(Module, 5, 0, 1, 10000),
  Values = run(Module, MaxSecs, 0, MinIter, MaxIter),
  {Module, Values}.

run(Module, MaxSecs, Iter, MinIter, MaxIter) ->
  run(Module, erlang:convert_time_unit(MaxSecs, second, microsecond), Iter, MinIter, MaxIter, []).

run(_, TimeRemainingUsecs, Iter, MinIter, MaxIter, Acc) when
    (TimeRemainingUsecs < 0 andalso Iter >= MinIter) orelse
    (Iter >= MaxIter) ->
  Acc;
run(Module, TimeRemainingUsecs, Iter, MinIter, MaxIter, Acc) ->
  %% io:format("Timing module ~p, time remaining: ~p~n", [Module, TimeRemainingUsecs]),
  {Time, _} = timer:tc(Module, solve, []),
  run(Module, TimeRemainingUsecs - Time, Iter + 1, MinIter, MaxIter, [Time|Acc]).
