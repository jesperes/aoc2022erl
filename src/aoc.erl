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
    [io_lib:format("Module,Avg (μs),Avg (s),Iter,σ~n", []),
     lists:map(
       fun(#{module := Module,
             standard_deviation := S,
             avg := Avg,
             values := Values}) ->
           %% {Avg0, Unit} = format_time(Avg),
           AvgUsecs = trunc(Avg),
           AvgSecs = Avg / 1000000,
           io_lib:format("~s,~w,~.3f,~w,~.2f~n",
                         [Module, AvgUsecs, AvgSecs, length(Values), S])
       end, Runs)],
  ok = file:write_file("/tmp/tabulate", [unicode:characters_to_binary(Str)]),
  Output = os:cmd(
             lists:flatten(io_lib:format("tabulate -1 -f simple -s, /tmp/tabulate", []))),
  io:format("~ts~n", [Output]).

standard_deviation(Values) ->
  math:sqrt(variance(Values)).

variance(Values) ->
  Sum = lists:sum(Values),
  Mean = Sum / length(Values),
  SquaredDiffs = lists:foldl(fun(V, Acc) ->
                                 Diff = (V - Mean),
                                 [Diff * Diff|Acc]
                             end, [], Values),
  lists:sum(SquaredDiffs) / (length(Values) - 1).

avg(Values) ->
  lists:sum(Values) / length(Values).

timing(Module) ->
  io:format("Timing module ~p...~n", [Module]),
  MaxSecs = 5,
  MinIter = 5,
  MaxIter = 1000,
  Values = run(Module, MaxSecs, 0, MinIter, MaxIter),
  #{module => Module,
    standard_deviation => standard_deviation(Values),
    avg => avg(Values),
    values => Values}.

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
