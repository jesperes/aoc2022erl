-module(aoc).

-export([ measure/2
        , timings/0
        ]).

measure(Fun, Times) ->
  {Total, Value} =
    lists:foldl(
      fun(_, {OldTime, _}) ->
          {Time, Value} = timer:tc(Fun),
          {OldTime + Time, Value}
      end, {0, undefined}, lists:seq(0, Times)),
  AverageUsecs = trunc(Total / Times),
  {AverageUsecs, Value}.

timings() ->
  Days = [{day01, 5000},
          {day02, 5000},
          {day03, 3000},
          {day04, 2000}],
  Str = "Day,Reps,Time (usecs),Time (msecs)\n" ++
    lists:map(fun({Day, Reps}) ->
                  {TimeUsecs, _} = measure(fun Day:solve/0, Reps),
                  TimeMsecs = TimeUsecs / 1000,
                  io_lib:format("~ts,~p,~p,~.5f~n", [Day, Reps, TimeUsecs, TimeMsecs])
              end, Days),

  Tempfile = "/tmp/tabulate",
  ok = file:write_file(Tempfile, Str),
  try
    Cmd = io_lib:format("tabulate -f fancy_grid -s, -1 ~s", [Tempfile]),
    Result = os:cmd(Cmd),
    io:format("~ts~n", [Result])
  after
      ok = file:delete(Tempfile)
  end.
