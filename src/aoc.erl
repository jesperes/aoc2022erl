-module(aoc).

-export([ timings/0
        , timings/1
        , timing/1
        , tabulate/1
        ]).

-include_lib("eunit/include/eunit.hrl").

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
get_avg(V1) ->
  Avg = trunc(lists:sum(V1) / length(V1)),
  {length(V1), Avg, lists:min(V1), lists:max(V1)}.

-spec timing(Module :: module()) -> {module(), [number()]}.
timing(Module) ->
  MaxSecs = 5,
  MinIter = 1,
  MaxIter = 100,
  %% warmup
  %% run(Module, 5, 0, 1, 10000),
  Values = run(Module, MaxSecs, 0, MinIter, MaxIter),
  {Module, Values}.

run(Module, MaxSecs, Iter, MinIter, MaxIter) ->
  run(Module, erlang:convert_time_unit(MaxSecs, second, microsecond), Iter, MinIter, MaxIter, []).

run(_, TimeRemainingUsecs, Iter, MinIter, MaxIter, Acc) when
    (TimeRemainingUsecs < 0 andalso Iter >= MinIter) orelse
    (Iter >= MaxIter) ->
  Acc;
run(Module, TimeRemainingUsecs, Iter, MinIter, MaxIter, Acc) ->
  io:format("Timing module ~p, time remaining: ~.1fs       \r",
            [Module, TimeRemainingUsecs / 1_000_000.0]),
  {Time, Result} = timer:tc(Module, solve, []),
  case Module of
    day01 -> ?assertEqual({69836, 207968}, Result);
    day02 -> ?assertEqual({14297, 10498}, Result);
    day03 -> ?assertEqual({8349, 2681}, Result);
    day04 -> ?assertEqual({582, 893}, Result);
    day05 -> ?assertEqual({"CNSZFDVLJ","QNDWLMGNS"}, Result);
    day06 -> ?assertEqual({1802, 3551}, Result);
    day07 -> ?assertEqual({1543140, 1117448}, Result);
    day08 -> ?assertEqual({1684, 486540}, Result);
    day09 -> ?assertEqual({6311, 2482}, Result);
    day10 -> ?assertEqual({14060,
                           <<"███   ██  ███  █  █ ████ █  █ ████   ██ \n"
                             "█  █ █  █ █  █ █ █  █    █ █  █       █ \n"
                             "█  █ █  █ █  █ ██   ███  ██   ███     █ \n"
                             "███  ████ ███  █ █  █    █ █  █       █ \n"
                             "█    █  █ █    █ █  █    █ █  █    █  █ \n"
                             "█    █  █ █    █  █ █    █  █ ████  ██  \n"/utf8>>},
                          Result);
    day11 -> ?assertEqual({102399, 23641658401}, Result);
    day12 -> ?assertEqual({370, 363}, Result);
    day13 -> ?assertEqual({5198, 22344}, Result);
    day14 -> ?assertEqual({696, 23610}, Result);
    day15 -> ?assertEqual({4665948, 13543690671045}, Result);
    day16 -> ?assertEqual({1376, 1933}, Result);
    day17 -> ?assertEqual(3153, Result);                                %% TBD part 2
    day18 -> ?assertEqual({3530, 2000}, Result);
    day19 -> ?assertEqual({1382, 31740}, Result);
    day20 -> ?assertEqual({7278, 14375678667089}, Result);
    day21 -> ?assertEqual({268597611536314, 3451534022348}, Result);
    day22 -> ?assertEqual(tbd, Result);                                 %% TBD
    day23 -> ?assertEqual({3684, 862}, Result);
    day24 -> ?assertEqual(tbd, Result);                                 %% TBD
    day25 -> ?assertEqual(tbd, Result)                                  %% TBD
  end,
  run(Module, TimeRemainingUsecs - Time, Iter + 1, MinIter, MaxIter, [Time|Acc]).
