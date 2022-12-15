-module(day15).

-export([ solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(int(X), binary_to_integer(X)).

solve() ->
  Bin = input:get(15),
  %% Bin = <<"Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n",
  %%         "Sensor at x=9, y=16: closest beacon is at x=10, y=16\n",
  %%         "Sensor at x=13, y=2: closest beacon is at x=15, y=3\n",
  %%         "Sensor at x=12, y=14: closest beacon is at x=10, y=16\n",
  %%         "Sensor at x=10, y=20: closest beacon is at x=10, y=16\n",
  %%         "Sensor at x=14, y=17: closest beacon is at x=10, y=16\n",
  %%         "Sensor at x=8, y=7: closest beacon is at x=2, y=10\n",
  %%         "Sensor at x=2, y=0: closest beacon is at x=2, y=10\n",
  %%         "Sensor at x=0, y=11: closest beacon is at x=2, y=10\n",
  %%         "Sensor at x=20, y=14: closest beacon is at x=25, y=17\n",
  %%         "Sensor at x=17, y=20: closest beacon is at x=21, y=22\n",
  %%         "Sensor at x=16, y=7: closest beacon is at x=15, y=3\n",
  %%         "Sensor at x=14, y=3: closest beacon is at x=15, y=3\n",
  %%         "Sensor at x=20, y=1: closest beacon is at x=15, y=3\n"
  %%       >>,

  Lines = binary:split(Bin, <<"\n">>, [global]),
  SensorsAndBeacons =
    lists:foldl(
      fun(<<>>, Acc) -> Acc;
         (Line, Acc) ->
          case re:run(Line, "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)",
                      [{capture, all_but_first, binary}]) of
            {match, [Sx, Sy, Bx, By]} ->
              Sensor = {?int(Sx), ?int(Sy)},
              Beacon = {?int(Bx), ?int(By)},
              [{Sensor, Beacon, dist(Sensor, Beacon)}|Acc]
          end
      end, [], Lines),
  X = find_positions_without_beacons(SensorsAndBeacons, 2000000),
  X.


find_positions_without_beacons(SensorsAndBeacons, Y) ->
  MinX = lists:min([Sx - Dist || {{Sx, _}, _, Dist} <- SensorsAndBeacons]),
  MaxX = lists:max([Sx + Dist || {{Sx, _}, _, Dist} <- SensorsAndBeacons]),

  %%io:format("~2w ", [Y]),

  N = lists:foldl(
        fun(X, Acc) ->
            Pos = {X, Y},
            case is_beacon_or_sensor(Pos, SensorsAndBeacons) of
              true ->
                %%io:format("@", []),
                Acc;
              false ->
                case cannot_contain_beacon(Pos, SensorsAndBeacons) of
                  true -> Acc + 1;
                  false ->
                    %%io:format(".", []),
                    Acc
                end
            end
        end, 0, lists:seq(MinX, MaxX)),

  %%io:format("~n", []),
  N.

is_beacon_or_sensor(Pos, SnB) ->
  lists:any(fun({_, B, _}) when B =:= Pos -> true;
               ({S, _, _}) when S =:= Pos -> true;
               (_) -> false
            end, SnB).

cannot_contain_beacon(Pos, SnB) ->
  lists:any(fun({Sensor, _Beacon, Dist} = _Pair) ->
                case dist(Pos, Sensor) of
                  D when D =< Dist ->
                    %%io:format("#", []),
                    true;
                  _ ->
                    false
                end
            end, SnB).

%% exclude_other_beacons({Sx, Sy} = Sensor, {Bx, By} = Beacon, Map) ->
%%   Dist = dist(Sensor, Beacon),
%%   lists:foldl(
%%     fun(Pos, Acc) ->
%%         maps:put(Pos, $#, Acc)
%%     end, Map,
%%     [{X, Y} || X <- lists:seq(Sx - Dist, Sx + Dist),
%%                Y <- lists:seq(Sy - Dist, Sy + Dist),
%%                dist(Sensor, {X, Y}) =< Dist]).

dist({X0, Y0}, {X1, Y1}) ->
  abs(X0 - X1) + abs(Y0 - Y1).

-ifdef(TEST).

day13_test() ->
  not_solved = solve().

-endif.
