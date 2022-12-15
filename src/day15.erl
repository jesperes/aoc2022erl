-module(day15).

-export([ solve/0
        ]).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(int(X), binary_to_integer(X)).

%% Part 2: find the only possible solution for the distress beacon,
%% i.e. the only position which is not in range of any beacon. Each
%% sensor excludes a diamond-shaped area determined by the
%% Manhattan-distance to its nearest beacon. There is a single
%% position which is not covered by any such diamond, find it!
%%
%% For part 2, the {x,y} coordinates must be in the range [0, 4000000]
%% which means that the total number of position is 4 trillion (4
%% terapixels). We cannot search all of the pixels.
%%
%% However, the pixel we are looking for (since there is only one),
%% must be just outside the range of one of the sensors (otherwise
%% there would be more than one pixel). This limits the search space
%% enormously.
%%

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
  SnB =
    lists:foldl(
      fun(<<>>, Acc) -> Acc;
         (Line, Acc) ->
          case re:run(Line, "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)",
                      [{capture, all_but_first, binary}]) of
            {match, [Sx, Sy, Bx, By]} ->
              Sensor = {?int(Sx), ?int(Sy)},
              Beacon = {?int(Bx), ?int(By)},
              Dist = dist(Sensor, Beacon),
              %% io:format("Sensor at ~w, closest beacon is at ~w, distance is ~w~n",
              %%           [Sensor, Beacon, Dist]),
              [{Sensor, Beacon, Dist}|Acc]
          end
      end, [], Lines),

  Cands = candidates(SnB),
  InRange = in_range(Cands, 0, 4000000),

  Grid = lists:foldl(
           fun(Pos, Acc) ->
               case lists:any(
                      fun({S, _, Dist}) ->
                          dist(Pos, S) =< Dist
                      end, SnB) of
                 false ->
                   io:format("Tuning frequency of ~p = ~p~n", [Pos, tuning_frequency(Pos)]),
                   maps:put(Pos, $@, Acc);
                 true ->
                   Acc
               end
           end, #{}, InRange),

  io:format("~s~n", [grid:to_str(Grid)]).

tuning_frequency({X, Y}) ->
  X * 4000000 + Y.

candidates(SnB) ->
  lists:foldl(fun({S, _B, Dist}, Acc) ->
                  just_outside(S, Dist, Acc)
              end, [], SnB).


in_range(Coords, Min, Max) ->
  lists:filter(fun({X, Y}) ->
                   X >= Min andalso X =< Max andalso
                     Y >= Min andalso Y =< Max
               end, Coords).

%% Generate the coordinates with manhattan distance D + 1 from {Cx,
%% Cy}.
just_outside({Cx, Cy} = _Center, D, Acc) ->
  Dist = D + 1,
  L1 = diagonal({Cx,        Cy - Dist}, {1,   1}, Dist, Acc),
  L2 = diagonal({Cx + Dist, Cy},        {-1,  1}, Dist, L1),
  L3 = diagonal({Cx,        Cy + Dist}, {-1, -1}, Dist, L2),
  L  = diagonal({Cx - Dist, Cy},        {1,  -1}, Dist, L3),
  %% print_coords(L),
  %% ?assert(lists:all(fun(Pos) ->
  %%                       dist(Pos, Center) == Dist
  %%                   end, L)),
  L.

diagonal(_, _, 0, Acc) ->
  Acc;
diagonal({X, Y}, {Dx, Dy} = Delta, N, Acc) ->
  diagonal({X + Dx, Y + Dy}, Delta, N - 1, [{X, Y}|Acc]).


%% find_positions_without_beacons(SnB, Y) ->
%%   MinX = lists:min([Sx - Dist || {{Sx, _}, _, Dist} <- SnB]),
%%   MaxX = lists:max([Sx + Dist || {{Sx, _}, _, Dist} <- SnB]),

%%   io:format("MinX = ~w~n", [MinX]),
%%   io:format("MaxX = ~w~n", [MaxX]).






%%   N = lists:foldl(
%%         fun(X, Acc) ->
%%             Pos = {X, Y},
%%             case is_beacon_or_sensor(Pos, SnB) of
%%               true ->
%%                 %%io:format("@", []),
%%                 Acc;
%%               false ->
%%                 case cannot_contain_beacon(Pos, SnB) of
%%                   true -> Acc + 1;
%%                   false ->
%%                     %%io:format(".", []),
%%                     Acc
%%                 end
%%             end
%%         end, 0, lists:seq(MinX, MaxX)),

%%   %%io:format("~n", []),
%%   N.

%% is_beacon_or_sensor(Pos, SnB) ->
%%   lists:any(fun({_, B, _}) when B =:= Pos -> true;
%%                ({S, _, _}) when S =:= Pos -> true;
%%                (_) -> false
%%             end, SnB).

%% cannot_contain_beacon(Pos, SnB) ->
%%   lists:any(fun({Sensor, _Beacon, Dist} = _Pair) ->
%%                 case dist(Pos, Sensor) of
%%                   D when D =< Dist ->
%%                     %%io:format("#", []),
%%                     true;
%%                   _ ->
%%                     false
%%                 end
%%             end, SnB).

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

print_coords(L) ->
  io:format("~s~n",
            [grid:to_str(
               lists:foldl(fun(Pos, Acc) ->
                               maps:put(Pos, $#, Acc)
                           end, #{}, L))]).

-ifdef(TEST).

day13_test() ->
  not_solved = solve().

-endif.
