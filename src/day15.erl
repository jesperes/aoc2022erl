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
  %% Bin = input:get(15),
  %% P1YRange = {10, 10},
  %% P2Range = {0, 20},

  Bin = <<"Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n",
          "Sensor at x=9, y=16: closest beacon is at x=10, y=16\n",
          "Sensor at x=13, y=2: closest beacon is at x=15, y=3\n",
          "Sensor at x=12, y=14: closest beacon is at x=10, y=16\n",
          "Sensor at x=10, y=20: closest beacon is at x=10, y=16\n",
          "Sensor at x=14, y=17: closest beacon is at x=10, y=16\n",
          "Sensor at x=8, y=7: closest beacon is at x=2, y=10\n",
          "Sensor at x=2, y=0: closest beacon is at x=2, y=10\n",
          "Sensor at x=0, y=11: closest beacon is at x=2, y=10\n",
          "Sensor at x=20, y=14: closest beacon is at x=25, y=17\n",
          "Sensor at x=17, y=20: closest beacon is at x=21, y=22\n",
          "Sensor at x=16, y=7: closest beacon is at x=15, y=3\n",
          "Sensor at x=14, y=3: closest beacon is at x=15, y=3\n",
          "Sensor at x=20, y=1: closest beacon is at x=15, y=3\n"
        >>,
  P1YRange = {10, 10},
  P2Range = {0, 20},

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
              [{Sensor, Beacon, Dist}|Acc]
          end
      end, [], Lines),

  P1XRange = {lists:min([Sx - Dist || {{Sx, _}, _, Dist} <- SnB]),
              lists:max([Sx + Dist || {{Sx, _}, _, Dist} <- SnB])},
  P1Cands = candidates(SnB, P1XRange, P1YRange),

  P1 = lists:foldl(fun(Pos, Acc) ->
                       case is_in_range_of_any_sensor(Pos, SnB) of
                         true -> [Pos|Acc];
                         false -> Acc
                       end
                   end, [], P1Cands),

  P2Cands = candidates(SnB, P2Range, P2Range),
  {value, Pos} = lists:search(fun(Pos) ->
                                  not lists:any(
                                        fun({S, _, Dist}) ->
                                            dist(Pos, S) =< Dist
                                        end, SnB)
                              end, P2Cands),
  P2 = tuning_frequency(Pos),
  {length(P1), P2}.

is_in_range_of_any_sensor(Pos, SnB) ->
  lists:any(fun({S, _B, Dist}) ->
                case dist(Pos, S) of
                  D when D =< Dist ->
                    true;
                  _ -> false
                end
            end, SnB).

tuning_frequency({X, Y}) ->
  X * 4000000 + Y.

candidates(SnB, XRange, YRange) ->
  lists:foldl(fun({S, _B, Dist}, Acc) ->
                  just_outside(S, Dist, XRange, YRange, Acc)
              end, [], SnB).

in_range(X, {Min, Max}) ->
  X >= Min andalso X =< Max.

%% Generate the coordinates with manhattan distance D + 1 from {Cx,
%% Cy}.
just_outside({Cx, Cy} = _Center, D, XRange, YRange, Acc) ->
  Dist = D + 1,
  L1 = diagonal({Cx,        Cy - Dist}, {1,   1}, Dist, XRange, YRange, Acc),
  L2 = diagonal({Cx + Dist, Cy},        {-1,  1}, Dist, XRange, YRange, L1),
  L3 = diagonal({Cx,        Cy + Dist}, {-1, -1}, Dist, XRange, YRange, L2),
  L  = diagonal({Cx - Dist, Cy},        {1,  -1}, Dist, XRange, YRange, L3),
  L.

diagonal(_, _, 0, _, _, Acc) ->
  Acc;
diagonal({X, Y}, {Dx, Dy} = Delta, N, XRange, YRange, Acc) ->
  case in_range(X, XRange) andalso in_range(Y, YRange) of
    true ->
      diagonal({X + Dx, Y + Dy}, Delta, N - 1, XRange, YRange, [{X, Y}|Acc]);
    false ->
      diagonal({X + Dx, Y + Dy}, Delta, N - 1, XRange, YRange, Acc)
  end.

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
