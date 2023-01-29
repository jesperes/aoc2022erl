-module(day15).

-export([ solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(int(X), binary_to_integer(X)).

-type coord() :: {integer(), integer()}.

-record(snb, { sensor :: coord()
             , beacon :: coord()
             , dist :: integer()
             }).

-type snb() :: #snb{}.

-record(input, { snbs = [] :: [snb()]
               , sensors = sets:new()
               , beacons = sets:new()
               }).

solve() ->
  Bin = input:get(15),
  P1Y = 2000000,
  P2Range = {0, 4000000},
  Lines = binary:split(Bin, <<"\n">>, [global]),
  Input =
    lists:foldl(
      fun(<<>>, Acc) -> Acc;
         (Line, #input{ snbs = SnBs
                      , beacons = Beacons
                      , sensors = Sensors
                      } = Input)->
          case re:run(Line, "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)",
                      [{capture, all_but_first, binary}]) of
            {match, [Sx, Sy, Bx, By]} ->
              Sensor = {?int(Sx), ?int(Sy)},
              Beacon = {?int(Bx), ?int(By)},
              Dist = dist(Sensor, Beacon),
              Input#input{ snbs = [#snb{ sensor = Sensor
                                       , beacon = Beacon
                                       , dist = Dist
                                       }|SnBs]
                         , sensors = sets:add_element(Sensor, Sensors)
                         , beacons = sets:add_element(Beacon, Beacons)
                         }
          end
      end, #input{},
      Lines
     ),

  {part1(Input, P1Y),
   part2(Input, P2Range)}.

part1(Input, P1Y) ->
  {P1XMin, P1XMax} = {lists:min([Sx - Dist || #snb{sensor = {Sx, _}, dist = Dist} <- Input#input.snbs]),
                      lists:max([Sx + Dist || #snb{sensor = {Sx, _}, dist = Dist} <- Input#input.snbs])},
  P1Line = {{P1XMin, P1Y}, {P1XMax, P1Y}},
  Intervals =
    lists:foldl(
      fun(#snb{sensor = Sensor, dist = Dist}, Acc) ->
          Intersects =
            lists:filtermap(
              fun(EL) ->
                  intersects({P1Line, EL})
              end, edge_lines(Sensor, Dist)),

          case Intersects of
            [] -> Acc;
            [_] -> Acc;
            [{Ax, _}, {Bx, _}] when Ax =< Bx ->
              [{Ax, Bx}|Acc];
            [{Ax, _}, {Bx, _}] ->
              [{Bx, Ax}|Acc]
          end
      end, [], Input#input.snbs),
  Sorted = lists:sort(Intervals),
  Count = count_intervals(Sorted, 0),
  %% Find all sensors/beacons on the P1Y line, and substract them from
  %% the count (assuming they should not be counted as a position
  %% which "cannot contain a beacon").
  SnB = sets:size(
          sets:filter(fun({_, Y}) when Y == P1Y -> true;
                         (_) -> false
                      end, sets:union(Input#input.sensors,
                                      Input#input.beacons))),
  Count - SnB.

part2(Input, _P2Range) ->
  PL = all_perimeter_lines(Input),
  Pairs = [{L1, L2} || L1 <- PL, L2 <- PL, L1 < L2],

  Freq =
    lists:foldl(
      fun(Pair, Acc) ->
          case intersects(Pair) of
            false ->
              Acc;
            {true, Pos} ->
              case is_in_range_of_any_sensor(Pos, Input#input.snbs) of
                true ->
                  %% This intersection is actually within
                  %% range of another sensor, so ignore it.
                  maps:remove(Pos, Acc);
                false ->
                  maps:update_with(Pos, fun(Old) -> Old + 1 end, 2, Acc)
              end
          end
      end, #{}, Pairs),

  [{Pos, _}] = lists:filter(fun({_, N}) when N >= 4 -> true;
                               (_) -> false
                            end, maps:to_list(Freq)),

  tuning_frequency(Pos).

%% ============================================================
%% Helper functions
%% ============================================================

count_intervals([{X0, X1}], Acc) ->
  X1 - X0 + 1 + Acc;
count_intervals([{_X1, X2} = I1, {_X3, X4}|Rest], Acc) when X4 =< X2 ->
  %% [X3,X4] is a subset of [X1,X2]
  count_intervals([I1|Rest], Acc);
count_intervals([{X1, X2}, {X3, X4}|Rest], Acc) when X3 =< X2 ->
  %% [X1,X2] partially overlaps [X3,X4]
  count_intervals([{X1, X4}|Rest], Acc);
count_intervals([{X1, X2}, {X3, _X4} = I1|Rest], Acc) when X2 < X3 ->
  %% [X1,X2] is a disjoint interval
  count_intervals([I1|Rest], X2 - X1 + 1 + Acc).

all_perimeter_lines(Input) ->
  lists:flatten(
    lists:foldl(
      fun(#snb{sensor = Sensor, dist = Dist}, Acc) ->
          [perimeter_lines(Sensor, Dist)|Acc]
      end, [], Input#input.snbs)).

perimeter_lines({X, Y}, D) ->
  L1 = {{X, Y - D - 1}, {X + D, Y - 1}},
  L2 = {{X + D + 1, Y}, {X + 1, Y + D}},
  L3 = {{X, Y + D + 1}, {X - D, Y + 1}},
  L4 = {{X - D - 1, Y}, {X - 1, Y - D}},
  [L1, L2, L3, L4].

edge_lines({X, Y}, D) ->
  L1 = {{X, Y - D}, {X + D, Y}},
  L2 = {{X + D, Y}, {X, Y + D}},
  L3 = {{X, Y + D}, {X - D, Y}},
  L4 = {{X - D, Y}, {X, Y - D}},
  [L1, L2, L3, L4].

intersects({{{Ax, Ay}, {Bx, By}} = L1,
            {{Cx, Cy}, {Dx, Dy}} = L2}) ->
  A1 = By - Ay,
  B1 = Ax - Bx,
  C1 = A1*Ax + B1*Ay,

  A2 = Dy - Cy,
  B2 = Cx - Dx,
  C2 = A2*Cx + B2*Cy,

  Det = A1*B2 - A2*B1,
  if Det == 0 -> false;
     true ->
      X0 = (B2*C1 - B1*C2),
      Y0 = (A1*C2 - A2*C1),

      if ((X0 rem Det) == 0) andalso ((Y0 rem Det) == 0) ->
          X00 = X0 div Det,
          Y00 = Y0 div Det,

          case
            is_validx(X00, L1) andalso
            is_validx(X00, L2) andalso
            is_validy(Y00, L1) andalso
            is_validy(Y00, L2)
          of
            true -> {true, {X00, Y00}};
            _ -> false
          end;
         true ->
          false
      end
  end.

is_validx(X, {{Ax, _Ay}, {Bx, _By}}) when Ax =< Bx ->
  X >= Ax andalso X =< Bx;
is_validx(X, {P1, P2}) ->
  is_validx(X, {P2, P1}).

is_validy(Y, {{_Ax, Ay}, {_Bx, By}}) when Ay =< By ->
  Y >= Ay andalso Y =< By;
is_validy(Y, {P1, P2}) ->
  is_validy(Y, {P2, P1}).

is_in_range_of_any_sensor(Pos, SnB) ->
  lists:any(fun(#snb{sensor = S, dist = Dist}) ->
                case dist(Pos, S) of
                  D when D =< Dist -> true;
                  _ -> false
                end
            end, SnB).

tuning_frequency({X, Y}) ->
  X * 4000000 + Y.

dist({X0, Y0}, {X1, Y1}) ->
  abs(X0 - X1) + abs(Y0 - Y1).

-ifdef(TEST).

day15_test() ->
  {4665948, 13543690671045} = solve().

-endif.
