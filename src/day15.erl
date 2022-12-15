-module(day15).

-export([ solve/0
        ]).

-compile([export_all, nowarn_export_all]).

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
  %% InputType = example,
  InputType = actual,
  %% InputType = test1,

  {Bin, P1Y, _P2Range} =
    case InputType of
      example ->
        {<<"Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n",
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
           "Sensor at x=20, y=1: closest beacon is at x=15, y=3\n">>,
         10, {0, 20}};
      actual ->
        {input:get(15), 2000000, {0, 4000000}};
      test1 ->
        {<<"Sensor at x=0, y=0: closest beacon is at x=0, y=-3\n"
           "Sensor at x=6, y=0: closest beacon is at x=6, y=3\n"
           "Sensor at x=3, y=0: closest beacon is at x=3, y=-20\n"
           %% "Sensor at x=7, y=-5: closest beacon is at x=7, y=-12\n"
         >>, 1, {-100, 100}}
    end,

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

  part2(Input, P2Range).

part2(Input, P2Range) ->
  PL = perimeter_lines1(Input, P1Y),
  Pairs = [{L1, L2} || L1 <- PL, L2 <- PL, L1 < L2],

  Freq =
    lists:foldl(fun(Pair, Acc) ->
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

  %% Grid0 = draw_perimeter_lines(PL, #{}),

  %% Grid = lists:foldl(
  %%          fun(Pos, Acc) ->
  %%              IsPerim = maps:is_key(Pos, Grid0),
  %%              InRange =
  %%                lists:any(fun(#snb{sensor = Sensor, dist = Dist}) ->
  %%                              case dist(Sensor, Pos) of
  %%                                D when D =< Dist ->
  %%                                  true;
  %%                                _ -> false
  %%                              end
  %%                          end, Input#input.snbs),

  %%              F = maps:get(Pos, Freq, undefined),
  %%              IsSensor = sets:is_element(Pos, Input#input.sensors),
  %%              IsBeacon = sets:is_element(Pos, Input#input.beacons),

  %%              case {IsPerim, InRange, F, IsSensor, IsBeacon} of
  %%                {_, _, _, true, _} ->
  %%                  maps:put(Pos, $S, Acc);
  %%                {_, _, _, _, true} ->
  %%                  maps:put(Pos, $B, Acc);
  %%                {_, _, F0, _, _} when is_integer(F0) ->
  %%                  maps:put(Pos, F + $0, Acc);
  %%                %% {true, _, _, _, _} ->
  %%                %%   Acc;
  %%                {_, true, _, _, _} ->
  %%                  maps:put(Pos, $#, Acc);
  %%                {true, _, _, _, _} ->
  %%                  %%maps:put(Pos, 32, Acc);
  %%                  Acc;
  %%                _ ->
  %%                  maps:put(Pos, 32, Acc)
  %%              end
  %%          end, Grid0,
  %%          [{X, Y} || X <- lists:seq(-20, 30),
  %%                     Y <- lists:seq(-20, 30)]),

  %% io:format("~s~n", [grid:to_str(Grid)]),

  [{Pos, _}] = lists:filter(fun({_, N}) when N >= 4 -> true;
                               (_) -> false
                            end, maps:to_list(Freq)),
  tuning_frequency(Pos).


%% part1(Input, P1Y) ->
%%   {P1XMin, P1XMax} = {lists:min([Sx - Dist || #snb{sensor = {Sx, _}, dist = Dist} <- Input#input.snbs]),
%%                       lists:max([Sx + Dist || #snb{sensor = {Sx, _}, dist = Dist} <- Input#input.snbs])},
%%   P1 = lists:foldl(fun(X, Acc) ->
%%                        Pos = {X, P1Y},
%%                        case sets:is_element(Pos, Input#input.items_on_p1_yline) of
%%                          true -> Acc;
%%                          _ ->
%%                            case is_in_range_of_any_sensor(Pos, Input#input.snbs) of
%%                              true -> Acc + 1;
%%                              false -> Acc
%%                            end
%%                        end
%%                    end, 0, lists:seq(P1XMin, P1XMax)),
%%   P1.


%%  {?debugTime("part1", part1(Input, P1Y)),
%%   ?debugTime("part2", part2(Input, P2Range))}.

%% TODO optimize part 1: for each sensor, check how much of the given
%% Y (2000000) it intersects. This gives a (x1, x2) range. Sort these
%% ranges, and iterate from lowest to highest, jumping from x1 -> x2
%% to avoid checking every position.

draw_perimeter_lines([], Acc) ->
  Acc;
draw_perimeter_lines([{{Ax, Ay}, {Bx, By}}|Rest], Acc) ->
  Coords = lists:zip(seq(Ax, Bx),
                     seq(Ay, By)),
  Acc0 = lists:foldl(fun(Pos, Acc1) ->
                         maps:put(Pos, $*, Acc1)
                     end, Acc, Coords),
  draw_perimeter_lines(Rest, Acc0).


seq(A, A) ->
  [A];
seq(A, B) when A < B ->
  lists:seq(A, B);
seq(A, B) ->
  lists:seq(A, B, -1).


perimeter_lines1(Input, _) ->
  lists:flatten(lists:foldl(fun(#snb{sensor = Sensor, dist = Dist}, Acc) ->
                                [perimeter_lines(Sensor, Dist)|Acc]
                            end, [], Input#input.snbs)).

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

is_validx(X, {{Ax, _Ay}, {Bx, _By}}) when Ax < Bx ->
  X >= Ax andalso X =< Bx;
is_validx(X, {P1, P2}) ->
  is_validx(X, {P2, P1}).

is_validy(Y, {{_Ax, Ay}, {_Bx, By}}) when Ay < By ->
  Y >= Ay andalso Y =< By;
is_validy(Y, {P1, P2}) ->
  is_validy(Y, {P2, P1}).

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

is_in_range_of_any_sensor(Pos, SnB) ->
  lists:any(fun(#snb{sensor = S, dist = Dist}) ->
                case dist(Pos, S) of
                  D when D =< Dist -> true;
                  _ -> false
                end
            end, SnB).

tuning_frequency({X, Y}) ->
  X * 4000000 + Y.

candidates(Input, Range) ->
  lists:foldl(fun(#snb{sensor = S, dist = Dist}, Acc) ->
                  just_outside(S, Dist, Range, Range, Acc)
              end, [], Input#input.snbs).

in_range(X, {Min, Max}) ->
  X >= Min andalso X =< Max.

%% Generate the coordinates with manhattan distance D + 1 from {Cx,
%% Cy}.
just_outside({Cx, Cy} = _Center, D, Range, Range, Acc) ->
  Dist = D + 1,
  L1 = diagonal({Cx,        Cy - Dist}, {1,   1}, Dist, Range, Acc),
  L2 = diagonal({Cx + Dist, Cy},        {-1,  1}, Dist, Range, L1),
  L3 = diagonal({Cx,        Cy + Dist}, {-1, -1}, Dist, Range, L2),
  L  = diagonal({Cx - Dist, Cy},        {1,  -1}, Dist, Range, L3),
  L.

diagonal(_, _, 0, _, Acc) ->
  Acc;
diagonal({X, Y}, {Dx, Dy} = Delta, N, Range, Acc) ->
  case in_range(X, Range) andalso in_range(Y, Range) of
    true ->
      diagonal({X + Dx, Y + Dy}, Delta, N - 1, Range, [{X, Y}|Acc]);
    false ->
      diagonal({X + Dx, Y + Dy}, Delta, N - 1, Range, Acc)
  end.

dist({X0, Y0}, {X1, Y1}) ->
  abs(X0 - X1) + abs(Y0 - Y1).

%% print_coords(L) ->
%%   io:format("~s~n",
%%             [grid:to_str(
%%                lists:foldl(fun(Pos, Acc) ->
%%                                maps:put(Pos, $#, Acc)
%%                            end, #{}, L))]).

-ifdef(TEST).

day13_test() ->
  %% {4665948, 13543690671045} = ?debugTime("part1 and part2", solve())
  13543690671045 = ?debugTime("part1 and part2", solve()).

-endif.
