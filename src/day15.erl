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

-type coord() :: {integer(), integer()}.

-record(snb, { sensor :: coord()
             , beacon :: coord()
             , dist :: integer()
             }).

-type snb() :: #snb{}.

-record(input, { snbs = [] :: [snb()]
               , items_on_p1_yline = sets:new()
               }).

solve() ->
  InputType = actual,

  {Bin, P1Y, P2Range} =
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
        {input:get(15), 2000000, {0, 4000000}}
    end,

  Lines = binary:split(Bin, <<"\n">>, [global]),
  Input =
    lists:foldl(
      fun(<<>>, Acc) -> Acc;
         (Line, #input{ snbs = SnBs
                      , items_on_p1_yline = P1Items
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
                         , items_on_p1_yline =
                             case {Sensor, Beacon} of
                               {{_, P1Y}, {_, P1Y}} ->
                                 sets:add_element(Beacon, sets:add_element(Sensor, P1Items));
                               {{_, P1Y}, _} ->
                                 sets:add_element(Sensor, P1Items);
                               {_, {_, P1Y}} ->
                                 sets:add_element(Beacon, P1Items);
                               _ ->
                                 P1Items
                             end
                         }
          end
      end, #input{}, Lines),

  {part1(Input, P1Y),
   part2(Input, P2Range)}.

part1(Input, P1Y) ->
  {P1XMin, P1XMax} = {lists:min([Sx - Dist || #snb{sensor = {Sx, _}, dist = Dist} <- Input#input.snbs]),
                      lists:max([Sx + Dist || #snb{sensor = {Sx, _}, dist = Dist} <- Input#input.snbs])},
  P1 = lists:foldl(fun(X, Acc) ->
                       Pos = {X, P1Y},
                       case sets:is_element(Pos, Input#input.items_on_p1_yline) of
                         true -> Acc;
                         _ ->
                           case is_in_range_of_any_sensor(Pos, Input#input.snbs) of
                             true -> Acc + 1;
                             false -> Acc
                           end
                       end
                   end, 0, lists:seq(P1XMin, P1XMax)),
  P1.

part2(Input, P2Range) ->
  P2Cands = candidates(Input, P2Range),
  {value, Pos} = lists:search(fun(Pos) ->
                                  not lists:any(
                                        fun(#snb{sensor = S, dist = Dist}) ->
                                            dist(Pos, S) =< Dist
                                        end, Input#input.snbs)
                              end, P2Cands),
  P2 = tuning_frequency(Pos),
  P2.

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

print_coords(L) ->
  io:format("~s~n",
            [grid:to_str(
               lists:foldl(fun(Pos, Acc) ->
                               maps:put(Pos, $#, Acc)
                           end, #{}, L))]).

-ifdef(TEST).

day13_test_() ->
  {timeout, 20, fun() ->
                    {4665948, 13543690671045} = solve()
                end}.

-endif.
