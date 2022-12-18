-module(day18).

-compile([export_all, nowarn_export_all]).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

%% Maps seems to outperform sets in this case.
-define(set_from_list(L), maps:from_list(lists:zip(L, lists:duplicate(length(L), true)))).
-define(set_new, #{}).
-define(set_is_elem(Elem, Set), maps:is_key(Elem, Set)).
-define(set_add_elem(Elem, Set), maps:put(Elem, true, Set)).

solve() ->
  Bin = input:get(18),
  Lines = binary:split(Bin, <<"\n">>, [global]),
  Cubes =
    lists:foldl(fun(<<>>, Acc) -> Acc;
                   (Line, Acc) ->
                    [X, Y, Z] = binary:split(Line, <<",">>, [global]),
                    [{ binary_to_integer(X)
                     , binary_to_integer(Y)
                     , binary_to_integer(Z)}|Acc]
                end, [], Lines),

  Pairs = [{C1, C2} || C1 <- Cubes,
                       C2 <- Cubes,
                       C1 < C2],

  TotalSurfaceArea = length(Cubes) * 6,
  P1 = lists:foldl(fun({C1, C2}, Area) ->
                       case adjacent(C1, C2) of
                         true -> Area - 2;
                         false -> Area
                       end
                   end, TotalSurfaceArea, Pairs),
  P2 = solve2(Cubes),
  {P1, P2}.

solve2(Cubes) ->
  Lava = ?set_from_list(Cubes),
  Start = {0, 0, 0},
  Init = {0, ?set_new},
  {SurfaceArea, _Water} = fill(Start, Init, Lava),
  SurfaceArea.

fill({X, Y, Z}, Acc, _Lava) when
    X < -1 ; X > 20 ; Y < -1 ; Y > 20 ; Z < -1 ; Z > 20 ->
  Acc;
fill({X, Y, Z} = Cube, {AreaIn, WaterIn} = Acc, Lava) ->
  case ?set_is_elem(Cube, Lava) of
    true ->
      %% We have reached a lava cube, only increase surface area
      {AreaIn + 1, WaterIn};
    false ->
      case ?set_is_elem(Cube, WaterIn) of
        true -> Acc;
        false ->
          %% New unfilled cube reached, fill it with water and
          %% recurse. Optimization: unrolling the fold, unclear gain.
          Acc0 = {AreaIn, ?set_add_elem(Cube, WaterIn)},
          Acc1 = fill({X + 1, Y, Z}, Acc0, Lava),
          Acc2 = fill({X - 1, Y, Z}, Acc1, Lava),
          Acc3 = fill({X, Y + 1, Z}, Acc2, Lava),
          Acc4 = fill({X, Y - 1, Z}, Acc3, Lava),
          Acc5 = fill({X, Y, Z + 1}, Acc4, Lava),
          Acc6 = fill({X, Y, Z - 1}, Acc5, Lava),
          Acc6
      end
  end.

%% Two cubes are adjacent if their manhattan distance is exactly 1.
adjacent({A, B, C}, {D, E, F}) ->
  (abs(A - D) + abs(B - E) + abs(C - F)) == 1.

adjacent_cubes({X, Y, Z}) ->
  [{X + 1, Y,     Z    },
   {X - 1, Y,     Z    },
   {X,     Y + 1, Z    },
   {X,     Y - 1, Z    },
   {X,     Y,     Z + 1},
   {X,     Y,     Z - 1}].


-ifdef(TEST).

day18_test() ->
  {3530, 2000} = solve().

-endif.
