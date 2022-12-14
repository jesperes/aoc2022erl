-module(day14).

-export([ solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(int(X), binary_to_integer(X)).
-define(START, {500, 0}).

solve() ->
  Bin = input:get(14),
  Lines = binary:split(Bin, <<"\n">>, [global]),
  Grid = lists:foldl(
           fun(Line, Acc) ->
               line_to_coords(Line, Acc)
           end, #{}, Lines),
  MaxY = lists:max(lists:map(fun({_, Y}) -> Y end, maps:keys(Grid))),
  P1 = simulate1(?START, Grid, MaxY),
  P2 = simulate2(?START, Grid, MaxY),
  {P1, P2}.

simulate1({_, Y}, Grid, MaxY) when Y > MaxY ->
  num_units(Grid);
simulate1({X, Y} = Pos, Grid, MaxY) ->
  Down = {X, Y + 1},
  case maps:get(Down, Grid, $.) of
    $. -> simulate1(Down, Grid, MaxY);
    _ ->
      DownLeft = {X - 1, Y + 1},
      case maps:get(DownLeft, Grid, $.) of
        $. -> simulate1(DownLeft, Grid, MaxY);
        _ ->
          DownRight = {X + 1, Y + 1},
          case maps:get(DownRight, Grid, $.) of
            $. -> simulate1(DownRight, Grid, MaxY);
            _ -> simulate1(?START, maps:put(Pos, $O, Grid), MaxY)
          end
      end
  end.

simulate2({_, Y} = Pos, Grid, MaxY) when Y == MaxY + 1 ->
  %% If we reach line MaxY, we hit the "infinite line" at MaxY + 2,
  %% and the sand unit comes to rest. Repeat with next unit.
  simulate2(?START, maps:put(Pos, $O, Grid), MaxY);
simulate2({X, Y} = Pos, Grid, MaxY) ->
  Down = {X, Y + 1},
  case maps:get(Down, Grid, $.) of
    $. -> simulate2(Down, Grid, MaxY);
    _ -> DownLeft = {X - 1, Y + 1},
      case maps:get(DownLeft, Grid, $.) of
        $. -> simulate2(DownLeft, Grid, MaxY);
        _ ->
          DownRight = {X + 1, Y + 1},
          case maps:get(DownRight, Grid, $.) of
            $. -> simulate2(DownRight, Grid, MaxY);
            _ when Pos =:= ?START ->
              num_units(maps:put(Pos, $O, Grid));
            _ ->
              simulate2(?START, maps:put(Pos, $O, Grid), MaxY)
          end
      end
  end.

line_to_coords(Line, Map) ->
  Points = binary:split(Line, <<" -> ">>, [global]),
  segments_to_coords(Points, Map).

segments_to_coords([_], Map) -> Map;
segments_to_coords([P1, P2|List], Map) ->
  Map0 = draw_coords(P1, P2, Map),
  segments_to_coords([P2|List], Map0).

draw_coords(P1, P2, Map) ->
  [X1, Y1] = binary:split(P1, <<",">>),
  [X2, Y2] = binary:split(P2, <<",">>),
  lists:foldl(fun(Pos, Acc) ->
                  maps:put(Pos, $#, Acc)
              end, Map,
              [{X, Y} || X <- seq(?int(X1), ?int(X2)),
                         Y <- seq(?int(Y1), ?int(Y2))]).

seq(X, X) -> [X];
seq(X, Y) when X < Y ->
  lists:seq(X, Y);
seq(X, Y) when X > Y ->
  lists:seq(Y, X).

num_units(Grid) ->
  maps:size(maps:filter(fun(_, V) -> V =:= $O end, Grid)).

-ifdef(TEST).

day13_test() ->
  {696,23610} = solve().

-endif.
