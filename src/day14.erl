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
  P1 = simulate(?START, Grid, MaxY, p1),
  P2 = simulate(?START, Grid, MaxY, p2),
  {P1, P2}.

simulate({_, Y}, Grid, MaxY, p1) when Y > MaxY ->
  num_units(Grid);
simulate({_, Y} = Pos, Grid, MaxY, p2) when Y == MaxY + 1 ->
  simulate(?START, maps:put(Pos, $O, Grid), MaxY, p2);
simulate({X, Y} = Pos, Grid, MaxY, Part) ->
  Down = {X, Y + 1},
  case maps:get(Down, Grid, $.) of
    $. -> simulate(Down, Grid, MaxY, Part);
    _ ->
      DownLeft = {X - 1, Y + 1},
      case maps:get(DownLeft, Grid, $.) of
        $. -> simulate(DownLeft, Grid, MaxY, Part);
        _ ->
          DownRight = {X + 1, Y + 1},
          case maps:get(DownRight, Grid, $.) of
            $. -> simulate(DownRight, Grid, MaxY, Part);
            _ when Pos =:= ?START andalso Part =:= p2 ->
              num_units(maps:put(Pos, $O, Grid));
            _ ->
              simulate(?START, maps:put(Pos, $O, Grid), MaxY, Part)
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

day14_test() ->
  {696,23610} = solve().

-endif.
