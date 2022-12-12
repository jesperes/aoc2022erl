-module(day12).

-export([solve/0
        ]).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(DELTAS, [{0, 1}, {-1, 0}, {0, -1}, {1, 0}]).

solve() ->
  Bin = input:get(12),
  Lines = [First|_] = binary:split(Bin, <<"\n">>, [global]),
  W = byte_size(First),
  H = length(Lines) - 1,
  Dim = {W, H},
  {Spos, _} = binary:match(Bin, <<"S">>),
  S = pos_to_coord(Spos, Dim),
  {Epos, _} = binary:match(Bin, <<"E">>),
  E = pos_to_coord(Epos, Dim),

  %% For P1, search from S -> E
  P1 = find([S], E, Bin, Dim),

  %% For P2, search from all As to E
  AllAs = lists:map(
            fun({Pos, _}) ->
                pos_to_coord(Pos, Dim)
            end, binary:matches(Bin, <<"a">>)),
  StartList = [S] ++ AllAs,
  P2 = find(StartList, E, Bin, Dim),
  {P1, P2}.

find(StartList, Goal, Bin, Dim) ->
  OpenSet = gb_sets:from_list([{dist(S, Goal), S} || S <- StartList]),
  GScore = lists:foldl(fun(Pos, Map) ->
                           maps:put(Pos, 0, Map)
                       end, #{}, StartList),
  find(Goal, OpenSet, GScore, Bin, Dim).

find(Goal, OpenSet, GScore, Bin, Dim) ->
  {{_, Current}, OpenSet0} = gb_sets:take_smallest(OpenSet),
  if Current =:= Goal ->
      maps:get(Current, GScore);
     true ->
      Nbrs = neighbors(Current, Bin, Dim),
      {O0, G0} =
        lists:foldl(
          fun(Nbr, {OpenSet1, GScore0} = Acc) ->
              NewGScore = maps:get(Current, GScore) + 1,
              NbrGScore = maps:get(Nbr, GScore, inf),
              if NewGScore < NbrGScore ->
                  OpenSet2 = gb_sets:add({NewGScore + dist(Nbr, Goal), Nbr}, OpenSet1),
                  GScore1 = maps:put(Nbr, NewGScore, GScore0),
                  {OpenSet2, GScore1};
                 true ->
                  Acc
              end
          end, {OpenSet0, GScore}, Nbrs),

      find(Goal, O0, G0, Bin, Dim)
  end.

neighbors({X, Y} = Curr, Bin, {W, H} = Dim) ->
  Nbrs = [{X + Dx, Y + Dy} ||
           {Dx, Dy} <- ?DELTAS,
           X + Dx >= 0,
           X + Dx < W,
           Y + Dy >= 0,
           Y + Dy < H],
  ElevCurr = elevation_at(Curr, Bin, Dim),
  lists:filter(
    fun(Nbr) ->
        elevation_at(Nbr, Bin, Dim) =< ElevCurr + 1
    end, Nbrs).

dist({X, Y}, {Xg, Yg}) ->
  abs(X - Xg) + abs(Y - Yg).

pos_to_coord(Pos, {W, _}) ->
  {Pos rem (W + 1), Pos div (W + 1)}.

elevation_at({X, Y}, Bin, {W, _}) ->
  case binary:at(Bin, Y * (W + 1) + X) of
    $S -> $a;
    $E -> $z;
    E -> E
  end - $a.

-ifdef(TEST).

day11_test() ->
  {370, 363} = solve().

-endif.
