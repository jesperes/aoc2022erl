-module(day08).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(DELTAS, [{0, -1}, {0, 1}, {-1, 0}, {1, 0}]).
-define(VALID_COORD(X, Y, W, H),
        ((X >= 0)
         andalso (X < W)
         andalso (Y < H)
         andalso (Y >= 0))).

solve() ->
  Bin = input:get(8),
  Dim = get_dimensions(Bin),
  count_trees(Bin, Dim).

foldxy(Fun, Init, {W, H} = _Dim) ->
  foldrange(fun(Y, Acc) ->
                foldrange(fun(X, InnerAcc) ->
                              Fun({X, Y}, InnerAcc)
                          end, Acc, 0, W)
            end, Init, 0, H).

foldrange(_Fun, Acc, Curr, Max) when Curr == Max ->
  Acc;
foldrange(Fun, Acc, Curr, Max) ->
  foldrange(Fun, Fun(Curr, Acc), Curr + 1, Max).

count_trees(Bin, Dim) ->
  foldxy(fun(Pos, {Visible, ScenicScore}) ->
             Visible0 =
               case is_tree_visible(Pos, Bin, Dim) of
                 true -> Visible + 1;
                 false -> Visible
               end,
             ScenicScore0 = max(ScenicScore, scenic_score(Pos, Bin, Dim)),
             {Visible0, ScenicScore0}
         end, {0, 0}, Dim).

scenic_score(Pos, Bin, Dim) ->
  TH = tree_height(Bin, Dim, Pos),
  lists:foldl(fun(Delta, Acc) ->
                  Acc * view_dist(incr(Pos, Delta), Delta, Bin, Dim, TH, 0)
              end, 1, ?DELTAS).

view_dist({X, Y}, _Delta, _Bin, {W, H}, _TH, Acc) when not ?VALID_COORD(X, Y, W, H) ->
  Acc;
view_dist(Pos, Delta, Bin, Dim, TH, Acc) ->
  case tree_height(Bin, Dim, Pos) of
    TH0 when TH0 >= TH ->
      %% Tree blocks the view: we can see this tree, but no
      %% further
      Acc + 1;
    _ ->
      %% Tree is lower: we can see this tree, and beyond.
      view_dist(incr(Pos, Delta), Delta, Bin, Dim, TH, Acc + 1)
  end.

is_tree_visible(Pos, Bin, Dim) ->
  TH = tree_height(Bin, Dim, Pos),
  lists:any(fun(Delta) ->
                not has_blocking_trees(incr(Pos, Delta), Delta, Bin, Dim, TH)
            end,
            ?DELTAS).

has_blocking_trees({X, Y}, _Delta, _Bin, {W, H}, _TH) when not ?VALID_COORD(X, Y, W, H) ->
  false;
has_blocking_trees(Pos, Delta, Bin, Dim, TH) ->
  case tree_height(Bin, Dim, Pos) of
    TH0 when TH0 >= TH ->
      true;
    _ ->
      has_blocking_trees(incr(Pos, Delta), Delta, Bin, Dim, TH)
  end.

incr({X, Y}, {Dx, Dy}) ->
  {X + Dx, Y + Dy}.

get_dimensions(Bin) ->
  Rows = [First|_] = binary:split(Bin, <<"\n">>, [global]),
  W = byte_size(First),
  H = length(Rows) - 1,
  {W, H}.

tree_height(Bin, {W, _H}, {X, Y}) ->
  binary:at(Bin, Y * (W + 1) + X) - $0.

-ifdef(TEST).

tree_height_test() ->
  Bin = input:get(8),
  Dim = get_dimensions(Bin),
  ?assertEqual(1, tree_height(Bin, Dim, {98, 0})),
  ?assertEqual(3, tree_height(Bin, Dim, {0, 1})),
  ?assertEqual(1, tree_height(Bin, Dim, {0, 98})),
  ?assertEqual(3, tree_height(Bin, Dim, {98, 98})).

day08_test() ->
  {1684,486540} = solve().

-endif.
