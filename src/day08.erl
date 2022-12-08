-module(day08).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  Bin = input:get(8),
  {W, Rows} = split(Bin),
  H = length(Rows) - 1,
  count_trees(Bin, W, H).

count_trees(Bin, W, H) ->
  lists:foldl(fun({X, Y}, {Visible, ScenicScore}) ->
                  Visible0 =
                    case is_tree_visible(X, Y, Bin, W, H) of
                      true -> Visible + 1;
                      false -> Visible
                    end,
                  ScenicScore0 = max(ScenicScore, scenic_score(X, Y, Bin, W, H)),
                  {Visible0, ScenicScore0}
              end, {0, 0},
              [{X, Y} || X <- lists:seq(0, W - 1),
                         Y <- lists:seq(0, H - 1)]).

scenic_score(X, Y, Bin, W, H) ->
  TH = tree_height(Bin, W, X, Y),

  Northwards = [{X, Y0} || Y0 <- lists:seq(Y - 1, 0, -1)],
  Southwards = [{X, Y0} || Y0 <- lists:seq(Y + 1, H - 1)],
  Westwards = [{X0, Y} || X0 <-  lists:seq(X - 1, 0, -1)],
  Eastwards = [{X0, Y} || X0 <- lists:seq(X + 1, W - 1)],

  ViewDistNorth = view_dist(Northwards, Bin, W, TH, 0),
  ViewDistSouth = view_dist(Southwards, Bin, W, TH, 0),
  ViewDistWest = view_dist(Westwards, Bin, W, TH, 0),
  ViewDistEast = view_dist(Eastwards, Bin, W, TH, 0),

  ViewDistNorth * ViewDistSouth * ViewDistEast * ViewDistWest.

view_dist([], _Bin, _W, _TH, Acc) ->
  Acc;
view_dist([{X, Y}|Trees], Bin, W, TH, Acc) ->
  case tree_height(Bin, W, X, Y) of
    TH0 when TH0 >= TH ->
      %% Tree blocks the view: we can see this tree, but no
      %% further
      Acc + 1;
    _ ->
      %% Tree is lower: we can see this tree, and beyond.
      view_dist(Trees, Bin, W, TH, Acc + 1)
  end.


is_tree_visible(X, Y, Bin, W, H) ->
  TH = tree_height(Bin, W, X, Y),

  Northwards = [{X, Y0} || Y0 <- lists:seq(0, Y - 1)],
  Southwards = [{X, Y0} || Y0 <- lists:seq(Y + 1, H - 1)],
  Westwards = [{X0, Y} || X0 <- lists:seq(0, X - 1)],
  Eastwards = [{X0, Y} || X0 <- lists:seq(X + 1, W - 1)],

  lists:any(fun(TreeList) ->
                not has_blocking_trees(TreeList, Bin, W, TH)
            end,
            [Northwards,
             Southwards,
             Westwards,
             Eastwards]).

has_blocking_trees(Trees, Bin, W, TH) ->
  lists:any(fun({X, Y}) ->
                tree_height(Bin, W, X, Y) >= TH
            end, Trees).

split(Bin) ->
  Rows = [First|_] = binary:split(Bin, <<"\n">>, [global]),
  W = byte_size(First),
  {W, Rows}.

tree_height(Bin, W, X, Y) ->
  binary:at(Bin, Y * (W + 1) + X) - $0.

-ifdef(TEST).

tree_height_test() ->
  Bin = input:get(8),
  {W, _} = split(Bin),
  ?assertEqual(1, tree_height(Bin, W, 98, 0)),
  ?assertEqual(3, tree_height(Bin, W, 0, 1)),
  ?assertEqual(1, tree_height(Bin, W, 0, 98)),
  ?assertEqual(3, tree_height(Bin, W, 98, 98)).

day08_test() ->
  {1684,486540} = solve().

-endif.
