-module(day13).

-export([ solve/0
        , eval/1
        , compare/2
        ]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  Bin = input:get(13),
  Lines = binary:split(Bin, <<"\n\n">>, [global]),
  {_, P1} = lists:foldl(
              fun(Pair, {Index, Acc}) ->
                  [First, Second] = binary:split(Pair, <<"\n">>),
                  E1 = eval(First),
                  E2 = eval(Second),
                  case compare(E1, E2) of
                    true ->
                      {Index + 1, Acc + Index};
                    false ->
                      {Index + 1, Acc}
                  end
              end, {1, 0}, Lines),

  AllPackets = lists:foldl(fun(<<>>, Acc) -> Acc;
                              (Packet, Acc) ->
                               [eval(Packet)|Acc]
                           end, [], binary:split(Bin, <<"\n">>, [global])),
  Sorted = lists:sort(fun compare/2, AllPackets ++ [[[2]], [[6]]]),
  {I1, I2} = find_divider_packets(Sorted, 1, 0),
  P2 = I1 * I2,
  {P1, P2}.

find_divider_packets([P|Ps], Index, _) when P == [[2]] ->
  find_divider_packets(Ps, Index + 1, Index);
find_divider_packets([P|_Ps], Index, I1) when P == [[6]] ->
  {I1, Index};
find_divider_packets([_|Ps], Index, Acc) ->
  find_divider_packets(Ps, Index + 1, Acc).

compare(X, Y) when is_integer(X) andalso
                   is_integer(Y) ->
  if X == Y ->
      continue;
     X < Y ->
      true;
     true ->
      false
  end;
compare(X, Y) when is_integer(X) andalso is_list(Y) ->
  compare([X], Y);
compare(X, Y) when is_list(X) andalso is_integer(Y) ->
  compare(X, [Y]);
compare([], []) ->
  continue;
compare([], Y) when is_list(Y) ->
  true;
compare(X, []) when is_list(X) ->
  false;
compare(X, X) when is_list(X) ->
  continue;
compare([X|Xs], [Y|Ys]) ->
  case compare(X, Y) of
    true ->
      true;
    continue ->
      compare(Xs, Ys);
    false ->
      false
  end.

eval(Bin) ->
  Str = binary_to_list(Bin) ++ ".",
  {ok, Tokens, _} = erl_scan:string(Str),
  {ok, ExprList} = erl_parse:parse_term(Tokens),
  ExprList.

-ifdef(TEST).

day13_test() ->
  {5198, 22344} = solve().

-endif.
