-module(day20).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

-define(ENCRYPTION_KEY, 811589153).

solve() ->
  Numbers =
    lists:foldr(
      fun(<<>>, Acc) -> Acc;
         (Bin, Acc) ->
          [binary_to_integer(Bin)|Acc]
      end, [], binary:split(input:get(20), <<"\n">>, [global])),

  {mix(Numbers, 1, 1),
   mix(Numbers, ?ENCRYPTION_KEY, 10)}.

sum3(Ring, Zero) ->
  R1 = ring:move_element_to_head(Zero, Ring),
  {Sum, _} =
    lists:foldl(fun(_, {Sum, R}) ->
                    R2 = ring:shift(-1000, R),
                    {_, A} = ring:peek(R2),
                    {A + Sum, R2}
                end, {0, R1}, [1, 2, 3]),
  Sum.

mix(Numbers, Multiplier, Rounds) ->
  Len = length(Numbers),
  List = lists:zip(lists:seq(0, Len - 1),
                   lists:map(fun(N) -> N * Multiplier end, Numbers)),
  Ring = ring:from_list(List),
  do_mix_rounds(Ring, List, Rounds).

do_mix_rounds(Ring, List, 0) ->
  {value, Zero} = lists:search(fun({_, 0}) -> true;
                                  (_) -> false
                               end, List),
  sum3(Ring, Zero);
do_mix_rounds(Ring, List, N) ->
  Len = ring:length(Ring),
  RingOut =
    lists:foldl(
      fun({_, Num} = Elem, RingIn) ->
          {_, Ring1} = ring:remove(ring:move_element_to_head(Elem, RingIn)),
          Shift = (Num + (Len - 1)) rem (Len - 1),
          ring:insert(Elem, ring:shift(-Shift, Ring1))
      end, Ring, List),
  do_mix_rounds(RingOut, List, N - 1).

-ifdef(TEST).

day20_test_() ->
  {timeout, 600, ?_assertEqual({7278, 14375678667089}, solve())}.

-endif.
