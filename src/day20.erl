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
          Ring0 = ring:move_element_to_head(Elem, RingIn),
          {_, Ring1} = ring:remove(Ring0),
          Shift = (Num + (Len - 1)) rem (Len - 1),
          Ring2 = ring:shift(-Shift, Ring1),
          Ring3 = ring:insert(Elem, Ring2),
          Ring3
      end, Ring, List),
  do_mix_rounds(RingOut, List, N - 1).

-ifdef(TEST).

%% mix_test() ->
%%   Numbers = [1, 2, -3, 3, -2, 0, 4],
%%   {Ring, List} = make_ring(Numbers),
%%   Ring1 = mix(Ring, List),
%%   Ring2 = ring:move_element_to_head({0, 1}, Ring1),
%%   {_, L} = lists:unzip(ring:to_list(Ring2)),
%%   ?assertEqual([1, 2, -3, 4, 0, 3, -2], L).

%% sum3_test() ->
%%   L = [1, 2, -3, 4, 0, 3, -2],
%%   {Ring, List} = make_ring(L),
%%   {value, Zero} = lists:search(fun({_, 0}) -> true;
%%                                   (_) -> false
%%                                end, List),
%%   ?debugVal(Zero),
%%   ?debugVal(Ring),
%%   ?assertEqual(3, sum3(Ring, Zero)).

day20_test_() ->
  {timeout, 600, ?_assertEqual({7278, 14375678667089}, solve())}.

-endif.
