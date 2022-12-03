-module(day03).

-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").

solve() ->
  Bin = input:get(3),
  Items = binary:split(Bin, <<"\n">>, [global]),
  P1 =
    lists:foldl(
      fun(Item, Sum) ->
          Sum + set_intersection_prio(split2(Item))
      end, 0, Items),

  P2 =
    lists:foldl(
      fun({A, B, C}, Sum) ->
          Sum + set_intersection_prio([A, B, C])
      end, 0, chunks3(Items, [])),

  {P1, P2}.

binary_to_set(Bin) ->
  sets:from_list(binary_to_list(Bin)).

set_intersection_prio(Binaries) ->
  Sets = lists:map(fun binary_to_set/1, Binaries),
  [First|Rest] = Sets,
  Intersection = lists:foldl(fun sets:intersection/2, First, Rest),
  lists:foldl(
    fun(Item, Sum) ->
        Sum + prio(Item)
    end, 0, sets:to_list(Intersection)).

prio(Item) when Item >= $a andalso Item =< $z ->
  Item - $a + 1;
prio(Item) when Item >= $A andalso Item =< $Z ->
  Item - $A + 27.

split2(Item) ->
  Len = byte_size(Item),
  Half = Len div 2,
  Left = binary:part(Item, {0, Half}),
  Right = binary:part(Item, {Half, Half}),
  [Left, Right].

chunks3([], Acc) ->
  Acc;
chunks3([<<>>], Acc) ->
  Acc;
chunks3([A, B, C|Rest], Acc) ->
  chunks3(Rest, [{A, B, C}|Acc]).

-ifdef(TEST).

day03_test() ->
  {8349, 2681} = solve().

-endif.
