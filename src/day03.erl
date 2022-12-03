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
      fun(Group, Sum) ->
          Sum + set_intersection_prio(Group)
      end, 0, chunks3(Items, [])),

  {P1, P2}.

binary_to_set(Bin) ->
  sets:from_list(binary_to_list(Bin)).

set_intersection_prio([B1, B2]) ->
  set_prio(sets:intersection(binary_to_set(B1),
                             binary_to_set(B2)));
set_intersection_prio([B1, B2, B3]) ->
  set_prio(sets:intersection(
             binary_to_set(B1),
             sets:intersection(binary_to_set(B2),
                               binary_to_set(B3)))).

set_prio(Set) ->
  case sets:is_empty(Set) of
    true -> 0;
    false ->
      [SetItem] = sets:to_list(Set),
      prio(SetItem)
  end.

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
  chunks3(Rest, [[A, B, C]|Acc]).

-ifdef(TEST).

day03_test() ->
  {8349, 2681} = solve().

-endif.
