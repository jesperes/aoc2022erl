-module(day03).

-export([solve/0
        ]).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  Bin = input:get(3),
  Items = binary:split(Bin, <<"\n">>, [global]),
  process_items(Items, {0, 0}).

%% Process items in groups of three.
process_items([], Acc) ->
  Acc;
process_items([<<>>], Acc) ->
  Acc;
process_items([A, B, C|Rest], {P1, P2}) ->
  P1out = P1 + count1(A) + count1(B) + count1(C),
  P2out = P2 + count2([A, B, C]),
  process_items(Rest, {P1out, P2out}).

%% Find the sum of the priorities of the item common to the two halves
%% of each rucksack.
count1(Binary) ->
  [Left, Right] = split2(Binary),
  LeftMask = binary_to_mask(Left),
  RightMask = binary_to_mask(Right),
  mask_to_prio(LeftMask band RightMask).

%% Find the "badge" of each three-Elf group (the prio of the item
%% common to all three rucksacks.
count2([A, B, C]) ->
  MaskA = binary_to_mask(A),
  MaskB = binary_to_mask(B),
  MaskC = binary_to_mask(C),
  mask_to_prio(MaskA band MaskB band MaskC).

%% Convert a binary to a bitmap where the Nth bit is set is there is a
%% char in the binary with prio N.
binary_to_mask(<<>>) ->
  0;
binary_to_mask(<<C, Rest/binary>>) ->
  (1 bsl prio(C)) bor binary_to_mask(Rest).

%% Find the position of the single bit set in the bitmask.
mask_to_prio(Mask) ->
  trunc(math:log2(Mask)).

prio(Item) when Item =< $Z ->
   Item - $A + 27;
prio(Item) ->
  Item - $a + 1.

split2(Item) ->
  Half = byte_size(Item) div 2,
  Left = binary:part(Item, {0, Half}),
  Right = binary:part(Item, {Half, Half}),
  [Left, Right].

-ifdef(TEST).

day03_test() ->
  {8349, 2681} = solve().

-endif.
