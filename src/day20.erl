-module(day20).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

-define(ENCRYPTION_KEY, 811589153).

%% Represent the "ring" of numbers as a binary, where each tuple
%% {Idx,Num} is represented using 16 bits for `Idx' and 64 bits for
%% `Num'. Some translation is needed between byte-positions and
%% "logical positions" in the ring.
-define(INDEX_BITS, 16).
-define(NUM_BITS, 64).
-define(ELEM_BITS, 80).
-define(ELEM_BYTES, 10).

solve() ->
  Numbers =
    lists:foldr(
      fun(<<>>, Acc) -> Acc;
         (Bin, Acc) ->
          [binary_to_integer(Bin)|Acc]
      end, [], binary:split(input:get(20), <<"\n">>, [global])),

  {mix_many(Numbers, 1, 1),
   mix_many(Numbers, ?ENCRYPTION_KEY, 10)}.

mix_many(Numbers, Multiplier, Rounds) ->
  List = lists:enumerate(0, lists:map(fun(N) -> N * Multiplier end, Numbers)),
  InitialRing = << <<Idx:?INDEX_BITS, Num:?NUM_BITS/signed>> || {Idx, Num} <- List >>,
  Order = InitialRing,
  RingOut =
    lists:foldl(fun(_, Ring) ->
                    mix(Order, Ring)
                end, InitialRing, lists:seq(1, Rounds)),
  sum3(RingOut).

mix(<<>>, Ring) ->
  Ring;
mix(<<Elem:?ELEM_BYTES/binary, Rest/binary>>, Ring) ->
  mix(Rest, mix1(Elem, Ring)).

mix1(<<_:?INDEX_BITS, Num:?NUM_BITS/signed>> = Elem, Ring) ->
  [A, B] = binary:split(Ring, Elem),
  AB = <<A/binary, B/binary>>,
  Len = byte_size(AB) div ?ELEM_BYTES,
  Pos = (byte_size(A) div ?ELEM_BYTES + Num) rem Len,
  InsertAt = ?IF(Pos < 0, Len + Pos, Pos) * ?ELEM_BYTES,
  A0 = binary:part(AB, 0, InsertAt),
  B0 = binary:part(AB, InsertAt, byte_size(AB) - InsertAt),
  <<A0/binary, Elem/binary, B0/binary>>.

find_zero(<<Idx:?INDEX_BITS, 0:?NUM_BITS/signed, _/binary>>) ->
  <<Idx:?INDEX_BITS, 0:?NUM_BITS/signed>>;
find_zero(<<_:?ELEM_BITS, Rest/binary>>) ->
  find_zero(Rest).

sum3(Ring) ->
  Zero = find_zero(Ring),
  {Pos, _} = binary:match(Ring, Zero),
  Len = byte_size(Ring),
  <<_:?INDEX_BITS, A:?NUM_BITS/signed>> = binary:part(Ring, (Pos + 1000 * ?ELEM_BYTES) rem Len, ?ELEM_BYTES),
  <<_:?INDEX_BITS, B:?NUM_BITS/signed>> = binary:part(Ring, (Pos + 2000 * ?ELEM_BYTES) rem Len, ?ELEM_BYTES),
  <<_:?INDEX_BITS, C:?NUM_BITS/signed>> = binary:part(Ring, (Pos + 3000 * ?ELEM_BYTES) rem Len, ?ELEM_BYTES),
  A + B + C.

-ifdef(TEST).

day20_test_() ->
  ?_assertEqual({7278, 14375678667089}, solve()).

-endif.
