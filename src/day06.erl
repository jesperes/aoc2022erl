-module(day06).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  Bin = input:get(6),
  P1 = find_start_marker(Bin, 4, 0),
  P2 = find_start_marker(Bin, 14, 0),
  {P1, P2}.

find_start_marker(<<>>, _MarkerSize, 0) ->
  not_found;
find_start_marker(Bin, MarkerSize, N) ->
  {<<_, RestMarker/binary>> = Marker, Rest} = erlang:split_binary(Bin, MarkerSize),
  case all_different(Marker) of
    true ->
      N + MarkerSize;
    false ->
      find_start_marker(<<RestMarker/binary, Rest/binary>>, MarkerSize, N + 1)
  end.

all_different(Binary) ->
  all_different(Binary, 0).

all_different(<<>>, _) -> true;
all_different(<<C, Rest/binary>>, Acc) ->
  Mask = 1 bsl (C - $a),
  if Mask band Acc == 0 ->
      all_different(Rest, Mask bor Acc);
     true -> false
  end.

-ifdef(TEST).

day06_test() ->
  {1802, 3551} = solve().

d1_test() ->
  ?assertEqual(5, find_start_marker(<<"bvwbjplbgvbhsrlpgdmjqwftvncz">>, 4, 0)).

-endif.
