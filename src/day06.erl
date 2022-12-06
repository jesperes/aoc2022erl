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
find_start_marker(<<_, Next/binary>> = Bin, MarkerSize, N) ->
  case all_different(Bin, MarkerSize) of
    true ->
      N + MarkerSize;
    false ->
      find_start_marker(Next, MarkerSize, N + 1)
  end.

%% Check if the first `MarkerSize' bytes in `Bin' are different.
all_different(Bin, MarkerSize) ->
  all_different(Bin, byte_size(Bin) - MarkerSize, 0).

all_different(Bin, N, _) when byte_size(Bin) == N ->
  true;
all_different(<<C, Rest/binary>>, N, Acc) ->
  Mask = 1 bsl (C - $a),
  if Mask band Acc == 0 ->
      all_different(Rest, N, Mask bor Acc);
     true ->
      false
  end.

-ifdef(TEST).

day06_test() ->
  {1802, 3551} = solve().

-endif.
