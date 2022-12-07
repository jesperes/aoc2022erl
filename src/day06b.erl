-module(day06b).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(to_key(Char), Char).

solve() ->
  Bin = input:get(6),
  P1 = find_start_marker(Bin, 4),
  P2 = find_start_marker(Bin, 14),
  {P1, P2}.

find_start_marker(Bin, MarkerSize) ->
  find_start_marker(Bin, -MarkerSize, 0, MarkerSize, #{}).

find_start_marker(Bin, Out, Current, MarkerSize, Map) ->
  C = binary:at(Bin, Current),
  Map0 = incr_map(C, Map),
  Map1 =
    if Out >= 0 ->
        COut = binary:at(Bin, Out),
        decr_or_delete(COut, Map0);
       true ->
        Map0
    end,
  case maps:size(Map1) of
    Size when Size >= MarkerSize ->
      Current + 1;
    _ ->
      find_start_marker(Bin, Out + 1, Current + 1, MarkerSize, Map1)
  end.

incr_map(Key, Map) ->
  maps:update_with(?to_key(Key), fun(Old) -> Old + 1 end, 1, Map).

decr_or_delete(Key, Map) ->
  case maps:get(?to_key(Key), Map) of
    1 ->
      maps:remove(?to_key(Key), Map);
    N ->
      maps:put(?to_key(Key), N - 1, Map)
  end.


-ifdef(TEST).

day06_test() ->
  {1802, 3551} = solve().

ex1_test() ->
  ?assertEqual(7, find_start_marker(<<"mjqjpqmgbljsphdztnvjfqwrcgsmlb">>, 4)).

ex2_test() ->
  ?assertEqual(26, find_start_marker(<<"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw">>, 14)).



-endif.
