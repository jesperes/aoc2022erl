-module(day01).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  MostCals =
    lists:reverse(
      lists:sort(
        lists:map(
          fun(ElfCals) ->
              lists:foldl(fun(<<>>, Acc) ->
                              Acc;
                             (Cal, Acc) ->
                              binary_to_integer(Cal) + Acc
                          end, 0, binary:split(ElfCals, <<"\n">>, [global]))
          end, binary:split(input:get(1), <<"\n\n">>, [global])))),
  [P1, X2, X3|_] = MostCals,
  P2 = P1 + X2 + X3,
  {P1, P2}.

-ifdef(TEST).

day01_test() ->
  ?assertEqual({69836, 207968}, solve()).

-endif.
