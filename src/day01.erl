-module(day01).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

day01_test() ->
  MostCals =
    lists:reverse(
      lists:sort(
        lists:map(
          fun(ElfCals) ->
              lists:sum(
                lists:filtermap(
                  fun(<<>>) -> false;
                     (Cal) -> {true, binary_to_integer(Cal)}
                  end, binary:split(ElfCals, <<"\n">>, [global])))
          end, binary:split(input:get(1), <<"\n\n">>, [global])))),

  P1 = hd(MostCals),
  ?assertEqual(69836, P1),

  {TopThree, _} = lists:split(3, MostCals),
  P2 = lists:sum(TopThree),
  ?assertEqual(207968, P2).

-endif.
