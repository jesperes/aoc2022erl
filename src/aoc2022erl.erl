-module(aoc2022erl).

-export([main/1]).

main([]) ->
  aoc:tabulate(aoc:timings());
main(Args) ->
  Modules =
    lists:map(fun(A) ->
                  list_to_atom(A)
              end, Args),
  aoc:tabulate(aoc:timings(Modules)).
