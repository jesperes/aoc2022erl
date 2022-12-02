-module(day01).

-export([ solve/0
        , solve_large/0
        ]).

-compile({parse_transform, ct_expand}).
-include_lib("eunit/include/eunit.hrl").

solve_large() ->
  {ok, Bin} = file:read_file("aoc_2022_day01_large_input.txt"),
  solve(Bin).

solve() ->
  solve(input:get(1)).

solve(Bin) ->
  Items = binary:split(Bin, <<"\n">>, [global]),
  State = {{0, 0, 0}, 0},
  {{A, B, C}, _} = process_input(Items, State),
  {A, A + B + C}.

process_input([], State) ->
  sort_abc(State);
process_input([<<>>|Rest], State) ->
  process_input(Rest, sort_abc(State));
process_input([Num|Rest], {TopThree, Curr}) ->
  process_input(Rest, {TopThree, Curr + binary_to_integer(Num)}).

sort_abc({{A, B, C} = TopThree, Curr}) ->
  if Curr > A -> {{Curr, A, B}, 0};
     Curr > B -> {{A, Curr, B}, 0};
     Curr > C -> {{A, B, Curr}, 0};
     true -> {TopThree, 0}
  end.

-ifdef(TEST).

day01_test() ->
  ?assertEqual({69836, 207968}, solve()).

-endif.
