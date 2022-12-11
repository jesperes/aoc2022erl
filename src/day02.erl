-module(day02).

-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").

-define(O_ROCK, $A).
-define(O_PAPER, $B).
-define(O_SCISSORS, $C).

-define(ROCK_OR_LOSE, $X).
-define(PAPER_OR_DRAW, $Y).
-define(SCISSORS_OR_WIN, $Z).

-define(LOSE, 0).
-define(DRAW, 3).
-define(WIN, 6).

-define(S_ROCK, 1).
-define(S_PAPER, 2).
-define(S_SCISSORS, 3).

solve() ->
  Bin = input:get(2),
  do_solve(Bin, {0, 0}).

do_solve(<<>>, Acc) -> Acc;

do_solve(<<?O_ROCK:8, _:8, ?ROCK_OR_LOSE:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  do_solve(Rest, {P1 + ?S_ROCK + ?DRAW, P2 + ?S_SCISSORS + ?LOSE});
do_solve(<<?O_ROCK:8, _:8, ?PAPER_OR_DRAW:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  do_solve(Rest, {P1 + ?S_PAPER + ?WIN, P2 + ?S_ROCK + ?DRAW});
do_solve(<<?O_ROCK:8, _:8, ?SCISSORS_OR_WIN:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  do_solve(Rest, {P1 + ?S_SCISSORS + ?LOSE, P2 + ?S_PAPER + ?WIN});

do_solve(<<?O_PAPER:8, _:8, ?ROCK_OR_LOSE:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  do_solve(Rest, {P1 + ?S_ROCK + ?LOSE, P2 + ?S_ROCK + ?LOSE});
do_solve(<<?O_PAPER:8, _:8, ?PAPER_OR_DRAW:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  do_solve(Rest, {P1 + ?S_PAPER + ?DRAW, P2 + ?S_PAPER + ?DRAW});
do_solve(<<?O_PAPER:8, _:8, ?SCISSORS_OR_WIN:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  do_solve(Rest, {P1 + ?S_SCISSORS + ?WIN, P2 + ?S_SCISSORS + ?WIN});

do_solve(<<?O_SCISSORS:8, _:8, ?ROCK_OR_LOSE:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  do_solve(Rest, {P1 + ?S_ROCK + ?WIN, P2 + ?S_PAPER + ?LOSE});
do_solve(<<?O_SCISSORS:8, _:8, ?PAPER_OR_DRAW:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  do_solve(Rest, {P1 + ?S_PAPER + ?LOSE, P2 + ?S_SCISSORS + ?DRAW});
do_solve(<<?O_SCISSORS:8, _:8, ?SCISSORS_OR_WIN:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  do_solve(Rest, {P1 + ?S_SCISSORS + ?DRAW, P2 + ?S_ROCK + ?WIN}).

-ifdef(TEST).

day02_test() ->
  {14297, 10498} = solve().

-endif.
