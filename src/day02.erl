-module(day02).

-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").

-define(OPPONENT_ROCK, $A).
-define(OPPONENT_PAPER, $B).
-define(OPPONENT_SCISSORS, $C).

-define(PLAYER_ROCK, $X).
-define(PLAYER_PAPER, $Y).
-define(PLAYER_SCISSORS, $Z).

-define(EXPECTED_TO_LOSE, $X).
-define(EXPECTED_TO_DRAW, $Y).
-define(EXPECTED_TO_WIN, $Z).


%% meaning of X/Y/Z in part 1
-type player_choice() :: ?PLAYER_ROCK |
                         ?PLAYER_PAPER |
                         ?PLAYER_SCISSORS.

%% meaning of X/Y/Z in part 2
-type expected_outcome() :: ?EXPECTED_TO_DRAW |
                            ?EXPECTED_TO_WIN |
                            ?EXPECTED_TO_LOSE.

-type opponent_choice() :: ?OPPONENT_PAPER |
                           ?OPPONENT_ROCK |
                           ?OPPONENT_SCISSORS.
-type score() :: 1..3.
-type outcome_score() :: 0 | 3 | 6.
-type round_score() :: 1..9.
-type outcome() :: draw | player | opponent.

-spec score(Choice :: player_choice()) -> score().
score(?PLAYER_ROCK) -> 1;
score(?PLAYER_PAPER) -> 2;
score(?PLAYER_SCISSORS) -> 3.

-spec outcome(OpponentChoice :: opponent_choice(),
              PlayerChoice :: player_choice()) ->
        outcome().
outcome(?OPPONENT_ROCK, ?PLAYER_ROCK)         -> draw;
outcome(?OPPONENT_ROCK, ?PLAYER_PAPER)        -> player;
outcome(?OPPONENT_ROCK, ?PLAYER_SCISSORS)     -> opponent;

outcome(?OPPONENT_PAPER, ?PLAYER_ROCK)        -> opponent;
outcome(?OPPONENT_PAPER, ?PLAYER_PAPER)       -> draw;
outcome(?OPPONENT_PAPER, ?PLAYER_SCISSORS)    -> player;

outcome(?OPPONENT_SCISSORS, ?PLAYER_ROCK)     -> player;
outcome(?OPPONENT_SCISSORS, ?PLAYER_PAPER)    -> opponent;
outcome(?OPPONENT_SCISSORS, ?PLAYER_SCISSORS) -> draw.

-spec outcome_score(Outcome :: outcome() | expected_outcome()) -> outcome_score().
outcome_score(opponent) -> 0;
outcome_score(draw) -> 3;
outcome_score(player) -> 6;
outcome_score(?EXPECTED_TO_LOSE) -> 0;
outcome_score(?EXPECTED_TO_DRAW) -> 3;
outcome_score(?EXPECTED_TO_WIN) -> 6.

-spec score_part1(OpponentChoice :: opponent_choice(),
                  PlayerChoice :: player_choice()) -> round_score().
score_part1(OpponentChoice, PlayerChoice) ->
  score(PlayerChoice) + outcome_score(outcome(OpponentChoice, PlayerChoice)).

-spec score_part2(OpponentChoice :: opponent_choice(),
                  ExpectedOutcome :: expected_outcome()) -> round_score().
score_part2(Opp, ExpectedOutcome) ->
  PlayerChoice =
    case ExpectedOutcome of
      ?EXPECTED_TO_DRAW when Opp =:= ?OPPONENT_ROCK ->     ?PLAYER_ROCK;
      ?EXPECTED_TO_DRAW when Opp =:= ?OPPONENT_PAPER ->    ?PLAYER_PAPER;
      ?EXPECTED_TO_DRAW when Opp =:= ?OPPONENT_SCISSORS -> ?PLAYER_SCISSORS;

      ?EXPECTED_TO_WIN  when Opp =:= ?OPPONENT_ROCK ->     ?PLAYER_PAPER;
      ?EXPECTED_TO_WIN  when Opp =:= ?OPPONENT_PAPER ->    ?PLAYER_SCISSORS;
      ?EXPECTED_TO_WIN  when Opp =:= ?OPPONENT_SCISSORS -> ?PLAYER_ROCK;

      ?EXPECTED_TO_LOSE when Opp =:= ?OPPONENT_ROCK ->     ?PLAYER_SCISSORS;
      ?EXPECTED_TO_LOSE when Opp =:= ?OPPONENT_PAPER ->    ?PLAYER_ROCK;
      ?EXPECTED_TO_LOSE when Opp =:= ?OPPONENT_SCISSORS -> ?PLAYER_PAPER
    end,
  score(PlayerChoice) + outcome_score(ExpectedOutcome).

solve() ->
  Bin = input:get(2),
  do_solve(Bin, {0, 0}).

do_solve(<<>>, Acc) -> Acc;
do_solve(<<Opp:8, _:8, Pl:8, _:8, Rest/binary>>, {Score1, Score2}) when is_binary(Rest) ->
  do_solve(Rest, {Score1 + score_part1(Opp, Pl),
                  Score2 + score_part2(Opp, Pl)}).

-ifdef(TEST).

day02_test() ->
  {14297, 10498} = solve().

-endif.
