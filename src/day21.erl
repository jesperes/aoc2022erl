-module(day21).

-export([ solve/0
        ]).

-compile([ export_all
         , nowarn_export_all
         ]).

-include_lib("eunit/include/eunit.hrl").

-define(int(X), binary_to_integer(X)).
-define(atom(X), binary_to_atom(X)).
-define(match(Subject, RE), re:run(Subject, RE, [{capture, all_but_first, binary}])).

test_input() ->
  <<"root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32">>.

solve() ->
  %% Bin = input:get(21),
  Bin = test_input(),
  solve(Bin).

solve(Bin) ->
  Lines = binary:split(Bin, <<"\n">>, [global]),

  Monkeys =
    lists:foldl(
      fun(<<>>, Acc) -> Acc;
         (Line, Acc) ->
          {match, [Monkey, Expr]} = ?match(Line, "(.*): (.*)"),
          case ?match(Expr, "(.*) (.*) (.*)") of
            {match, [Left, Op, Right]} ->
              maps:put(?atom(Monkey), {expr, ?atom(Left), ?atom(Op), ?atom(Right)}, Acc);
            nomatch ->
              maps:put(?atom(Monkey), ?int(Expr), Acc)
          end
      end, #{}, Lines),

  %% {monkey_yell(root, Monkeys),
  solve_for_humn(root, Monkeys).

monkey_yell(Monkey, Map) ->
  ?debugVal(Monkey),
  case maps:get(Monkey, Map) of
    {expr, Left, Op, Right} ->
      trunc(erlang:Op(monkey_yell(Left, Map), monkey_yell(Right, Map)));
    Num ->
      Num
  end.


%% monkey_yell2 -> {expr, humn, '==', 42}} | {expr, Num}

solve_for_humn(Monkey, Map) ->
  Expr = maps:get(Monkey, Map),
  %%?debugVal({Monkey, Expr}),
  case Expr of
    Num when is_integer(Num) ->
      Num;
    {expr, L, '+', R} when Monkey =:= root ->
      L0 = solve_for_humn(L, Map),
      R0 = solve_for_humn(R, Map),
      ?debugVal(L0),
      ?debugVal(R0),
      case {L0, R0} of
        {{humn, _, Op, R2}, R1} when is_integer(R1) andalso is_integer(R2) ->
          trunc(erlang:Op(R1, R2));
        {{humn, R2, Op, _}, R1} when is_integer(R1) andalso is_integer(R2) ->
          trunc(erlang:Op(R2, R1))
      end;

    {expr, L, '+', humn} -> {humn, Monkey, '-', solve_for_humn(L, Map)};
    {expr, humn, '+', R} -> {humn, Monkey, '-', solve_for_humn(R, Map)};
    {expr, L, '*', humn} -> {humn, Monkey, 'div', solve_for_humn(L, Map)};
    {expr, humn, '*', R} -> {humn, Monkey, 'div', solve_for_humn(R, Map)};
    {expr, L, '/', humn} -> {humn, solve_for_humn(L, Map), '/', Monkey};
    {expr, humn, '/', R} -> {humn, Monkey, '*', solve_for_humn(R, Map)};
    {expr, L, '-', humn} -> {humn, solve_for_humn(L, Map), '-', Monkey};
    {expr, humn, '-', R} -> {humn, Monkey, '-', solve_for_humn(R, Map)};

    {expr, Left, Op, Right} when is_integer(Left) andalso is_integer(Right)->
      trunc(erlang:Op(Left, Right))

  end.

%% Tests
%% =============================================================================

-ifdef(TEST).

%% ex1_test_() ->
%%   [ ?_assertEqual(5, solve_for_humn(10, {humn, '*', 2}, #{})),
%%     ?_assertEqual(20, solve_for_humn(10, {humn, '/', 2}, #{})),
%%     ?_assertEqual(8, solve_for_humn(10, {humn, '+', 2}, #{})),
%%     ?_assertEqual(12, solve_for_humn(10, {humn, '-', 2}, #{})),
%%     ?_assertEqual(5, solve_for_humn(10, {2, '*', humn}, #{})),
%%     ?_assertEqual(2, solve_for_humn(10, {20, '/', humn}, #{})),
%%     ?_assertEqual(8, solve_for_humn(10, {2, '+', humn}, #{})),
%%     ?_assertEqual(12, solve_for_humn(10, {2, '-', humn}, #{}))
%%   ].

%%solve_test() ->
%%  ?assertEqual({268597611536314, not_solved}, solve()).

ex1_test() ->
  Bin = <<"root: a + b\n"
          "a: c + humn\n"
          "c: 9\n"
          "b: 27\n"
          "humn: 5\n"
          >>,
  ?assertEqual(18, solve(Bin)).

ex2_test() ->
  Bin = <<"root: a + b\n"
          "a: humn + c\n"
          "c: 9\n"
          "b: 27\n"
          "humn: 5\n"
          >>,

  ?assertEqual(18, solve(Bin)).

ex3_test() ->
  Bin = <<"root: a + b\n"
          "a: humn * c\n"
          "c: 9\n"
          "b: 27\n"
          "humn: 5\n"
          >>,

  ?assertEqual(3, solve(Bin)).

ex4_test() ->
  Bin = <<"root: a + b\n"
          "a: c * humn\n"
          "c: 9\n"
          "b: 27\n"
          "humn: 5\n"
          >>,

  ?assertEqual(3, solve(Bin)).

ex5_test() ->
  Bin = <<"root: a + b\n"
          "a: c / humn\n"
          "c: 9\n"
          "b: 3\n"
          "humn: 5\n"
          >>,

  ?assertEqual(3, solve(Bin)).

ex6_test() ->
  Bin = <<"root: a + b\n"
          "a: c - humn\n"
          "c: 9\n"
          "b: 3\n"
          "humn: 5\n"
          >>,

  ?assertEqual(6, solve(Bin)).

-endif.
