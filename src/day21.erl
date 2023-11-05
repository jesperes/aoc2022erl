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
  Bin = input:get(21),
  %% Bin = test_input(),
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

  {monkey_yell(root, Monkeys),
   solve_for_humn(root, Monkeys)}.

monkey_yell(Monkey, Map) ->
  %% ?debugVal(Monkey),
  case maps:get(Monkey, Map) of
    {expr, Left, Op, Right} ->
      trunc(erlang:Op(monkey_yell(Left, Map), monkey_yell(Right, Map)));
    Num ->
      Num
  end.

solve_for_humn(humn, _Map) ->
  humn;
solve_for_humn(Monkey, Map) ->
  Expr = maps:get(Monkey, Map),
  %%?debugVal(Monkey),
  %%?debugVal(Expr),
  case Expr of
    Num when is_integer(Num) ->
      Num;

    {expr, L, '+', R} when Monkey =:= root ->
      L0 = solve_for_humn(L, Map),
      R0 = solve_for_humn(R, Map),
      EL0 = eval(L0),
      ER0 = eval(R0),
      %%?debugVal(EL0),
      %%?debugVal(ER0),
      ?debugFmt("Reducing ~p = ~p~n", [EL0, ER0]),
      reduce(EL0, ER0);

    {expr, Left, Op, Right} when is_integer(Left) andalso is_integer(Right) ->
      trunc(erlang:Op(Left, Right));

    {expr, Left, Op, Right} ->
      L0 = solve_for_humn(Left, Map),
      R0 = solve_for_humn(Right, Map),
      {expr, L0, Op, R0}
  end.

eval(Num) when is_integer(Num) ->
  Num;
eval(humn) ->
  humn;
eval({expr, L, Op, R}) ->
  L0 = eval(L),
  R0 = eval(R),
  case {L0, R0} of
    {EL, ER} when is_integer(EL) andalso is_integer(ER) ->
      trunc(erlang:Op(EL, ER));
    {EL, ER} ->
      {expr, EL, Op, ER}
  end.

reduce(L, R) when is_integer(R) andalso is_tuple(L) -> reduce(R, L);
reduce(Num, humn) -> Num;
reduce(Num, {expr, L, '+', R}) when is_integer(L) -> reduce(Num - L, R);
reduce(Num, {expr, L, '+', R}) when is_integer(R) -> reduce(Num - R, L);
reduce(Num, {expr, L, '-', R}) when is_integer(L) -> reduce(L - Num, R);
reduce(Num, {expr, L, '-', R}) when is_integer(R) -> reduce(Num + R, L);
reduce(Num, {expr, L, '*', R}) when is_integer(L) -> reduce(Num div L, R);
reduce(Num, {expr, L, '*', R}) when is_integer(R) -> reduce(Num div R, L);
reduce(Num, {expr, L, '/', R}) when is_integer(L) -> reduce(Num * L, R);
reduce(Num, {expr, L, '/', R}) when is_integer(R) -> reduce(Num * R, L).


%% Tests
%% =============================================================================

-ifdef(TEST).

testdata_test() ->
  ?assertEqual({152, 301}, solve(test_input())).

realdata_test() ->
  ?assertEqual({268597611536314, not_solved}, solve()).

%% ex1_test() ->
%%   Bin = <<"root: a + b\n"
%%           "a: c + humn\n"
%%           "b: 27\n"
%%           "c: 18\n"
%%           "humn: 5\n"
%%           >>,
%%   ?assertEqual({50, 9}, solve(Bin)).

ex2_test() ->
  Bin = <<"root: a + b\n"
          "a: c + d\n"
          "c: 18\n"
          "b: 27\n"
          "d: e - humn\n"
          "e: 5\n"
          "humn: 5\n"
        >>,

  %% {_, _} = redbug:start("day21:reduce->return", [{msgs, 100000}]),

  ?assertEqual({45, -4}, solve(Bin)).

%% ex3_test() ->
%%   Bin = <<"root: a + b\n"
%%           "a: c * humn\n"
%%           "c: 9\n"
%%           "b: 27\n"
%%           "humn: 5\n"
%%         >>,
%%   ?assertEqual({72, 3}, solve(Bin)).

%% ex4_test() ->
%%   Bin = <<"root: a + b\n"
%%           "a: c / humn\n"
%%           "c: 9\n"
%%           "b: 3\n"
%%           "humn: 5\n"
%%         >>,

%%   ?assertEqual({4, 3}, solve(Bin)).

%% ex5_test() ->
%%   Bin = <<"root: a + b\n"
%%           "a: humn + c\n"
%%           "b: 27\n"
%%           "c: 18\n"
%%           "humn: 5\n"
%%           >>,
%%   ?assertEqual({50, 9}, solve(Bin)).


%% ex6_test() ->
%%   Bin = <<"root: a + b\n"
%%           "a: humn - c\n"
%%           "b: 27\n"
%%           "c: 18\n"
%%           "humn: 5\n"
%%           >>,
%%   ?assertEqual({14, 45}, solve(Bin)).

-endif.
