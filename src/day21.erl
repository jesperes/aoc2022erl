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

%% State record for various processing tasks
%% -record(state, {}).

solve() ->
  %% Day = 21,
  %% Bin = input:get(Day),
  Bin = <<"root: pppw + sjmn
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
hmdt: 32">>,

  _Lines = binary:split(Bin, <<"\n">>, [global]).

   %% Monkeys = lists:foldl(
   %%            fun(<<>>, Acc) -> Acc;
   %%               (Line, Acc) ->
   %%                {match, [Monkey, Expr]} = ?match(Line, "(.*): (.*)"),
   %%                case ?match(Expr, "(.*) (.*) (.*)") of
   %%                  {match, [Left, Op, Right]} ->
   %%                    maps:put(?atom(Monkey), {expr, ?atom(Left), ?atom(Op), ?atom(Right)}, Acc);
   %%                  nomatch ->
   %%                    maps:put(?atom(Monkey), ?int(Expr), Acc)
   %%                end
   %%            end, #{}, Lines).
  %% {monkey_yell(root, Monkeys),
  %% monkey_yell2(root, Monkeys).

monkey_yell(Monkey, Map) ->
  case maps:get(Monkey, Map) of
    {expr, Left, Op, Right} ->
      trunc(erlang:Op(monkey_yell(Left, Map), monkey_yell(Right, Map)));
    Num ->
      Num
  end.


%% monkey_yell2(humn, _) -> "humn";
%% monkey_yell2(Monkey, Map) ->
%%   case maps:get(Monkey, Map) of
%%     {expr, Left, Op, Right} ->
%%       L = monkey_yell2(Left, Map),
%%       R = monkey_yell2(Right, Map),
%%       case is_integer(L) andalso is_integer(R) of
%%         true ->
%%           trunc(erlang:Op(L, R));
%%         false ->
%%           ["(",
%%            maybe_integer_to_list(L),
%%            atom_to_list(Op),
%%            maybe_integer_to_list(R),
%%            ")"]
%%       end;
%%     Num when is_integer(Num) ->
%%       Num
%%   end.

%% monkey_yell2(root, Map) ->
%%   {expr, Left, _, Right} = maps:get(root, Map),
%%   L = monkey_yell2(Left, Map),
%%   R = monkey_yell2(Right, Map),
%%   case {L, Op, R} of
%%     {{humn, LH}, Op, R} ->
%%       %% Left side contains human



%% monkey_yell2(humn, _) -> "humn";
%% monkey_yell2(Monkey, Map) ->
%%   case maps:get(Monkey, Map) of
%%     {expr, Left, Op, Right} ->
%%       L = monkey_yell2(Left, Map),
%%       R = monkey_yell2(Right, Map),
%%       case is_integer(L) andalso is_integer(R) of
%%         true -> trunc(erlang:Op(L, R));
%%         false when is_integer(L) -> {L, Op, {humn, R}};
%%         false when is_integer(R) -> {{humn, L}, Op, R}
%%       end;
%%     Num when is_integer(Num) ->
%%       Num
%%   end.

maybe_integer_to_list(N) when is_integer(N) ->
  integer_to_list(N);
maybe_integer_to_list(N) ->
  N.

%% Tests
%% =============================================================================

-ifdef(TEST).

solve_test() ->
  ok.
  %% ?assertEqual(not_solved, solve()).

-endif.
