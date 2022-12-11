-module(day11).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

parse(Bin) ->
  List = split(Bin, <<"\n\n">>),
  lists:foldl(
    fun(Monkey, Acc) ->
        Lines = split(Monkey, <<"\n">>),
        M = lists:foldl(
              fun(<<>>, Acc0) -> Acc0;
                 (Line, Acc0) ->
                  [Left, Right] = split(Line, <<":">>),
                  Left0 = trim(Left),
                  Right0 = trim(Right),
                  case Left0 of
                    "Monkey " ++ Num ->
                      maps:put(num, list_to_integer(Num), Acc0);
                    "Starting items" ->
                      maps:put(items,
                               lists:map(fun(N) ->
                                             list_to_integer(string:trim(N))
                                         end, string:split(Right0, ",", all)),
                               Acc0);
                    "Operation" ->
                      Op =
                        case string:split(Right0, " ", all) of
                          ["new", "=", "old", "*", "old"] ->
                            fun(Old) -> Old * Old end;
                          ["new", "=", "old", "*", OldStr] ->
                            fun(Old) -> Old * list_to_integer(string:trim(OldStr)) end;
                          ["new", "=", "old", "+", OldStr] ->
                            fun(Old) -> Old + list_to_integer(string:trim(OldStr)) end
                        end,
                      maps:put(op, Op, Acc0);
                    "If " ++ TrueOrFalseStr ->
                      TrueOrFalse = list_to_atom(TrueOrFalseStr),
                      ?assert((TrueOrFalse =:= true) orelse
                              (TrueOrFalse =:= false)),
                      "throw to monkey " ++ MonkeyNum = Right0,
                      maps:put(TrueOrFalse, list_to_integer(MonkeyNum), Acc0);
                    "Test" ->
                      "divisible by " ++ Div = Right0,
                      maps:put(test, list_to_integer(Div), Acc0)
                  end
              end, #{}, Lines),
        maps:put(maps:get(num, M), M, Acc)
    end, #{}, List).

solve() ->
  Bin = input:get(11),
  Monkeys = parse(Bin),

  %% The divisors are all prime, so LCM is just the product of them
  LCM = maps:fold(fun(_, #{test := Div}, Acc) ->
                      Div * Acc
                  end, 1, Monkeys),

  P1 = simulate(Monkeys, #{}, 20, fun(L) -> L div 3 end),
  P2 = simulate(Monkeys, #{}, 10000, fun(L) -> L rem LCM end),
  {P1, P2}.

simulate(_Monkeys, ItemCounts, 0, _ReduceFun) ->
  [X1, X2|_] = lists:reverse(lists:sort(maps:values(ItemCounts))),
  X1 * X2;
simulate(Monkeys, ItemCounts, N, ReduceFun) ->
  {MonkeysOut, ItemCountsOut} =
    lists:foldl(
      fun(MonkeyNum, {Monkeys0, ItemCounts0}) ->
          #{items := Items,
            op := OpFun,
            true := IfTrue,
            false := IfFalse,
            test := DivisibleBy} = maps:get(MonkeyNum, Monkeys0),

          Monkeys00 =
            maps:update_with(
              MonkeyNum,
              fun(OldMonkey) -> maps:put(items, [], OldMonkey) end,
              Monkeys0),

          %% This inner fold is per-item
          lists:foldl(
            fun(Item, {Monkeys1, ItemCounts1}) ->
                WorryLevel = OpFun(Item),
                WorryLevel0 = ReduceFun(WorryLevel),
                ThrowTo = if WorryLevel0 rem DivisibleBy == 0 ->
                              IfTrue;
                             true ->
                              IfFalse
                          end,
                M0 = maps:update_with(
                       ThrowTo,
                       fun(DestMonkey) ->
                           maps:update_with(items,
                                            fun(Old) -> Old ++ [WorryLevel0] end,
                                            [WorryLevel0],
                                            DestMonkey)
                       end, Monkeys1),
                IC0 = maps:update_with(
                        MonkeyNum,
                        fun(Old) -> Old + 1 end,
                        1,
                        ItemCounts1),
                {M0, IC0}
              end, {Monkeys00, ItemCounts0}, Items)
      end, {Monkeys, ItemCounts}, lists:sort(maps:keys(Monkeys))),

  simulate(MonkeysOut, ItemCountsOut, N - 1, ReduceFun).

-ifdef(TEST).

trim(Binary) ->
  string:trim(binary_to_list(Binary)).

split(Binary, Sep) ->
  binary:split(Binary, Sep, [global]).

day11_test() ->
  {102399, 23641658401} = solve().

-endif.
