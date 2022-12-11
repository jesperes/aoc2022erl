-module(day11).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(ITEMS_TAG, 1).
-define(OPFUN_TAG, 2).
-define(TRUE_TAG,  3).
-define(FALSE_TAG, 4).
-define(DIV_TAG,   5).
-define(COUNT_TAG, 6).

-define(TAG_MASK,   (16#F bsl 16)).

-define(ITEMS_MASK, (?ITEMS_TAG bsl 16)).
-define(OPFUN_MASK, (?OPFUN_TAG bsl 16)).
-define(TRUE_MASK,  (?TRUE_TAG bsl 16)).
-define(FALSE_MASK, (?FALSE_TAG bsl 16)).
-define(DIV_MASK,   (?DIV_TAG bsl 16)).
-define(COUNT_MASK, (?COUNT_TAG bsl 16)).

-define(ITEMS_KEY(N), (?ITEMS_MASK bor N)).
-define(OPFUN_KEY(N), (?OPFUN_MASK bor N)).
-define(TRUE_KEY(N),  (?TRUE_MASK  bor N)).
-define(FALSE_KEY(N), (?FALSE_MASK bor N)).
-define(DIV_KEY(N),   (?DIV_MASK   bor N)).
-define(COUNT_KEY(N), (?COUNT_MASK bor N)).

-define(SET_START_ITEMS(N, Ints, Map), maps:put(?ITEMS_KEY(Num0), Ints, Map)).
-define(SET_OPFUN(N, OpFun, Map), maps:put(?OPFUN_KEY(Num0), OpFun, Map)).
-define(SET_TRUE(N, Val, Map), maps:put(?TRUE_KEY(Num0), Val, Map)).
-define(SET_FALSE(N, Val, Map), maps:put(?FALSE_KEY(Num0), Val, Map)).
-define(SET_DIV(N, Val, Map), maps:put(?DIV_KEY(Num0), Val, Map)).
-define(INCR_COUNT(N, Map), maps:update_with(?COUNT_KEY(Num), fun(Old) -> Old + 1 end, 1, Map)).


parse(Bin) ->
  List = split(Bin, <<"\n">>),
  lists:foldl(
    fun(<<>>, Acc0) -> Acc0;
       (Line, {Num0, Acc0}) ->
        case Line of
          <<"Monkey ", Num:8, ":">> ->
            {Num - $0, Acc0};
          <<"  Starting items: ", Rest/binary>> ->
            Items = lists:map(fun(<<" ", N/binary>>) ->
                                  binary_to_integer(N);
                                 (N) ->
                                  binary_to_integer(N)
                              end, split(Rest, <<",">>)),
            {Num0, ?SET_START_ITEMS(Num0, Items, Acc0)};
          <<"  Operation: ", Rest/binary>> ->
            Op =
              case split(Rest, <<" ">>) of
                [<<"new">>, <<"=">>, <<"old">>, <<"*">>, <<"old">>] ->
                  fun(Old) -> Old * Old end;
                [<<"new">>, <<"=">>, <<"old">>, <<"*">>, OldStr] ->
                  fun(Old) -> Old * binary_to_integer(OldStr) end;
                [<<"new">>, <<"=">>, <<"old">>, <<"+">>, OldStr] ->
                  fun(Old) -> Old + binary_to_integer(OldStr) end
              end,
            {Num0, ?SET_OPFUN(Num0, Op, Acc0)};
          <<"  Test: divisible by ", Rest/binary>> ->
            {Num0, ?SET_DIV(Num0, binary_to_integer(Rest), Acc0)};
          <<"    If true: throw to monkey ", Rest/binary>> ->
            {Num0, ?SET_TRUE(Num0, binary_to_integer(Rest), Acc0)};
          <<"    If false: throw to monkey ", Rest/binary>> ->
            {Num0, ?SET_FALSE(Num0, binary_to_integer(Rest), Acc0)}
        end
    end, {0, #{}}, List).

solve() ->
  Bin = input:get(11),
  {N, Map} = parse(Bin),

  %% The divisors are all prime, so LCM is just the product of them
  LCM = maps:fold(fun(Key, Div, Acc) when Key band ?DIV_MASK == ?DIV_MASK ->
                      Div * Acc;
                     (_, _, Acc) -> Acc
                  end, 1, Map),

  P1 = simulate(Map, N, _Rounds1 = 20,    _OpFun1 = {'div', 3}),
  P2 = simulate(Map, N, _Rounds2 = 10000, _OpFun2 = {'rem', LCM}),
  {P1, P2}.

solution(Map) ->
  ItemCounts =
    maps:fold(fun(K, V, Acc) when K band ?TAG_MASK == ?COUNT_MASK ->
                  [V|Acc];
                 (_, _, Acc) ->
                  Acc
              end, [], Map),
  [X1, X2|_] = lists:reverse(lists:sort(ItemCounts)),
  X1 * X2.

simulate(Map, _N, 0, _ReduceFun) ->
  solution(Map);
simulate(Map, N, Round, {ReduceOp, ReduceNum} = ReduceFun) ->
  MapOut =
    lists:foldl(
      fun(Num, Map0) ->
          Items = maps:get(?ITEMS_KEY(Num), Map0),
          OpFun = maps:get(?OPFUN_KEY(Num), Map0),
          DivisibleBy = maps:get(?DIV_KEY(Num), Map0),
          IfTrue = maps:get(?TRUE_KEY(Num), Map0),
          IfFalse = maps:get(?FALSE_KEY(Num), Map0),

          %% Clear the monkey's items
          Map1 = maps:put(?ITEMS_KEY(Num), [], Map0),

          lists:foldl(
            fun(Item, Map2) ->
                WorryLevel = OpFun(Item),
                WorryLevel0 = if ReduceOp =:= 'div' -> WorryLevel div ReduceNum;
                                 true -> WorryLevel rem ReduceNum
                              end,
                ThrowTo = if WorryLevel0 rem DivisibleBy == 0 -> IfTrue;
                             true -> IfFalse
                          end,

                Map3 = maps:update_with(?ITEMS_KEY(ThrowTo),
                                        fun(Old) -> Old ++ [WorryLevel0] end,
                                        [WorryLevel0],
                                        Map2),

                ?INCR_COUNT(Num, Map3)
              end, Map1, Items)
      end, Map, lists:seq(0, N)),

  simulate(MapOut, N, Round - 1, ReduceFun).

split(Binary, Sep) ->
  binary:split(Binary, Sep, [global]).

-ifdef(TEST).

day11_test() ->
  {102399, 23641658401} = solve().

-endif.
