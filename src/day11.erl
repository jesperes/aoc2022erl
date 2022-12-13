-module(day11).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-record(monkey, { num
                , divisible_by
                , opfun
                , on_true
                , on_false
                }).

-record(state, { monkeys = []
               , items = #{}
               , counts = #{}
               , num
               }).

parse(Bin) ->
  List = split(Bin, <<"\n">>),
  State =
    lists:foldl(
      fun(<<>>, Acc0) -> Acc0;
         (Line, #state{monkeys = Monkeys, num = Num, items = ItemMap} = State0) ->
          case Line of
            <<"Monkey ", Num0:8, ":">> ->
              State0#state{num = Num0 - $0,
                           monkeys = [#monkey{num = Num0 - $0}|Monkeys]};
            <<"  Starting items: ", Rest/binary>> ->
              Items = lists:map(fun(<<" ", N/binary>>) ->
                                    binary_to_integer(N);
                                   (N) ->
                                    binary_to_integer(N)
                                end, split(Rest, <<",">>)),
              State0#state{items = maps:put(Num, Items, ItemMap)};
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
              [M|Ms] = Monkeys,
              State0#state{monkeys = [M#monkey{opfun = Op}|Ms]};
            <<"  Test: divisible by ", Rest/binary>> ->
              [M|Ms] = Monkeys,
              State0#state{monkeys = [M#monkey{divisible_by = binary_to_integer(Rest)}|Ms]};
            <<"    If true: throw to monkey ", Rest/binary>> ->
              [M|Ms] = Monkeys,
              State0#state{monkeys = [M#monkey{on_true = binary_to_integer(Rest)}|Ms]};
            <<"    If false: throw to monkey ", Rest/binary>> ->
              [M|Ms] = Monkeys,
              State0#state{monkeys = [M#monkey{on_false = binary_to_integer(Rest)}|Ms]}
          end
      end, #state{}, List),
  State#state{monkeys = lists:reverse(State#state.monkeys)}.

solve() ->
  Bin = input:get(11),
  State = parse(Bin),

  LCM = lists:foldl(fun(#monkey{divisible_by = Div}, Acc) ->
                        Acc * Div
                    end, 1, State#state.monkeys),

  P1 = simulate(State, _Rounds1 = 20,    fun(X) -> X div 3 end),
  P2 = simulate(State, _Rounds2 = 10000, fun(X) -> X rem LCM end),
  {P1, P2}.

solution(State) ->
  Values = maps:values(State#state.counts),
  [X1, X2|_] = lists:reverse(lists:sort(Values)),
  X1 * X2.

simulate(State, 0, _ReduceFun) ->
  solution(State);
simulate(State, Round, ReduceFun) ->
  MapOut =
    lists:foldl(
      fun(#monkey{num = Num,
                  opfun = OpFun,
                  on_true = OnTrue,
                  on_false = OnFalse,
                  divisible_by = Div},
          #state{items = Items,
                 counts = Counts} = State0) ->
          case length(maps:get(Num, Items, [])) of
            0 -> State0;
            NumItems ->
              State1 = State0#state{
                         items = maps:put(Num, [], Items),
                         counts = maps:update_with(
                                    Num,
                                    fun(Old) -> Old + NumItems end,
                                    NumItems,
                                    Counts)},

              {TrueList, FalseList} =
                lists:foldl(
                  fun(Item, {TrueList, FalseList}) ->
                      WorryLevel = ReduceFun(OpFun(Item)),
                      if WorryLevel rem Div == 0 ->
                          {[WorryLevel|TrueList], FalseList};
                         true ->
                          {TrueList, [WorryLevel|FalseList]}
                      end
                  end, {[], []}, maps:get(Num, Items)),

              #{OnTrue := TrueL,
                OnFalse := FalseL} = State1#state.items,

              State1#state{
                items = maps:merge(State1#state.items,
                                   #{OnTrue => TrueList ++ TrueL,
                                     OnFalse => FalseList ++ FalseL})}

          end
      end, State, State#state.monkeys),

  simulate(MapOut, Round - 1, ReduceFun).

split(Binary, Sep) ->
  binary:split(Binary, Sep, [global]).

-ifdef(TEST).

day11_test() ->
  {102399, 23641658401} = solve().

-endif.
