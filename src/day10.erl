-module(day10).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-record(state, { x = 1
               , cycle = 1
               , sum = 0
               }).

-define(IS_INTERESTING_CYCLE(C),
        (C == 20 orelse
         C == 60 orelse
         C == 100 orelse
         C == 140 orelse
         C == 180 orelse
         C == 220)).

solve() ->
  Bin = input:get(10),
  Instrs = binary:split(Bin, <<"\n">>, [global]),
  lists:foldl(fun(<<>>, Acc) ->
                  Acc;
                 (<<"noop">>, #state{x = X, cycle = C, sum = Sum} = State) when ?IS_INTERESTING_CYCLE(C) ->
                  %%?debugFmt("    NOOP INTR: cycle=~p, x=~p, sum=~p, signal_strength=~p", [C, X, Sum, C * X]),
                  State#state{cycle = C + 1, sum = Sum + (C * X)};

                 (<<"noop">>, #state{x = _X, cycle = C, sum = _Sum} = State) ->
                  %%?debugFmt("NOOP NOT INTR: cycle=~p, x=~p, sum=~p", [C, X, Sum]),
                  State#state{cycle = C + 1};

                 (<<"addx ", NumB/binary>>, #state{x = X, cycle = C, sum = Sum} = State) when ?IS_INTERESTING_CYCLE(C) ->
                  Num = binary_to_integer(NumB),
                  %%?debugFmt("         INTR: cycle=~p, x=~p, sum=~p, signal_strength=~p", [C, X, Sum, C * X]),
                  State#state{cycle = C + 2, x = X + Num, sum = Sum + (C * X)};

                 (<<"addx ", NumB/binary>>, #state{x = X, cycle = C, sum = Sum} = State) when ?IS_INTERESTING_CYCLE(C + 1) ->
                  Num = binary_to_integer(NumB),
                  %%?debugFmt("         INTR: cycle=~p, x=~p, sum=~p, signal_strength=~p", [C, X, Sum, C * X]),
                  State#state{cycle = C + 2, x = X + Num, sum = Sum + ((C + 1) * X)};

                 (<<"addx ", NumB/binary>>, #state{x = X, cycle = C, sum = _Sum} = State) ->
                  Num = binary_to_integer(NumB),
                  %%?debugFmt("         INTR: cycle=~p, x=~p, sum=~p", [C, X, Sum]),
                  State#state{cycle = C + 2, x = X + Num}
                 end, #state{}, Instrs).

-ifdef(TEST).

day10_test() ->
  not_solved = solve().

-endif.
