-module(day10).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-record(state, { x = 1
               , cycle = 1
               , sum = 0
               , draw_pos = 0
               , crt = #{}
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
  P1 = solve_p1(Instrs, #state{}),
  P2 = solve_p2(Instrs, #state{}),
  %% io:format("~tp~n", [P2]),
  %% io:format("~tp~n", [binary:split(P2, <<"\n">>, [global])]),
  {P1, P2}.

solve_p1(Instrs, StateIn) ->
  lists:foldl(
    fun(<<>>, State) ->
        State#state.sum;
       (<<"noop">>, #state{x = X, cycle = C, sum = Sum} = State) when ?IS_INTERESTING_CYCLE(C) ->
        State#state{cycle = C + 1, sum = Sum + (C * X)};

       (<<"noop">>, #state{x = _X, cycle = C, sum = _Sum} = State) ->
        State#state{cycle = C + 1};

       (<<"addx ", NumB/binary>>, #state{x = X, cycle = C, sum = Sum} = State) when ?IS_INTERESTING_CYCLE(C) ->
        Num = binary_to_integer(NumB),
        State#state{cycle = C + 2, x = X + Num, sum = Sum + (C * X)};

       (<<"addx ", NumB/binary>>, #state{x = X, cycle = C, sum = Sum} = State) when ?IS_INTERESTING_CYCLE(C + 1) ->
        Num = binary_to_integer(NumB),
        State#state{cycle = C + 2, x = X + Num, sum = Sum + ((C + 1) * X)};

       (<<"addx ", NumB/binary>>, #state{x = X, cycle = C, sum = _Sum} = State) ->
        Num = binary_to_integer(NumB),
        State#state{cycle = C + 2, x = X + Num}
    end, StateIn, Instrs).

solve_p2(Instrs, StateIn) ->
  lists:foldl(
    fun(Instr, #state{x = X, draw_pos = Pos, crt = CRT} = State) ->
        case Instr of
          <<>> ->
            unicode:characters_to_binary(grid:to_plain_str(State#state.crt));
          <<"noop">> ->
            CRT0 = draw(X, Pos, CRT),
            State#state{draw_pos = Pos + 1, crt = CRT0};
          <<"addx ", NumB/binary>> ->
            Num = binary_to_integer(NumB),
            CRT0 = draw(X, Pos, CRT),
            CRT1 = draw(X, Pos + 1, CRT0),
            State#state{draw_pos = Pos + 2, crt = CRT1, x = X + Num}
        end
    end, StateIn, Instrs).

draw(X, Pos, CRT) when (Pos rem 40) == X - 1;
                       (Pos rem 40) == X;
                       (Pos rem 40) == X + 1 ->
  maps:put({Pos rem 40, Pos div 40}, $█, CRT);
draw(_X, Pos, CRT) ->
  maps:put({Pos rem 40, Pos div 40}, 32, CRT).

-ifdef(TEST).

day10_test() ->
  {14060, <<"███   ██  ███  █  █ ████ █  █ ████   ██ \n"
            "█  █ █  █ █  █ █ █  █    █ █  █       █ \n"
            "█  █ █  █ █  █ ██   ███  ██   ███     █ \n"
            "███  ████ ███  █ █  █    █ █  █       █ \n"
            "█    █  █ █    █ █  █    █ █  █    █  █ \n"
            "█    █  █ █    █  █ █    █  █ ████  ██  \n"/utf8>>} = solve().

-endif.
