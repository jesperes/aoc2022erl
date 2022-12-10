-module(day09).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(START, {0, 0}).

-record(state, { rope = []
               , visited = sets:from_list([?START])
               }).

solve() ->
  Bin = input:get(9),
  Lines = binary:split(Bin, <<"\n">>, [global]),
  {simulate(Lines, 2), simulate(Lines, 10)}.

simulate(Lines, NumKnots) ->
  simulate0(Lines,
           #state{rope = lists:map(fun(_) -> ?START end,
                                   lists:seq(1, NumKnots))}).

simulate0(Lines, StateIn) ->
  #state{visited = V} =
    lists:foldl(fun(<<>>, State) -> State;
                   (<<Dir, 32, Steps/binary>>, State) ->
                    move(Dir, binary_to_integer(Steps), State)
                end, StateIn, Lines),
  sets:size(V).

move(_, 0, State) ->
  State;
move(Dir, Steps, #state{rope = [H|Rest], visited = V} = State)  ->
  NewHead = move(H, Dir),
  NewRope = [NewHead|move_step(NewHead, Rest)],
  Last = lists:nth(length(NewRope), NewRope),
  move(Dir, Steps - 1, State#state{rope = NewRope,
                                   visited = sets:add_element(Last, V)
                                  }).

move_step(_Towards, []) ->
  [];
move_step(Towards, [Head|Rest] = Rope) ->
  case is_touching(Towards, Head) of
    true ->
      Rope;
    false ->
      NewTowards = NewHead = follow(Head, Towards),
      [NewHead|move_step(NewTowards, Rest)]
  end.

is_touching({Xh, Yh}, {Xt, Yt}) ->
  DistX = abs(Xh - Xt),
  DistY = abs(Yh - Yt),
  if DistX >= 2 orelse DistY >= 2 ->
      false;
     true ->
      true
  end.

follow({Xt, Yt}, {Xh, Yh}) when Xt == Xh andalso Yh <  Yt -> {Xt, Yt - 1};
follow({Xt, Yt}, {Xh, Yh}) when Xt <  Xh andalso Yh <  Yt -> {Xt + 1, Yt - 1};
follow({Xt, Yt}, {Xh, Yh}) when Xt <  Xh andalso Yh == Yt -> {Xt + 1, Yt};
follow({Xt, Yt}, {Xh, Yh}) when Xt <  Xh andalso Yh >  Yt -> {Xt + 1, Yt + 1};
follow({Xt, Yt}, {Xh, Yh}) when Xt == Xh andalso Yh >  Yt -> {Xt, Yt + 1};
follow({Xt, Yt}, {Xh, Yh}) when Xt >  Xh andalso Yh >  Yt -> {Xt - 1, Yt + 1};
follow({Xt, Yt}, {Xh, Yh}) when Xt >  Xh andalso Yh == Yt -> {Xt - 1, Yt};
follow({Xt, Yt}, {Xh, Yh}) when Xt >  Xh andalso Yh <  Yt -> {Xt - 1, Yt - 1}.

move({X, Y}, $R) -> {X + 1, Y};
move({X, Y}, $U) -> {X, Y - 1};
move({X, Y}, $D) -> {X, Y + 1};
move({X, Y}, $L) -> {X - 1, Y}.

%% print(#state{rope = Rope, visited = _V} = State) ->
%%   [H|_] = Rope,
%%   %% Visited = lists:foldl(fun(Coord, Acc) ->
%%   %%                           maps:put(Coord, $#, Acc)
%%   %%                       end, #{},
%%   {_, Map} = lists:foldl(fun(Knot, {N, Map}) ->
%%                              {N + 1, maps:put(Knot, $0 + N, Map)}
%%                          end, {0, #{?START => $s}}, Rope),

%%   io:format("~nState = ~p~n", [State]),
%%   io:format(grid:to_str(
%%               maps:merge(Map, #{H => $H}))),
%%   io:format("~n", []).

-ifdef(TEST).

is_touching_test() ->
  ?assert(is_touching({0, 0}, {1, 0})),
  ?assert(is_touching({0, 0}, {1, 1})),
  ?assert(is_touching({0, 0}, {-1, 0})),
  ?assert(is_touching({0, 0}, {-1, -1})).

day09_test() ->
  {6311, 2482} = solve().

-endif.
