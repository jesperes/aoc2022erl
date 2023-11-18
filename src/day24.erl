-module(day24).

-export([ solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-record(state, { blizzard = #{}   %% Start position of all blizzards
               , blizzards = #{}  %% Map of #{minute => Blizzard}
               , walls = #{}
               , start
               , 'end'
               , width = 0 :: integer()
               , height = 0 :: integer()
               }).

solve() ->
  Bin = input:get(24),
  State0 = parse(Bin, 0, 0, #state{}),

  %% Compute the size of the "valley", excluding the walls
  {Pos1, _} = binary:match(Bin, <<"\n">>),
  {Pos2, _} = binary:match(Bin, <<"##.#\n">>),
  Width = Pos1,
  Height = (Pos2 + 4) div Width,
  State = State0#state{width = Width - 2, height = Height - 2},

  P1 = a_star(State, 0),

  #state{start = Start, 'end' = End} = State,
  P2a = a_star(State#state{start = End, 'end' = Start}, P1),
  P2b = a_star(State, P2a),
  {P1, P2b}.

a_star(#state{start = Start, 'end' = End} = State, T0) ->
  StartNode = {Start, T0},
  OpenSet = gb_sets:from_list([{dist(Start, End), StartNode}]),
  GScore = #{StartNode => 0},
  a_star(OpenSet, GScore, State).

a_star(OpenSet, GScore, #state{'end' = End} = State) ->
  %% The search node in the A* "open set" consists of a 3-tuple
  %% {FScore, {Pos, N}} where FScore is the estimated cost of the path
  %% if it goes through this node. N is time; used to compute the
  %% position of the blizzards, since the same position reached at
  %% different times need to be explored separately. Pos is the {X, Y}
  %% position in the grid.
  {{_FScore, {NodePos, N} = Node}, OpenSet0} = gb_sets:take_smallest(OpenSet),

  if NodePos =:= End ->
      N - 1;
     true ->
      {Nbrs, State0} = get_neighbors(Node, State),

      {O1, G1} =
        lists:foldl(
          fun({NbrPos, _} = Nbr, {O, G}) ->
              case {maps:get(Node, GScore) + 1, maps:get(Nbr, GScore, inf)} of
                {T, GNbr} when T < GNbr ->
                  %% We have reached this position through a shorter
                  %% path than before, so continue exploring from
                  %% here. "T" at this position is the known distance
                  %% from the start to Nbr.
                  {gb_sets:add({_NewFScore = T + dist(NbrPos, End), Nbr}, O),
                   maps:put(Nbr, T, G)};
                _ ->
                  %% We have already reached this position on a better
                  %% path, so ignore it.
                  {O, G}
              end
          end, {OpenSet0, GScore}, Nbrs),

      a_star(O1, G1, State0)
  end.

get_neighbors({{X, Y} = _Pos, N} = _Node, #state{walls = Walls, width = Width, height = Height} = State) ->
  {BlizzardN, State0} = get_blizzard_n(State, N),
  Nbrs =
    [{{Px, Py}, N + 1}
     || {Px, Py} <- [{X + 1, Y},
                     {X - 1, Y},
                     {X, Y + 1},
                     {X, Y - 1},
                     {X, Y}],
        Px >= 0, Px < Width + 2,
        Py >= 0, Py < Height + 2,
        not maps:is_key({Px, Py}, BlizzardN),
        not maps:is_key({Px, Py}, Walls)
    ], {Nbrs, State0}.

%% Compute where the blizzards are at a given time, and cache the result.
get_blizzard_n(#state{blizzard = Blizzard,
                      blizzards = Blizzards,
                      walls = _Walls,
                      width = W,
                      height = H} = State, N) ->
  case maps:get(N, Blizzards, undefined) of
    undefined ->
      BlizzardN =
        maps:fold(
          fun(Pos, Dir, Acc) ->
              NewPos = blizzard_pos(Pos, N, W, H, Dir),
              maps:put(NewPos, true, Acc)
          end, #{}, Blizzard),
      {BlizzardN, State#state{blizzards = maps:put(N, BlizzardN, Blizzards)}};
    Map ->
      {Map, State}
  end.

%% Add Width/Height on $</$^ to avoid negative remainder
blizzard_pos({X, Y}, N, W, _H, $>) ->
  {(((X - 1) + (N rem W)) rem W) + 1, Y};
blizzard_pos({X, Y}, N, W, _H, $<) ->
  {(((X - 1) + W - (N rem W)) rem W) + 1, Y};
blizzard_pos({X, Y}, N, _W, H, $v) ->
  {X, (((Y - 1) + (N rem H)) rem H) + 1};
blizzard_pos({X, Y}, N, _W, H, $^) ->
  {X, (((Y - 1) + H - (N rem H)) rem H) + 1}.

dist({X0, Y0}, {X1, Y1}) ->
  abs(X0 - X1) + abs(Y0 - Y1).

%% Parser
parse(<<>>, _, _, State) ->
  State;
parse(<<$#, $., $#, Rest/binary>>, 0, 0, #state{walls = Wl} = State) ->
  parse(Rest, 3, 0, State#state{start = {1, 0},
                                walls = maps:merge(#{{0, 0} => $#, {2, 0} => $#}, Wl)});
parse(<<$#, $., $#, $\n>>, X, Y, #state{walls = Wl} = State) ->
  State#state{'end' = {X + 1, Y}, walls = maps:merge(#{{X, Y} => $#, {X + 2, Y} => $#}, Wl)};
parse(<<$#, Rest/binary>>, X, Y, #state{walls = Map} = State) ->
  parse(Rest, X + 1, Y, State#state{walls = maps:put({X, Y}, $#, Map)});
parse(<<$\n, Rest/binary>>, _X, Y, State) ->
  parse(Rest, 0, Y + 1, State);
parse(<<$., Rest/binary>>, X, Y, State) ->
  parse(Rest, X + 1, Y, State);
parse(<<Dir, Rest/binary>>, X, Y, #state{blizzard = Map} = State) ->
  parse(Rest, X + 1, Y, State#state{blizzard = maps:put({X, Y}, Dir, Map)}).

-ifdef(TEST).

solve_test() ->
  ?assertEqual({245, 798}, solve()).

-endif.
