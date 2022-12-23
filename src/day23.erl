%% Some common code to cut-n-paste into AoC solutions

-module(day23).

-export([ solve/0
        ]).

-compile([ export_all
         , nowarn_export_all
         ]).

-include_lib("eunit/include/eunit.hrl").

-define(NORTH, 0).
-define(SOUTH, 1).
-define(WEST, 2).
-define(EAST, 3).

%% -define(int(X), binary_to_integer(X)).
%% -define(atom(X), binary_to_atom(X)).
%% -define(match(Subject, RE), re:run(Subject, RE, [{capture, all_but_first, binary}])).

%% State record for various processing tasks
%% -record(state, {}).

%% Some commonly used types
%% -type coord() :: {X :: integer(), Y :: integer()}.
%% -type coord3d() :: {X :: integer(), Y :: integer(), Z :: integer()}.

%% 5928 too high

solve() ->
  Day = 23,
  Bin = input:get(Day),
  {W, _} = binary:match(Bin, <<"\n">>),

  Elves = lists:foldl(
            fun({Pos, _}, Acc) ->
                Coord = {Pos rem (W + 1), Pos div (W + 1)},
                maps:put(Coord, true, Acc)
            end, #{}, binary:matches(Bin, <<"#">>)),

  %% Part 1
  After10 = do_rounds(Elves, 0, 10),
  {MinX, MaxX, MinY, MaxY} = limits(After10),
  P1 = (MaxX - MinX + 1) * (MaxY - MinY + 1) - maps:size(Elves),

  %% Part 2
  {no_move, Round, _NoMoveElves} = do_rounds(After10, 10, infinity),
  P2 = Round + 1,

  %% print_elves(NoMoveElves),
  {P1, P2}.

%% print_elves(Elves) ->
%%   io:format("~s~n",
%%             [grid:to_str(
%%                sets:fold(fun(Elem, Acc) ->
%%                              maps:put(Elem, $#, Acc)
%%                          end, #{}, Elves))]).

limits(Elves) ->
  maps:fold(
    fun({X, Y}, _, {MinX, MaxX, MinY, MaxY}) ->
        {min(X, MinX),
         max(X, MaxX),
         min(Y, MinY),
         max(Y, MaxY)}
    end, {0, 0, 0, 0}, Elves).

do_rounds(Elves, N, Max) when N >= Max ->
  Elves;
do_rounds(Elves, N, Max) ->
  case do_one_round(Elves, N) of
    {false, _} ->
      {no_move, N, Elves};
    {true, Elves0} ->
      do_rounds(Elves0, N + 1, Max)
  end.

do_one_round(Elves, N) ->
  {ElfMap, MoveMap} =
    maps:fold(
      fun(Elf, _, {Map1, Map2} = Acc) ->
          case possible_moves(Elf, N, Elves) of
            [] ->
              %% No possible moves, do nothing
              Acc;
            Moves when length(Moves) == 4 ->
              %% No adjacent elves, do nothing
              Acc;
            [ProposedMove|_] ->
              %% One or more possible moves, use the first one
              {maps:put(Elf, ProposedMove, Map1),
               maps:update_with(ProposedMove, fun(Old) -> [Elf|Old] end, [Elf], Map2)}
          end
      end, {#{}, #{}}, Elves),

  NonConflicingMoves =
    maps:filter(
      fun(ElfKey, Move) ->
          ElfsMovingHere = maps:get(Move, MoveMap),
          %% ?assert(lists:member(ElfKey, ElfsMovingHere)),
          case ElfsMovingHere of
            Elfs when length(Elfs) >= 2 ->
              false;
            _ ->
              true
          end
      end, ElfMap),

  case maps:size(NonConflicingMoves) of
    0 ->
      {false, Elves};
    _ ->
      {true, maps:fold(fun(Elf, Move, ElvesIn) ->
                           E0 = sets:del_element(Elf, ElvesIn),
                           E1 = sets:add_element(Move, E0),
                           E1
                       end, Elves, NonConflicingMoves)}
  end.



possible_moves(Elf, Round, Elves) ->
  do_possible_moves(Elf, Round, 0, Elves, []).

do_possible_moves(_Elf, _Round, 4, _Elves, Moves) ->
  lists:reverse(Moves);
do_possible_moves({X, Y} = Elf, Round, N, Elves, Moves) ->
  Dir = (Round + N) rem 4,
  Adj = case Dir of
          ?NORTH -> Py = Y - 1, [{X - 1, Py}, {X, Py}, {X  + 1, Py}];
          ?SOUTH -> Py = Y + 1, [{X - 1, Py}, {X, Py}, {X  + 1, Py}];
          ?WEST  -> Px = X - 1, [{Px, Y - 1}, {Px, Y}, {Px, Y + 1} ];
          ?EAST  -> Px = X + 1, [{Px, Y - 1}, {Px, Y}, {Px, Y + 1} ]
        end,

  case lists:any(fun(Pos) -> maps:is_key(Pos, Elves) end, Adj) of
    true ->  do_possible_moves(Elf, Round, N + 1, Elves, Moves);
    false -> do_possible_moves(Elf, Round, N + 1, Elves, [move(Elf, Dir)|Moves])
  end.

move({X, Y}, ?NORTH) -> {X, Y - 1};
move({X, Y}, ?SOUTH) -> {X, Y + 1};
move({X, Y}, ?WEST) ->  {X - 1, Y};
move({X, Y}, ?EAST) ->  {X + 1, Y}.

dir(0) -> north;
dir(1) -> south;
dir(2) -> west;
dir(3) -> east.

-ifdef(TEST).

solve_test() ->
  ?assertEqual({3684, 862}, solve()).

-endif.
