-module(day17).

-export([solve/0]).

%% -compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-type coord() :: {Y :: integer(), X :: integer()}.

-record(rock, { name :: atom()
              , coords :: [coord()]
              , width :: integer()
              , height :: integer()
              }).

%% y coordinate grows upwards
-define(ROCKS, { #rock{name = minus, width = 4, height = 1, coords = [{0,0}, {1,0}, {2,0}, {3,0}]}
               , #rock{name = plus,  width = 3, height = 3, coords = [{1,0}, {0,1}, {1,1}, {2,1}, {1,2}]}
               , #rock{name = corner, width = 3, height = 3, coords = [{0,0}, {1,0}, {2,0}, {2,1}, {2,2}]}
               , #rock{name = vertical, width = 1, height = 4, coords = [{0,0}, {0,1}, {0,2}, {0,3}]}
               , #rock{name = cube, width = 2, height = 2, coords = [{0,0}, {0,1}, {1,0}, {1,1}]}
               }).

-define(CAVE_WIDTH, 7).
-define(INITIAL_OFFSET, 2).
-define(INITIAL_DROP_HEIGHT, 3).
-define(JET_LEFT, $<).
-define(JET_RIGHT, $>).

-record(cave, { current_rock_num = 0 :: integer()
              , num_remaining_rocks = undefined :: integer()
              , left_offset = ?INITIAL_OFFSET :: integer()
              , height = ?INITIAL_DROP_HEIGHT + 1 :: integer()
              , jets = undefined :: binary()
              , orig_jets = undefined :: binary()
              , dropped_rocks = sets:new() :: sets:set()
              , top = 0 :: integer()
              }).

-define(NUM_ROCKS_PART1, 2022).
-define(NUM_ROCKS_PART2, 1_000_000_000_000).

%% Part 2 musings: look for cycles

%% This shape is a sequence of corner, vertical, cube, minus, plus
%% (only shown in column 5)

%% 37    |.#####.|
%% 36    |....#..|
%% 35    |....#..|
%% 34    |....#..|
%% 33    |....#..|
%% 32    |.##.#..|
%% 31    |.##.#..|
%% 30    |..###..|

solve() ->
  solve(?NUM_ROCKS_PART1).

solve(N) ->
  Bin = input:get(17),
  %% Bin = <<">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\n">>,
  {drop_rocks(Bin, N), tbd}.

drop_rocks(Bin, N) ->
  BottomLine = sets:from_list([{0,0}, {1,0}, {2,0}, {3,0}, {4,0}, {5,0}, {6,0}]),
  Cave = #cave{ num_remaining_rocks = N
              , jets = Bin
              , orig_jets = Bin
              , dropped_rocks = BottomLine
              },
  do_drop_rocks(Cave).

do_drop_rocks(#cave{num_remaining_rocks = 0} = Cave) ->
  Cave#cave.top;
do_drop_rocks(#cave{jets = <<"\n">>, orig_jets = OrigJets} = Cave) ->
  %% Restart the list of jets
  do_drop_rocks(Cave#cave{jets = OrigJets});
do_drop_rocks(#cave{height = Height} = Cave) when Height >= 0 ->
  drop_one_rock(Cave).

drop_one_rock(#cave{ jets = <<Jet, _/binary>>
                   , current_rock_num = CurrentRockNum
                   } = Cave) ->
  Rock = get_rock(CurrentRockNum, ?ROCKS),
  Cave1 = maybe_move(Jet, Rock, Cave),
  Cave2 = maybe_come_to_rest_or_fall_one_step(Rock, Cave1),
  do_drop_rocks(Cave2).

%% Make sure we aren't moving sideways into the wall, or into an
%% existing rock.
maybe_move(Wind, Rock, #cave{height = Height, left_offset = Offset} = Cave) ->
  NewOffset = move(Wind, Offset, Rock),
  case lists:any(
         fun(RockCoord) ->
             CaveCoord = rock_to_cave_coords(RockCoord, NewOffset, Height),
             sets:is_element(CaveCoord, Cave#cave.dropped_rocks)
         end, Rock#rock.coords) of
    true ->
      Cave;
    false ->
      Cave#cave{left_offset = NewOffset}
  end.

maybe_come_to_rest_or_fall_one_step(
  Rock,
  #cave{ jets = <<_, Rest/binary>>
       , current_rock_num = CurrentRockNum
       , left_offset = Offset
       , num_remaining_rocks = NumRemainingRocks
       , height = Height
       , top = Top
       , dropped_rocks = DroppedRocks
       } = Cave) ->

  ShouldComeToRest =
    lists:any(
      fun({X, Y}) ->
          RockCoord = {X, Y - 1},
          CaveCoord = rock_to_cave_coords(RockCoord, Offset, Height),
          sets:is_element(CaveCoord, DroppedRocks)
      end, Rock#rock.coords),

  %% We need to do two passes over the profile here, one to find out
  %% if the rock should come to rest, another one to update the
  %% profile.
  case ShouldComeToRest of
    true ->
      CaveCoordsOfRock =
        lists:map(fun(Pos) ->
                      rock_to_cave_coords(Pos, Offset, Height)
                  end, Rock#rock.coords),

      NewDroppedRocks =
        sets:union(sets:from_list(CaveCoordsOfRock),
                   Cave#cave.dropped_rocks),

      NewTop = lists:foldl(fun({_, Y}, Max) when Y > Max -> Y;
                              (_, Max) -> Max
                           end, Top, CaveCoordsOfRock),

      Cave#cave{ current_rock_num = CurrentRockNum + 1
               , dropped_rocks = NewDroppedRocks
               , left_offset = ?INITIAL_OFFSET
               , height = NewTop + ?INITIAL_DROP_HEIGHT + 1
               , num_remaining_rocks = NumRemainingRocks - 1
               , jets = Rest
               , top = NewTop
               };
    false ->
      %% Fall one step
      Cave#cave{jets = Rest, height = Height - 1}
  end.

%%% ============================================================
%%% Helpers
%%% ============================================================

get_rock(N, Rocks) ->
  element((N rem 5) + 1, Rocks).

rock_to_cave_coords({X, Y}, Offset, Height) ->
  {X + Offset, Y + Height}.

move(?JET_LEFT, Offset, _Rock) ->
  max(0, Offset - 1);
move(?JET_RIGHT, Offset, #rock{width = Width}) ->
  min(?CAVE_WIDTH - Width, Offset + 1).

%% -ifndef(TRACE).
%% print_state(_) -> ok.
%% -else.
%% print_state(Cave) ->
%%   if Cave#cave.current_rock_num =:= undefined ->
%%       ok;
%%      true ->
%%       ?fmt("Cave at rock number ~w~n", [Cave#cave.current_rock])
%%   end,
%%   Rock = get_rock(Cave#cave.current_rock_num, ?ROCKS),
%%   Offset = Cave#cave.left_offset,
%%   Height = Cave#cave.height,
%%   TopRock = lists:max(
%%               lists:map(fun({_, Y}) -> Y end,
%%                         sets:to_list(Cave#cave.dropped_rocks) ++
%%                           if Rock =:= undefined -> [];
%%                              true ->
%%                               lists:map(fun(Pos) ->
%%                                             rock_to_cave_coords(Pos, Offset, Height)
%%                                         end,
%%                                         Rock#rock.coords)
%%                           end)),
%%   FirstY = max(4, TopRock),
%%   LastY = 0,
%%   ?fmt(
%%     "~s",
%%     [lists:map(
%%        fun(Y) ->
%%            C = if Y == 0 -> $+ ;
%%                   true -> $|
%%                end,

%%            if Y == Cave#cave.top ->
%%                io_lib:format("~3w => ~s", [Y, [C]]);
%%               true ->
%%                io_lib:format("~3w    ~s", [Y, [C]])
%%            end ++

%%              lists:map(
%%                fun(X) ->
%%                    Pos = {X, Y},
%%                    RockCoord =
%%                      cave_to_rock_coords(Pos,
%%                                          Cave#cave.left_offset,
%%                                          Cave#cave.height),

%%                    IsBottomLine = (Y == 0),
%%                    IsFallingRock = if Rock =:= undefined -> false;
%%                                       true -> lists:member(RockCoord, Rock#rock.coords)
%%                                    end,
%%                    IsDroppedRock = sets:is_element(Pos, Cave#cave.dropped_rocks),

%%                    if IsBottomLine -> $-;
%%                       IsFallingRock -> $@;
%%                       IsDroppedRock -> $#;
%%                       true -> $.
%%                    end
%%                end, lists:seq(0, 6)) ++
%%              io_lib:format("~s~n", [[C]])
%%        end, lists:seq(FirstY, LastY, -1))]
%%     ).
%% -endif.


-ifdef(TEST).

day17_test() ->
  {3153, tbd} = solve(?NUM_ROCKS_PART1).
  %% not_solved = solve(?NUM_ROCKS_PART2).

-endif.
