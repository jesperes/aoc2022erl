-module(day17).

-compile([export_all, nowarn_export_all]).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

-type coord() :: {Y :: integer(), X :: integer()}.

-record(rock, { name :: atom()
              , coords :: [coord()]
              , width :: integer()
              , height :: integer()
              }).

-record(cycle, { rock_num_start :: integer()     %% First rock of cycle
               , height_at_start :: integer()    %% Height when cycle starts
               , cycle_height :: integer()       %% Height of cycle
               , num_rocks_in_cycle :: integer() %% Number of rocks in cycle
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

-record(state, { current_rock = 0 :: integer() | undefined
               , num_remaining_rocks = undefined :: integer()
               , left_offset = ?INITIAL_OFFSET :: integer()
               , height = ?INITIAL_DROP_HEIGHT + 1 :: integer()
               , winds = undefined :: binary()
               , orig_winds = undefined :: binary()
               , dropped_rocks = sets:new() :: sets:set()
               , top = 0 :: integer()
               , input_pos = 0 :: integer()
               , cycles = #{} :: map()
               , cycle = undefined :: #cycle{}
               }).

-define(TRACE, t).

-ifdef(TRACE).
-define(fmt(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(fmt(Fmt, Args), ok).
-endif.

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
  %% Bin = input:get(17),
  Bin = <<">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>">>,
  ?fmt("The first rock begins falling~n", []),
  ?fmt("~p~n", [Bin]),
  State = #state{ num_remaining_rocks = N
                , winds = Bin
                , orig_winds = Bin
                , dropped_rocks = sets:from_list([{0,0}, {1,0}, {2,0}, {3,0}, {4,0}, {5,0}, {6,0}])
                },
  print_state(State),
  Cycle = drop_rocks(State),
  #cycle{ rock_num_start = RockNumStart
        , height_at_start = HeightAtStart
        , cycle_height = CycleHeight
        , num_rocks_in_cycle = NumRocksInCycle
        } = Cycle,

  io:format("Cycle: ~p~n", [Cycle]),
  io:format("Rock at beginning of cycle: ~p~n", [get_rock(RockNumStart, ?ROCKS)]),
  N0 = (N - RockNumStart), %% drop rocks to reach cycle start
  %%Height0 = HeightAtStart,
  NumWholeCycles = N0 div NumRocksInCycle,
  %%RemainingRocks = N0 rem NumRocksInCycle,
  io:format("Num whole cycles: ~p~n", [NumWholeCycles]),
  FinalHeight = HeightAtStart + (NumWholeCycles * CycleHeight),
  {FinalHeight, Cycle}.


drop_rocks(#state{num_remaining_rocks = 0} = State) ->
  ?fmt("~nNo more rocks.~n", []),
  print_state(State#state{current_rock = undefined}),
  State#state.top;
drop_rocks(#state{ winds = <<>>
                 , orig_winds = Winds
                 } = State) ->
  drop_rocks(State#state{winds = Winds, input_pos = 0});
drop_rocks(#state{ winds = <<Wind, _/binary>> = _Winds
                 , height = Height
                 , current_rock = CurrentRock
                 } = State) when Height >= 0 ->
  Rock = get_rock(CurrentRock, ?ROCKS),
  State1 = maybe_move(Wind, Rock, State),

  %% case {Wind, State1#state.left_offset == State#state.left_offset} of
  %%   {$<, false} -> ?fmt("~nJet of gas pushes rock left~n", []);
  %%   {$>, false} -> ?fmt("~nJet of gas pushes rock right~n", []);
  %%   {$<, true} -> ?fmt("~nJet of gas pushes rock left, but nothing happens~n", []);
  %%   {$>, true} -> ?fmt("~nJet of gas pushes rock right, but nothing happens~n", [])
  %% end,
  print_state(State1),

  case maybe_come_to_rest_or_fall_one_step(State1) of
    #state{cycle = undefined} = State2 ->
      drop_rocks(State2);
    #state{cycle = Cycle} ->
      Cycle
  end.

fall_one_step(#state{height = Height} = State) ->
  State#state{height = Height - 1}.

maybe_move(Wind, Rock, #state{ height = Height
                             , left_offset = Offset
                             } = State) ->
  NewOffset = move(Wind, Offset, Rock),
  %% Make sure we aren't moving sideways into an existing rock
  case lists:any(fun(RockCoord) ->
                     CaveCoord = rock_to_cave_coords(RockCoord, NewOffset, Height),
                     sets:is_element(CaveCoord, State#state.dropped_rocks)
                 end, Rock#rock.coords) of
    true ->
      State;
    false ->
      State#state{left_offset = NewOffset}
  end.

maybe_come_to_rest_or_fall_one_step(#state{ winds = <<_, Rest/binary>>
                                          , current_rock = CurrentRock
                                          , left_offset = Offset
                                          , num_remaining_rocks = NumRemainingRocks
                                          , height = Height
                                          , top = Top
                                          } = State) ->
  Rock = get_rock(CurrentRock, ?ROCKS),

  %% We need to do two passes over the profile here, one to find out
  %% if the rock should come to rest, another one to update the
  %% profile.
  case should_come_to_rest(State) of
    true ->
      ?fmt("~nRock falls 1 unit, causing it to come to rest:~n", []),

      CaveCoordsOfRock =
        lists:map(fun(Pos) ->
                      rock_to_cave_coords(Pos, Offset, Height)
                  end, Rock#rock.coords),

      NewDroppedRocks =
        sets:union(sets:from_list(CaveCoordsOfRock),
                   State#state.dropped_rocks),

      NewTop = max(lists:max(lists:map(fun({_, Y}) -> Y end, CaveCoordsOfRock)), Top),

      State1a = State#state{ dropped_rocks = NewDroppedRocks
                           , top = NewTop
                           , current_rock = CurrentRock + 1
                           },

      case check_for_cycles(State1a) of
        #state{cycle = undefined} = State1b ->
          State1c = State1b,

          %% io:format("New top = ~p~n", [NewTop]),
          print_state(State1c),

          %% check_for_full_horizontal_line(State1),

          State2 =
            State1c#state{ left_offset = ?INITIAL_OFFSET
                         , height = NewTop + ?INITIAL_DROP_HEIGHT + 1
                         , num_remaining_rocks = NumRemainingRocks - 1
                         , input_pos = State#state.input_pos + 1
                         , winds = Rest
                         },

          case State2#state.num_remaining_rocks > 0 of
            true ->
              ?fmt("~nA new rock begins falling:~n", []),
              print_state(State2),
              State2;
            false ->
              ok
          end,
          State2;
        CycleState ->
          CycleState
      end;
    false ->
      ?fmt("~nRock falls 1 unit~n", []),
      NewState = State#state{ winds = Rest
                            , input_pos = State#state.input_pos + 1
                            , height = Height - 1
                            },
      print_state(NewState),
      NewState
  end.


%%% Helpers

check_for_cycles(#state{ cycles = Cycles
                       , winds = <<_, Rest/binary>>
                       , orig_winds = OrigWinds
                       , current_rock = CurrentRock
                       , top = Top
                       } = State) ->
  Winds0 = <<Rest/binary, OrigWinds/binary>>,
  {NextWinds, _} = erlang:split_binary(Winds0, 10),
  NumTopLayers = 10,
  FromLayer = max(0, Top - NumTopLayers),

  TopLayers = lists:sort(
                sets:fold(
                  fun({X, Y}, Acc) when Y >= FromLayer ->
                      [{X, Top - Y}|Acc];
                     (_, Acc) ->
                      Acc
                  end, [], State#state.dropped_rocks)),

  Key = {(get_rock(CurrentRock, ?ROCKS))#rock.name, TopLayers, NextWinds},
  %% io:format("Cycle key: ~0p~n", [Key]),
  case maps:get(Key, Cycles, undefined) of
    undefined ->
      State#state{cycles = maps:put(Key, #cycle{ rock_num_start = CurrentRock + 1
                                               , height_at_start = FromLayer - 1
                                               }, Cycles)};
    #cycle{ rock_num_start = RockNumStart
          , height_at_start = HeightAtStart
          } = Cycle ->
      State#state{cycle = Cycle#cycle{cycle_height = (Top - NumTopLayers) - HeightAtStart,
                                      num_rocks_in_cycle = CurrentRock - RockNumStart}}
  end.

should_come_to_rest(#state{ left_offset = Offset
                          , current_rock = CurrentRock
                          , dropped_rocks = Rocks
                          , height = Height
                          }) ->
  Rock = get_rock(CurrentRock, ?ROCKS),
  lists:any(
    fun({X, Y}) ->
        RockCoord = {X, Y - 1},
        CaveCoord = rock_to_cave_coords(RockCoord, Offset, Height),
        sets:is_element(CaveCoord, Rocks)
    end, Rock#rock.coords).

get_rock(undefined, _) ->
  undefined;
get_rock(N, Rocks) ->
  element((N rem 5) + 1, Rocks).

rock_to_cave_coords({X, Y}, Offset, Height) ->
  {X + Offset, Y + Height}.

cave_to_rock_coords({X, Y}, Offset, Height) ->
  {X - Offset, Y - Height}.

move(?JET_LEFT, Offset, _Rock) ->
  max(0, Offset - 1);
move(?JET_RIGHT, Offset, #rock{width = Width}) ->
  min(?CAVE_WIDTH - Width, Offset + 1).

-ifndef(TRACE).
print_state(_) -> ok.
-else.
print_state(State) ->
  if State#state.current_rock =:= undefined ->
      ok;
     true ->
      ?fmt("State at rock number ~w~n", [State#state.current_rock])
  end,
  Rock = get_rock(State#state.current_rock, ?ROCKS),
  Offset = State#state.left_offset,
  Height = State#state.height,
  TopRock = lists:max(
              lists:map(fun({_, Y}) -> Y end,
                        sets:to_list(State#state.dropped_rocks) ++
                          if Rock =:= undefined -> [];
                             true ->
                              lists:map(fun(Pos) ->
                                            rock_to_cave_coords(Pos, Offset, Height)
                                        end,
                                        Rock#rock.coords)
                          end)),
  FirstY = max(4, TopRock),
  LastY = 0,
  ?fmt(
    "~s",
    [lists:map(
       fun(Y) ->
           C = if Y == 0 -> $+ ;
                  true -> $|
               end,

           if Y == State#state.top ->
               io_lib:format("~3w => ~s", [Y, [C]]);
              true ->
               io_lib:format("~3w    ~s", [Y, [C]])
           end ++

             lists:map(
               fun(X) ->
                   Pos = {X, Y},
                   RockCoord =
                     cave_to_rock_coords(Pos,
                                         State#state.left_offset,
                                         State#state.height),

                   IsBottomLine = (Y == 0),
                   IsFallingRock = if Rock =:= undefined -> false;
                                      true -> lists:member(RockCoord, Rock#rock.coords)
                                   end,
                   IsDroppedRock = sets:is_element(Pos, State#state.dropped_rocks),

                   if IsBottomLine -> $-;
                      IsFallingRock -> $@;
                      IsDroppedRock -> $#;
                      true -> $.
                   end
               end, lists:seq(0, 6)) ++
             io_lib:format("~s~n", [[C]])
       end, lists:seq(FirstY, LastY, -1))]
    ).
-endif.


-ifdef(TEST).

day17_test() ->
  3068 = solve(?NUM_ROCKS_PART1).
%%  not_solved = solve(?NUM_ROCKS_PART2).

-endif.
