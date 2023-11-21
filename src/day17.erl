-module(day17).

-export([solve/0]).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(debug(Fmt, Args), io:format(standard_error, Fmt, Args)).

-define(ROCKS, [[2#0011110],

                [2#0001000,
                 2#0011100,
                 2#0001000],

                [2#0000100,
                 2#0000100,
                 2#0011100],

                [2#0010000,
                 2#0010000,
                 2#0010000,
                 2#0010000],

                [2#0011000,
                 2#0011000]]).

-define(STATE_SIZE, 18).
%% Chamber is represented as a list of integers, where the head of the
%% list is upwards.

solve() ->
  Jets = input:get(17),
  P1 = drop_rocks(Jets, 2022),
  P2 = drop_rocks(Jets, 1_000_000_000_000),
  {P1, P2}.

drop_rocks(Jets, Limit) ->
  drop_rocks(0, 0, Jets, [], #{}, Limit, undefined).

drop_rocks(_RockIdx, _JetIdx, _Jets, Chamber, _States, 0, undefined) ->
  length(strip_empty_rows(Chamber));
drop_rocks(_RockIdx, _JetIdx, _Jets, Chamber, _States, 0, WarpHeight) ->
  length(strip_empty_rows(Chamber)) + WarpHeight;
drop_rocks(RockIdx, JetIdx, Jets, Chamber, States, Limit, WarpHeight) ->
  Rock = get_rock(RockIdx),
  RockLen = length(Rock),
  Chamber0 = lists:duplicate(RockLen, 0) ++ [0, 0, 0] ++ strip_empty_rows(Chamber),
  %% ?debug("~nThe first rock begins falling:~n~s~n", [print_chamber(Rock, 0, Chamber0)]),
  {Chamber1, JetIdx0} = drop_one_rock(Rock, RockLen, JetIdx, Jets, Chamber0),
  %% ?debug("~nFinal chamber after this rock:~n~s~n", [print_chamber(Chamber1)]),

  %% ?debug("Height: ~p, Limit = ~p~n", [length(Chamber1), Limit]),

  {States0, Limit0, WarpHeight0} =
    case {WarpHeight, length(Chamber1)} of
      {undefined, Len} when Len > ?STATE_SIZE ->
        {TopRows, _} = lists:split(?STATE_SIZE, Chamber1),
        Key = {JetIdx rem (byte_size(Jets) - 1), TopRows},
        Value = {RockIdx, Len},
        case maps:get(Key, States, undefined) of
          undefined ->
            {maps:put(Key, Value, States), Limit, WarpHeight};
          {CycleStartRockIdx, CycleStartLen} ->
            CycleLen = Len - CycleStartLen,
            RocksPerCycle = RockIdx - CycleStartRockIdx,
            NumCycles = (Limit - CycleStartRockIdx) div RocksPerCycle - 1,
            {States, Limit - (NumCycles * RocksPerCycle), NumCycles * CycleLen}
        end;
      _ ->
        {States, Limit, WarpHeight}
    end,

  drop_rocks(RockIdx + 1, JetIdx0, Jets, Chamber1, States0, Limit0 - 1, WarpHeight0).

drop_one_rock(Rock, RockLen, JetIdx, Jets, Chamber) ->
  {Jet, NewJetIdx} = get_jet(JetIdx, Jets),
  Rock1 = push_rock(Rock, Jet, Chamber),
  %% ?debug("~nAfter move~n~s~n", [print_chamber(Rock1, Jet, Chamber)]),
  ChamberLen = length(Chamber),
  if RockLen == ChamberLen ->
      %% We are at the bottom; there are no more rows left in the
      %% chamber. Merge the rock into the last rows(s) of the
      %% chamber.
      MergedAtBottom =
        lists:map(fun({RockRow, ChamberRow}) ->
                      RockRow bor ChamberRow
                  end, lists:zip(Rock1, Chamber)),
      {MergedAtBottom, NewJetIdx};
     true ->
      %% There is more chamber below, check if it is possile
      %% to fall one step.
      [TopRow|Chamber0] = Chamber,
      {TopRows, _} = lists:split(RockLen, Chamber0),
      CanFall = lists:all(fun({RockRow, ChamberRow}) ->
                              (RockRow band ChamberRow) == 0
                          end, lists:zip(Rock1, TopRows)),
      if CanFall ->
          %% Continue falling from the next step
          {Rest, JetIdx0} =
            drop_one_rock(Rock1, RockLen, NewJetIdx, Jets, Chamber0),
          {[TopRow|Rest], JetIdx0};
        true ->
          %% We cannot fall anymore (but we haven't reached the
          %% bottom yet.
          {TopRows0, Rest} = lists:split(RockLen, Chamber),
          Merged = lists:map(fun({RockRow, ChamberRow}) ->
                                 RockRow bor ChamberRow
                             end, lists:zip(Rock1, TopRows0)),
          {Merged ++ Rest, NewJetIdx}
      end
  end.

%% Given a jet, move the rock sideways, if possible.
push_rock(Rock, Jet, Chamber) ->
  push_rock(Rock, Rock, Jet, Chamber).
push_rock(Rock, OrigRock, Jet, Chamber) ->
  push_rock(Rock, OrigRock, Jet, Chamber, []).
push_rock([], _OrigRock, _Jet, _Chamber, Acc) ->
  lists:reverse(Acc);
push_rock([Row|RockRows], OrigRock, Jet, [Chamber|ChamberRows], Acc) ->
  if Row band 2#1000000 /= 0 andalso Jet =:= $< -> OrigRock; %% hit left edge
     Row band 2#0000001 /= 0 andalso Jet =:= $> -> OrigRock; %% hit right edge
     true ->
      ShiftedRow =
        if Jet =:= $< -> Row bsl 1;
           Jet =:= $> -> Row bsr 1
        end,
      if ShiftedRow band Chamber == 0 ->
          push_rock(RockRows, OrigRock, Jet, ChamberRows, [ShiftedRow|Acc]);
         true ->
          OrigRock
      end
  end.

get_rock(RockIdx) ->
  lists:nth((RockIdx rem 5) + 1, ?ROCKS).

get_jet(JetIdx, Jets) ->
  case binary:at(Jets, JetIdx) of
    N when N =:= $< orelse N =:= $> ->
      {N, JetIdx + 1};
    _ ->
      get_jet(0, Jets)
  end.

strip_empty_rows([]) -> [];
strip_empty_rows([0|Rest]) ->
  strip_empty_rows(Rest);
strip_empty_rows(List) ->
  List.

print_chamber(Chamber) ->
  print_chamber([], 0, Chamber).

print_chamber(_, _, []) ->
  io_lib:format("+-------+", []);
print_chamber(Rock, Jet, [ChamberRow|ChamberRest]) ->
  {TopRock, RockRest, JetChar} =
    case Rock of
      [] -> {0, [], 32};
      [Top|Rest] -> {Top, Rest, Jet}
    end,

  "|" ++
    lists:map(fun(Idx) ->
                  if TopRock band (1 bsl Idx) /= 0 -> $@;
                     ChamberRow band (1 bsl Idx) /= 0 -> $#;
                     true -> $.
                  end
              end, lists:seq(6, 0, -1)) ++
    "|" ++ [JetChar] ++ "\n" ++
    print_chamber(RockRest, Jet, ChamberRest).

-ifdef(TEST).

push_rock_test() ->
  [Rock1|_] = ?ROCKS,
  ?assertEqual([2#0011110], Rock1),
  ?assertEqual([2#0111100], push_rock(Rock1, Rock1, $<, [0, 0, 0, 0])),
  ?assertEqual([2#0001111], push_rock(Rock1, Rock1, $>, [0, 0, 0, 0])),

  Rock2 = push_rock(Rock1, Rock1, $>, [0, 0, 0, 0]),
  Rock3 = push_rock(Rock2, Rock2, $>, [0, 0, 0, 0]),
  Rock4 = push_rock(Rock3, Rock3, $>, [0, 0, 0, 0]),
  ?assertEqual(Rock4, Rock3).

push_rock2_test() ->
  [Rock1|_] = ?ROCKS,
  ?assertEqual([2#0011110], Rock1),
  ?assertEqual(Rock1, push_rock(Rock1, Rock1, $>, [2#0000001])),
  ?assertEqual(Rock1, push_rock(Rock1, Rock1, $<, [2#1100000])).

push_rock3_test() ->
  [_, Rock2|_] = ?ROCKS,
  ?assertEqual(Rock2, push_rock(Rock2, Rock2, $>, [2#0000011,
                                                   2#0000011,
                                                   2#0000011])),
  ?assertEqual(Rock2, push_rock(Rock2, Rock2, $<, [2#1100000,
                                                   2#1100000,
                                                   2#1100000])).

drop_rocks_test() ->
  Jets = <<">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\n">>,
  Height = drop_rocks(Jets, 2022),
  ?assertEqual(3068, Height).

day17_test() ->
  ?assertEqual({3153, 1553665689155}, solve()).

-endif.
