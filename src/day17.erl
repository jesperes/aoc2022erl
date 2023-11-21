-module(day17).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

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
  drop_rocks(0, 0, Jets, [], #{}, Limit, 0).

drop_rocks(_RockIdx, _JetIdx, _Jets, Chamber, _States, 0, WarpHeight) ->
  length(strip_empty_rows(Chamber)) + WarpHeight;
drop_rocks(RockIdx, JetIdx, Jets, Chamber, States, Limit, WarpHeight) ->
  Rock = get_rock(RockIdx),
  RockLen = length(Rock),
  Chamber0 = lists:duplicate(RockLen, 0) ++ [0, 0, 0] ++ strip_empty_rows(Chamber),
  {Chamber1, JetIdx0} = drop_one_rock(Rock, RockLen, JetIdx, Jets, Chamber0),

  %% Cycle optimization for part 2
  {States0, Limit0, WarpHeight0} =
    case {WarpHeight, length(Chamber1)} of
      {0, Len} when Len > ?STATE_SIZE andalso Limit > 1_000_000 ->
        Key = make_key(JetIdx, Jets, Chamber1),
        case maps:get(Key, States, undefined) of
          undefined ->
            {maps:put(Key, {RockIdx, Len}, States), Limit, WarpHeight};
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

make_key(JetIdx, Jets, Chamber) ->
  {TopRows, _} = lists:split(?STATE_SIZE, Chamber),
  JetNum = JetIdx rem (byte_size(Jets) - 1),
  TopRowsBin = list_to_binary(TopRows),
  <<JetNum, TopRowsBin/binary>>.

drop_one_rock(Rock, RockLen, JetIdx, Jets, Chamber) ->
  {Jet, NewJetIdx} = get_jet(JetIdx, Jets),
  Rock1 = push_rock(Rock, Jet, Chamber),
  ChamberLen = length(Chamber),
  if RockLen == ChamberLen ->
      MergedAtBottom =
        lists:map(fun({RockRow, ChamberRow}) ->
                      RockRow bor ChamberRow
                  end, lists:zip(Rock1, Chamber)),
      {MergedAtBottom, NewJetIdx};
     true ->
      [TopRow|Chamber0] = Chamber,
      {TopRows, _} = lists:split(RockLen, Chamber0),
      CanFall = lists:all(fun({RockRow, ChamberRow}) ->
                              (RockRow band ChamberRow) == 0
                          end, lists:zip(Rock1, TopRows)),
      if CanFall ->
          {Rest, JetIdx0} =
            drop_one_rock(Rock1, RockLen, NewJetIdx, Jets, Chamber0),
          {[TopRow|Rest], JetIdx0};
        true ->
          {TopRows0, Rest} = lists:split(RockLen, Chamber),
          {lists:map(fun({RockRow, ChamberRow}) ->
                         RockRow bor ChamberRow
                     end, lists:zip(Rock1, TopRows0)) ++ Rest, NewJetIdx}
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

%% Helpers

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

-ifdef(TEST).

day17_test() ->
  ?assertEqual({3153, 1553665689155}, solve()).

-endif.
