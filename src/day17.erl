-module(day17).

-export([solve/0]).

-compile([export_all, nowarn_export_all]).

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

%% Chamber is represented as a list of integers, where the head of the
%% list is upwards.

solve() ->
  Jets = input:get(17),
  drop_rocks([], ?ROCKS, <<>>, Jets, [], 2022).

drop_rocks(_, _, _, _, Chamber, 0) ->
  length(Chamber);
drop_rocks([], Rocks, Jets0, Jets, Chamber, Limit) ->
  drop_rocks(Rocks, Rocks, Jets0, Jets, Chamber, Limit);
drop_rocks(Rocks0, Rocks, <<>>, Jets, Chamber, Limit) ->
  ?debugFmt("Starting list of jets from the beginning...", []),
  drop_rocks(Rocks0, Rocks, Jets, Jets, Chamber, Limit);
drop_rocks([Rock|Rocks0], Rocks, Jets0, Jets, Chamber, Limit) ->
  {Chamber0, Jets1} = drop_rock(Rock, Jets0, Jets, Chamber),
  drop_rocks(Rocks0, Rocks, Jets1, Jets, Limit - 1, Chamber0).

drop_rock(Rock, Jets0, Jets, Chamber) ->
  %% Extend the chamber to make room for the rock
  Chamber0 = lists:duplicate(length(Rock), 0) ++ [0, 0, 0] ++ Chamber,
  %% Move the rock two steps to the right
  RockShift2 = lists:map(fun(Row) -> Row bsr 2 end, Rock),
  fall(RockShift2, Jets0, Jets, Chamber0).

fall(Rock, <<Jet, _Jets0/binary>>, _Jets, Chamber) ->
  _Rock0 = push_rock(Rock, Rock, Jet, Chamber),
  %% Rock1 = fall(Rock0, Chamber),
  %% here: we should return {NewChamber, NewJets}
  ok.

%% Given a jet, move the rock sideways, if possible.
push_rock(Rock, OrigRock, Jet, Chamber) ->
  push_rock(Rock, OrigRock, Jet, Chamber, []).
push_rock([], _OrigRock, _Jet, _Chamber, Acc) ->
  lists:reverse(Acc);
push_rock([Row|RockRows], OrigRock, Jet, [Chamber|ChamberRows], Acc) ->
  if Row band 2#1000000 /= 0 andalso Jet =:= $< ->
      OrigRock;
     Row band 2#0000001 /= 0 andalso Jet =:= $> ->
      OrigRock;
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

%% fall(Rock, Chamber) ->
%%   RockLen = length(Rock),
%%   ChamberLen = length(Chamber),
%%   if RockLen == ChamberLen ->
%%       %% Rock is at the very bottom of the chamber. Merge it into the
%%       %% chamber, and continue with the next rock.

print_chamber([]) -> ok;
print_chamber([X|Xs]) ->
  io:format(standard_error, "~.2B~n", [X]),
  print_chamber(Xs).

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

day17_test() ->
  ok.

-endif.
