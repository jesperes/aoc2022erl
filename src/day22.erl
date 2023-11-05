-module(day22).

-export([solve/0]).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(RIGHT, 0).
-define(DOWN, 1).
-define(LEFT, 2).
-define(UP, 3).

heading(0) ->
  right;
heading(1) ->
  down;
heading(2) ->
  left;
heading(3) ->
  up.

test_data() ->
  <<"        ...#    \n"
    "        .#..    \n"
    "        #...    \n"
    "        ....    \n"
    "...#.......#    \n"
    "........#...    \n"
    "..#....#....    \n"
    "..........#.    \n"
    "        ...#....\n"
    "        .....#..\n"
    "        .#......\n"
    "        ......#.\n"
    "                \n"
    "10R5L5R10L4R5L5\n">>.

solve() ->
  solve(input:get(22)).

coords(Bin) ->
  coords(Bin, 0, 0, #{}).

coords(<<C, Rest/binary>>, X, Y, Map) when C =:= $# orelse C =:= $. ->
  coords(Rest, X + 1, Y, maps:put({X, Y}, C, Map));
coords(<<32, Rest/binary>>, X, Y, Map) ->
  coords(Rest, X + 1, Y, Map);
coords(<<$\n, Rest/binary>>, _, Y, Map) ->
  coords(Rest, 0, Y + 1, Map);
coords(_, _, _, Map) ->
  Map.

instructions(Bin) ->
  {match, Matches} = re:run(Bin, "(\\d+|[RL])", [global, {capture, all_but_first, binary}]),
  lists:map(fun ([<<"R">>]) ->
                'R';
                ([<<"L">>]) ->
                'L';
                ([N]) ->
                binary_to_integer(N)
            end,
            Matches).

start_coord(Bin) ->
  {X, _} = binary:match(Bin, <<".">>),
  {X, 0}.

turn('R', Heading) ->
  (Heading + 1) rem 4;
turn('L', Heading) ->
  (Heading + 3) rem 4.

turn180(Heading) ->
  (Heading + 2) rem 4.

forward({X, Y}, ?RIGHT) ->
  {X + 1, Y};
forward({X, Y}, ?DOWN) ->
  {X, Y + 1};
forward({X, Y}, ?LEFT) ->
  {X - 1, Y};
forward({X, Y}, ?UP) ->
  {X, Y - 1}.

backward(Pos, Heading) ->
  forward(Pos, turn180(Heading)).

tile_type(Pos, Map) ->
  maps:get(Pos, Map, undefined).

solve(Bin) ->
  Map = coords(Bin),
  Instrs = instructions(Bin),
  Start = start_coord(Bin),
  %% io:format("~s~n", [grid:to_str(Map)]),
  %% io:format("~p~n", [Instrs]),
  %% io:format("start: ~p~n", [Start]),
  walk(Start, ?RIGHT, Instrs, Map, 1).

%% walk(Start, ?RIGHT, Instrs, Map, 2)}.

walk({X, Y}, Heading, [], _Map, _Part) ->
  Row = Y + 1,
  Col = X + 1,
  1000 * Row + 4 * Col + Heading;
walk(Pos, Heading, ['L' = Left | Instrs], Map, Part) ->
  walk(Pos, turn(Left, Heading), Instrs, Map, Part);
walk(Pos, Heading, ['R' = Right | Instrs], Map, Part) ->
  walk(Pos, turn(Right, Heading), Instrs, Map, Part);
walk(Pos, Heading, [Dist | Instrs], Map, Part) ->
  NewPos = steps(Pos, Heading, Dist, Map, Part),
  walk(NewPos, Heading, Instrs, Map, Part).

steps(Pos, _Heading, 0, _Map, _Part) ->
  Pos;
steps(Pos, Heading, Dist, Map, Part) ->
  NewPos = forward(Pos, Heading),
  case tile_type(NewPos, Map) of
    undefined ->
      WarpDest = warp_dest(Pos, Heading, Map, Part),
      case tile_type(WarpDest, Map) of
        $# ->
          Pos;
        $. ->
          steps(WarpDest, Heading, Dist - 1, Map, Part)
      end;
    $# ->
      Pos;
    $. ->
      steps(NewPos, Heading, Dist - 1, Map, Part)
  end.

warp_dest(Pos, Heading, Map, 1) ->
  warp_dest1(Pos, turn180(Heading), Map);
warp_dest(Pos, Heading, _Map, 2) ->
  warp_dest2(Pos, Heading).

warp_dest1(Pos, RevHeading, Map) ->
  RevPos = forward(Pos, RevHeading),
  case tile_type(RevPos, Map) of
    undefined ->
      Pos;
    _ ->
      warp_dest1(RevPos, RevHeading, Map)
  end.

%% My input looks like this:
%%             50 99   149
%%        0  49|  | 100|
%%        |  | |  | |  |
%% +----------6---+ +-----------+
%% |+------6---+  | 6  +--6----+|
%% ||          |  | |  |       ||
%% ||+---------a11b-a22b---+   ||       0
%% |||         1111 2222   |   ||
%% |||         1111 2222   |   ||
%% ||| +-------c11d-c22d-+ |   ||       49
%% ||| |       |  | |  | | |   ||
%% ||| |  +----a33b-+  | | |   ||       50
%% ||| |  |    3333    | | |   ||
%% ||| |  |    3333    | | |   ||
%% ||| |  |  +-c33d----+ | |   ||       99
%% ||| |  |  | |  |      | |   ||
%% ||| +--a44b-a55b------+ |   ||      100
%% |||    4444 5555        |   ||
%% |||    4444 5555        |   ||
%% ||+----c44d-c55d--------+   ||      149
%% ||     |  | |  |            ||
%% |+--1--a66b-+  |            ||      150
%% |      6666    |            ||
%% |      6666    |            ||
%% +---1--c66d----+            ||      199
%%        |  |                 ||
%%        |  +-----------------+|
%%        +---------------------+
%%
%% The sides of the cube are 50x50
%%

%% Rules for warping around the sides of the cube. There are 14 of
%% these, corresponding to the 14 "open" edges in the flattened cube.
warp_dest2({X, Y}, ?LEFT) when X == 0 andalso Y =< 149 ->
  {{50, 149 - Y}, ?RIGHT}; %% 4 left -> 1 left
warp_dest2({X, Y}, ?RIGHT) when X == 149 ->
  {{99, 149 - Y}, ?LEFT};  %% 2 right -> 5 right
warp_dest2({X, Y}, ?LEFT) when X == 50 andalso Y =< 49 ->
  {{0, 149 - Y}, ?RIGHT};  %% 1 left  -> 4 left
warp_dest2({X, Y}, ?UP) when Y == 0 andalso X =< 99 ->
  {{0, X + 100}, ?RIGHT};  %% 1 up -> 6 left
warp_dest2({X, Y}, ?UP) when Y == 0 andalso X >= 100 ->
  {{X - 100, 199}, ?UP};   %% 2 up -> 6 down
warp_dest2({X, Y}, ?UP) when Y == 100 andalso X =< 49 ->
  {{50, X + 50}, ?RIGHT};  %% 4 up -> 3 left
warp_dest2({X, Y}, ?LEFT) when X == 50 andalso Y >= 50 andalso Y =< 99 ->
  {{Y - 50, 100}, ?DOWN};  %% 3 left -> 4 up
warp_dest2({X, Y}, ?LEFT) when X == 0 andalso Y >= 150 ->
  {{Y - 100, 0}, ?DOWN};   %% 6 left -> 1 top
warp_dest2({X, Y}, ?DOWN) when Y == 199 ->
  {{X + 100, 0}, ?DOWN};   %% 6 down -> 2 up
warp_dest2({X, Y}, ?RIGHT) when X == 49 andalso Y >= 150 ->
  {{Y - 100, 149}, ?UP};   %% 6 right -> 5 bottom
warp_dest2({X, Y}, ?DOWN) when Y == 149 andalso X >= 50 ->
  {{49, X + 100}, ?LEFT};  %% 5 bottom -> 6 right
warp_dest2({X, Y}, ?RIGHT) when X == 99 andalso Y >= 50 andalso Y =< 99 ->
  {{Y + 50, 49}, ?UP};     %% 3 right -> 2 bottom
warp_dest2({X, Y}, ?DOWN) when X >= 100 andalso Y == 49 ->
  {{99, X - 50}, ?LEFT};   %% 2 bottom -> 3 right
warp_dest2({X, Y}, ?RIGHT) when X == 99 andalso Y >= 100 ->
  {{149, 149 - Y}, ?LEFT}. %% 5 right -> 2 right

-ifdef(TEST).

%% Test that warp_dest2/1 is consistent, i.e. for each empty position
%% along the edges, you can warp through, turn around 180 degrees,
%% then warp back to the same location.
warp_dest2_test() ->
  Bin = input:get(22),
  Map = coords(Bin),
  lists:foreach(
    fun({Pos, Heading}) ->
        case tile_type(Pos, Map) of
          $. ->
            NewPos = forward(Pos, Heading),
            case tile_type(NewPos, Map) of
              undefined ->
                {WarpDest, WarpHeading} = warp_dest2(Pos, Heading),
                RevHeading = turn180(WarpHeading),
                {OrigPos, RevRevHeading} = warp_dest2(WarpDest, RevHeading),
                OrigHeading = turn180(RevRevHeading),
                ?assertEqual(heading(OrigHeading), heading(Heading)),
                ?assertEqual(OrigPos, Pos);
              $# ->
                ok;
              $. ->
                ok
            end;
          $# ->
            ok
        end
    end,
    lists:sort([{Pos, Heading} || Pos <- maps:keys(Map), Heading <- lists:seq(0, 3)])).

-endif.
