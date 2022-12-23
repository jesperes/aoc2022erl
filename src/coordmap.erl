%% Wrapper for maps using coordinates as keys
-module(coordmap).

-include_lib("eunit/include/eunit.hrl").

-export([ new/0
        , put/3
        , get/2
        , get/3
        , to_list/1
        , from_list/1
        , size/1
        , is_key/2
        , keys/1
        , fold/3
        , update_with/3
        , update_with/4
        , remove/2
        , filter/2

        , benchmark/1
        , benchmark/0
        ]).

-define(MASK, 16#ffffffff).
-define(SHIFT, 32).
-define(OFFSET, (1 bsl 16)).
-define(to_int(Pos),
        begin
          {X, Y} = Pos,
          ((X + ?OFFSET) bsl ?SHIFT) bor (Y + ?OFFSET)
        end).
-define(to_int_x(X), (X bsr ?SHIFT) - ?OFFSET).
-define(to_int_y(Y), (Y band ?MASK) - ?OFFSET).
-define(to_coord(Int), {?to_int_x(Int), ?to_int_y(Int)}).

new() ->
  #{}.

put(Coord, Value, Map) ->
  maps:put(?to_int(Coord), Value, Map).

get(Coord, Map) ->
  maps:get(?to_int(Coord), Map).

get(Coord, Map, Default) ->
  maps:get(?to_int(Coord), Map, Default).

to_list(Map) when is_map(Map) ->
  lists:map(fun({Int, V}) ->
                {?to_coord(Int), V}
            end, maps:to_list(Map)).

from_list(List) when is_list(List) ->
  maps:from_list(
    lists:map(fun({Coord, V}) ->
                  {?to_int(Coord), V}
              end, List)).

size(Map) ->
  maps:size(Map).

is_key(Coord, Map) ->
  maps:is_key(?to_int(Coord), Map).

fold(Fun, Acc, Map) ->
  maps:fold(
    fun(Int, V, Acc0) ->
        Fun(?to_coord(Int), V, Acc0)
    end, Acc, Map).

keys(Map) ->
  lists:map(fun to_coord/1, maps:keys(Map)).

update_with(Coord, Fun, Default, Map) ->
  maps:update_with(?to_int(Coord), Fun, Default, Map).

update_with(Coord, Fun, Map) ->
  maps:update_with(?to_int(Coord), Fun, Map).

remove(Coord, Map) ->
  maps:remove(?to_int(Coord), Map).

filter(Fun, Map) ->
  maps:filter(fun(Int, Value) ->
                  Fun(?to_coord(Int), Value)
              end, Map).

%% Internal

to_coord(Coord) ->
  ?to_coord(Coord).

-ifdef(TEST).

int_coord_map_test() ->
  Coord = {100, 100},
  ?assertEqual(Coord, ?to_coord(?to_int(Coord))).

maps_test() ->
  Coords = lists:sort([{{X, Y}, true} || X <- lists:seq(1, 100),
                                         Y <- lists:seq(1, 100)]),

  CoordMap = from_list(Coords),
  L = lists:sort(to_list(CoordMap)),
  ?assertEqual(Coords, L).

fold_test() ->
  Coords = lists:sort([{{X, Y}, true} || X <- lists:seq(1, 100),
                                         Y <- lists:seq(1, 100)]),

  CoordMap = from_list(Coords),
  ?debugVal(CoordMap),
  Result = fold(fun({X, Y}, true, Acc) when X >= 1 andalso
                                            X =< 100 andalso
                                            Y >= 1 andalso
                                            Y =< 100 ->
                    Acc + 1
                end, 0, CoordMap),
  ?assertEqual(100 * 100, Result).

benchmark() ->
  lists:foreach(
    fun(B) ->
        Values =
          lists:foldl(
            fun(_, Acc) ->
                {Time, _} = timer:tc(fun() -> benchmark(B) end),
                [Time|Acc]
            end, [], lists:seq(1, 20)),
        Sum = lists:sum(Values),
        Avg = Sum / length(Values),
        io:format("~10s: ~w ms~n", [B, trunc(Avg / 1000)])
    end, [?MODULE, maps]).

benchmark(Backend) ->
  Coords = lists:sort([{X, Y} || X <- lists:seq(1, 1000),
                                 Y <- lists:seq(1, 1000)]),
  Map = lists:foldl(fun(Coord, Acc) ->
                        Backend:put(Coord, 1, Acc)
                    end, Backend:new(), Coords),

  Backend:fold(fun(_Coord, Value, Acc) ->
                   Value + Acc
               end, 0, Map).



-endif.
