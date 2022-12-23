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
        ]).

-define(is_coord(Coord), (is_tuple(Coord))).

-define(MASK, 16#ffffffff).
-define(SHIFT, 32).
-define(OFFSET, (1 bsl 16)).

new() ->
  #{}.

put(Coord, Value, Map) when ?is_coord(Coord) ->
  maps:put(to_int(Coord), Value, Map).

get(Coord, Map) when ?is_coord(Coord) ->
  maps:get(to_int(Coord), Map).

get(Coord, Map, Default) when ?is_coord(Coord) ->
  maps:get(to_int(Coord), Map, Default).

to_list(Map) when is_map(Map) ->
  lists:map(fun({Int, V}) ->
                {to_coord(Int), V}
            end, maps:to_list(Map)).

from_list(List) when is_list(List) ->
  maps:from_list(
    lists:map(fun({Coord, V}) when ?is_coord(Coord) ->
                  {to_int(Coord), V}
              end, List)).

size(Map) ->
  maps:size(Map).

is_key(Coord, Map) when ?is_coord(Coord) ->
  maps:is_key(to_int(Coord), Map).

fold(Fun, Acc, Map) ->
  maps:fold(
    fun(Int, V, Acc0) when is_integer(Int) ->
        Fun(to_coord(Int), V, Acc0)
    end, Acc, Map).

keys(Map) ->
  lists:map(fun to_coord/1, maps:keys(Map)).

update_with(Coord, Fun, Default, Map) when ?is_coord(Coord) ->
  maps:update_with(to_int(Coord), Fun, Default, Map).

update_with(Coord, Fun, Map) when ?is_coord(Coord) ->
  maps:update_with(to_int(Coord), Fun, Map).

remove(Coord, Map) when ?is_coord(Coord) ->
  maps:remove(to_int(Coord), Map).

filter(Fun, Map) ->
  maps:filter(fun(Int, Value) ->
                  Fun(to_coord(Int), Value)
              end, Map).

%% Internal

to_int({X, Y}) when X < ?MASK andalso Y < ?MASK ->
  ((X + ?OFFSET) bsl ?SHIFT) bor (Y + ?OFFSET).

to_int_x(X) ->
  (X bsr ?SHIFT) - ?OFFSET.

to_int_y(Y) ->
  (Y band ?MASK) - ?OFFSET.

to_coord(Int) when is_integer(Int) ->
  {to_int_x(Int), to_int_y(Int)}.


-ifdef(TEST).

int_coord_map_test() ->
  Coord = {100, 100},
  ?assertEqual(Coord, to_coord(to_int(Coord))).

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

-endif.
