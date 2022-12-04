-module(day04).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  Bin = input:get(4),
  Lines = binary:split(Bin, <<"\n">>, [global]),
  lists:foldl(
    fun(<<>>, Acc) -> Acc;
       (Line, {P1, P2}) ->
        [A, B] = binary:split(Line, <<",">>),
        Arange = b2i(binary:split(A, <<"-">>)),
        Brange = b2i(binary:split(B, <<"-">>)),
        P1out = case contains_either(Arange, Brange) of
                  true -> P1 + 1;
                  false -> P1
                end,
        P2out = case overlaps(Arange, Brange) of
                  true -> P2 + 1;
                  false -> P2
                end,
        {P1out, P2out}
    end, {0, 0}, Lines).

b2i(List) ->
  lists:map(fun(Bin) ->
                binary_to_integer(Bin)
            end, List).

overlaps([A0, A1], [B0, B1]) ->
  (A1 >= B0 andalso A1 =< B1) orelse
    (B1 >= A0 andalso B1 =< A1).

contains_either(A, B) ->
   contains(A, B) orelse contains(B, A).

contains([A0, A1], [B0, B1]) ->
  B0 >= A0 andalso B1 =< A1.

-ifdef(TEST).

day03_test() ->
  {582, 893} = solve().

-endif.
