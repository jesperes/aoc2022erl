-module(day25).

-compile([ export_all
         , nowarn_export_all
         ]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  Bin = input:get(25),
  DecimalSum =
    lists:foldl(fun(Line, Acc) ->
                    snafu(Line) + Acc
                end, 0, binary:split(Bin, <<"\n">>, [global])),
  rev_snafu(DecimalSum).

snafu(Bin) when is_binary(Bin) ->
  snafu(binary_to_list(Bin));
snafu(Str) ->
  snafu(lists:reverse(Str), 1, 0).

snafu([], _, Sum) -> Sum;
snafu([$2|Rest], N, Sum) -> snafu(Rest, N * 5, Sum + 2 * N);
snafu([$1|Rest], N, Sum) -> snafu(Rest, N * 5, Sum + 1 * N);
snafu([$0|Rest], N, Sum) -> snafu(Rest, N * 5, Sum);
snafu([$-|Rest], N, Sum) -> snafu(Rest, N * 5, Sum - 1 * N);
snafu([$=|Rest], N, Sum) -> snafu(Rest, N * 5, Sum - 2 * N).

rev_snafu(N) when is_integer(N) ->
  rev_snafu(N, "").

rev_snafu(N, S) when N == 0 -> S;
rev_snafu(N, S) ->
  Index = (N + 2) rem 5,
  C = snafu_char(Index),
  if Index < 2 ->
      rev_snafu((N + 5) div 5, [C|S]);
     true ->
      rev_snafu(N div 5, [C|S])
  end.

snafu_char(0) -> $=;
snafu_char(1) -> $-;
snafu_char(2) -> $0;
snafu_char(3) -> $1;
snafu_char(4) -> $2.


-ifdef(TEST).

solve_test() ->
  ?assertEqual(tbd, solve()).

snafu_test() ->
  ?assertEqual(976, snafu("2=-01")),
  ?assertEqual(314159265, snafu("1121-1110-1=0")),
  lists:foreach(fun(N) ->
                    Rev = rev_snafu(N),
                    Snafu = snafu(Rev),
                    ?assertEqual(Snafu, N)
                end, lists:seq(1, 1000)).

-endif.
