-module(input).

-export([get/1, do_get/1]).

input_filename(Day) ->
  io_lib:format("priv/input~2..0w.txt", [Day]).

-spec do_get(Day :: integer()) -> binary().
do_get(Day) ->
  Filename = input_filename(Day),
  case file:read_file(Filename) of
    {ok, Binary} -> Binary;
    {error, enoent} ->
      io:format("--- could not find input file ~s~n", [Filename]),
      <<>>
  end.

-ifdef(INLINE_INPUTS).
-compile({parse_transform, ct_expand}).

get(1) -> ct_expand:term(do_get(1));
get(2) -> ct_expand:term(do_get(2));
get(3) -> ct_expand:term(do_get(3));
get(4) -> ct_expand:term(do_get(4));
get(5) -> ct_expand:term(do_get(5));
get(6) -> ct_expand:term(do_get(6));
get(7) -> ct_expand:term(do_get(7));
get(8) -> ct_expand:term(do_get(8));
get(9) -> ct_expand:term(do_get(9));
get(10) -> ct_expand:term(do_get(10));
get(11) -> ct_expand:term(do_get(11));
get(12) -> ct_expand:term(do_get(12));
get(13) -> ct_expand:term(do_get(13));
get(14) -> ct_expand:term(do_get(14));
get(15) -> ct_expand:term(do_get(15));
get(16) -> ct_expand:term(do_get(16));
get(17) -> ct_expand:term(do_get(17));
get(18) -> ct_expand:term(do_get(18)).

-else.

-spec get(Day :: integer()) -> binary().
get(Day) ->
  do_get(Day).

-endif.
