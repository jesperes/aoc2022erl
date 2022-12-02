-module(input).

-export([get/1, do_get/1]).

input_filename(Day) ->
  io_lib:format("priv/input~2..0w.txt", [Day]).

-spec do_get(Day :: integer()) -> binary().
do_get(Day) ->
  {ok, Binary} = file:read_file(input_filename(Day)),
  Binary.

-ifdef(INLINE_INPUTS).
-compile({parse_transform, ct_expand}).

get(1) -> ct_expand:term(do_get(1));
get(2) -> ct_expand:term(do_get(2));
get(N) ->
  do_get(N).

-else.

-spec get(Day :: integer()) -> binary().
get(Day) ->
  do_get(Day).

-endif.
