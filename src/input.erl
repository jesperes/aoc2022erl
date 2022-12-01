-module(input).

-export([get/1]).

-spec get(Day :: integer()) -> binary().
get(Day) ->
  {ok, Binary} = file:read_file(io_lib:format("priv/input~2..0w.txt", [Day])),
  Binary.
