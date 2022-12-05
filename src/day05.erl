-module(day05).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

initial_state() ->
  #{1 => [n, v, c, s],
    2 => [s, n, h, j, m, z],
    3 => [d, n, j, g, t, c, m],
    4 => [m, r, w, j, f, d, t],
    5 => [h, f, p],
    6 => [j, h, z, t, c],
    7 => [z, l, s, f, q, r, p, d],
    8 => [w, p, f, d, h, l, s, c],
    9 => [z, g, n, f, p, m, s, d]}.

solve() ->
  Bin = input:get(5),
  Lines = binary:split(Bin, <<"\n">>, [global]),
  Stacks1 = initial_state(),
  Stacks2 = initial_state(),
  move_crates(Lines, {Stacks1, Stacks2}).

move_crates(Lines, Stacks) ->
  {StacksOut1, StacksOut2} =
    lists:foldl(
      fun(<<"move ", Rest/binary>>, {Stacks1, Stacks2}) ->
          [WhatBin, <<"from">>, FromBin, <<"to">>, ToBin] =
            binary:split(Rest, <<" ">>, [global]),
          What = binary_to_integer(WhatBin),
          From = binary_to_integer(FromBin),
          To = binary_to_integer(ToBin),
          {move(What, From, To, Stacks1, fun lists:reverse/1),
           move(What, From, To, Stacks2, fun(L) -> L end)};
         (_, StacksAcc) ->
          StacksAcc
      end, Stacks, Lines),
  {top_crates(StacksOut1), top_crates(StacksOut2)}.

top_crates(Stacks) ->
  lists:flatten(
    lists:map(
      fun({_, Stack}) ->
          atom_to_list(hd(Stack))
      end,
      lists:sort(maps:to_list(Stacks)))).

move(What, From, To, Stacks, Fun) ->
  #{From := FromStack, To := ToStack} = Stacks,
  {Crates, Remaining} = lists:split(What, FromStack),
  Stacks#{From := Remaining, To := Fun(Crates) ++ ToStack}.

-ifdef(TEST).

day05_test() ->
  {"cnszfdvlj","qndwlmgns"} = solve().

-endif.
