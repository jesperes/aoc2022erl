-module(day05).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

%%                         [Z] [W] [Z]
%%         [D] [M]         [L] [P] [G]
%%     [S] [N] [R]         [S] [F] [N]
%%     [N] [J] [W]     [J] [F] [D] [F]
%% [N] [H] [G] [J]     [H] [Q] [H] [P]
%% [V] [J] [T] [F] [H] [Z] [R] [L] [M]
%% [C] [M] [C] [D] [F] [T] [P] [S] [S]
%% [S] [Z] [M] [T] [P] [C] [D] [C] [D]
%%  1   2   3   4   5   6   7   8   9

initial_state() ->
  {[n, v, c, s],
   [s, n, h, j, m, z],
   [d, n, j, g, t, c, m],
   [m, r, w, j, f, d, t],
   [h, f, p],
   [j, h, z, t, c],
   [z, l, s, f, q, r, p, d],
   [w, p, f, d, h, l, s, c],
   [z, g, n, f, p, m, s, d]}.

solve() ->
  Bin = input:get(5),
  Lines = binary:split(Bin, <<"\n">>, [global]),
  State1 = initial_state(),
  State2 = initial_state(),
  move_crates(Lines, {State1, State2}).

move_crates(Lines, States) ->
  {P1, P2} =
    lists:foldl(
      fun(<<"move ", Rest/binary>>, {State1, State2}) ->
          [WhatBin, <<"from">>, FromBin, <<"to">>, ToBin] =
            binary:split(Rest, <<" ">>, [global]),
          What = binary_to_integer(WhatBin),
          From = binary_to_integer(FromBin),
          To = binary_to_integer(ToBin),
          {move(What, From, To, State1, fun lists:reverse/1),
           move(What, From, To, State2, fun(L) -> L end)};
         (_, StateIn) ->
          StateIn
      end, States, Lines),
  {top_crates(P1), top_crates(P2)}.

top_crates(Stacks) ->
  lists:flatten(
    lists:map(
      fun(Stack) ->
          atom_to_list(hd(Stack))
      end,
      tuple_to_list(Stacks))).

b2i(N) ->
   binary_to_integer(N).

move(What, From, To, State, Fun) ->
  FromStack = element(From, State),
  ToStack = element(To, State),
  {Crates, Remaining} = lists:split(What, FromStack),
  setelement(To, setelement(From, State, Remaining),
             Fun(Crates) ++ ToStack).

-ifdef(TEST).

day05_test() ->
  {"cnszfdvlj","qndwlmgns"} = solve().

-endif.
