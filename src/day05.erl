-module(day05).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  Bin = input:get(5),
  Lines = binary:split(Bin, <<"\n">>, [global]),
  move_crates(Lines).

move_crates(Lines) ->
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
         (<<>>, Stacks) when is_map(Stacks) ->
          %% Input parsing complete, make two copies of the input
          %% state, one for each puzzle part.
          {Stacks, Stacks};
         (<<>>, {_, _} = Stacks) ->
          Stacks;
         (CrateLine, StacksAcc) ->
          parse_crate_line(CrateLine, 1, StacksAcc)
      end, #{}, Lines),
  {top_crates(StacksOut1), top_crates(StacksOut2)}.

top_crates(Stacks) ->
  lists:flatten(
    lists:map(
      fun({_, Stack}) ->
          hd(Stack)
      end,
      lists:sort(maps:to_list(Stacks)))).

move(What, From, To, Stacks, Fun) ->
  #{From := FromStack, To := ToStack} = Stacks,
  {Crates, Remaining} = lists:split(What, FromStack),
  Stacks#{From := Remaining, To := Fun(Crates) ++ ToStack}.

parse_crate_line(<<$[, X, $], 32, Rest/binary>>, StackNum, Stacks) ->
  StacksOut = maps:update_with(StackNum, fun(Old) -> lists:append(Old, [X]) end, [X], Stacks),
  parse_crate_line(Rest, StackNum + 1, StacksOut); %% Reset stack num
parse_crate_line(<<$[, X, $]>>, StackNum, Stacks) ->
  maps:update_with(StackNum, fun(Old) -> lists:append(Old, [X]) end, [X], Stacks);
parse_crate_line(<<32, 32, 32, 32, Rest/binary>>, StackNum, Stacks) ->
  parse_crate_line(Rest, StackNum + 1, Stacks);
parse_crate_line(<<32, $1, _/binary>>, _, Stacks) ->
  Stacks.

-ifdef(TEST).

day05_test() ->
  {"CNSZFDVLJ","QNDWLMGNS"} = solve().

-endif.
