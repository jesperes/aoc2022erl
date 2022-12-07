-module(day07).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {cwd = undefined,
                sub_dirs = #{},
                sizes = #{}
               }).

-define(ROOT, [<<"root">>]).

solve() ->
  Bin = input:get(7),
  Lines = binary:split(Bin, <<"\n">>, [global]),
  State = to_tree(Lines, #state{}),
  P1 = lists:sum(
         lists:filter(fun(Value) -> Value =< 100000 end,
                      maps:values(State#state.sizes))),
  TotalUsed = maps:get(?ROOT, State#state.sizes),
  MaxFileSize = 40000000,
  P2 = lists:min(
         lists:filter(fun(Value) ->
                          TotalUsed - Value =< MaxFileSize
                      end, maps:values(State#state.sizes))),
  {P1, P2}.

to_tree([Line|Rest], #state{cwd = Cwd,
                            sub_dirs = Dirs,
                            sizes = Sizes} = State) ->

  case binary:split(Line, <<" ">>, [global]) of
    [<<"$">>, <<"cd">>, <<"/">>] ->
      to_tree(Rest, State#state{cwd = ?ROOT,
                                sizes = maps:put(?ROOT, 0, Sizes),
                                sub_dirs = maps:put(?ROOT, [], Dirs)});
    [<<"$">>, <<"cd">>, <<"..">>] ->
      to_tree(Rest, State#state{cwd = lists:sublist(Cwd, length(Cwd) - 1)});
    [<<"$">>, <<"cd">>, Dir] ->
      to_tree(Rest, State#state{cwd = Cwd ++ [Dir]});
    [<<"$">>, <<"ls">>] ->
      to_tree(Rest, State);
    [<<"dir">>, Dir] ->
      FullDir = Cwd ++ [Dir],
      Dirs0 = maps:update_with(Cwd,
                               fun(Old) -> [FullDir|Old] end,
                               Dirs),
      Dirs1 = maps:put(FullDir, [], Dirs0),
      Sizes1 = maps:put(FullDir, 0, Sizes),
      to_tree(Rest, State#state{sub_dirs = Dirs1,
                                sizes = Sizes1});
    [SizeBin, _File] ->
      Size = binary_to_integer(SizeBin),
      Sizes0 = maps:update_with(Cwd,
                                fun(Old) -> Old + Size end,
                                Size, Sizes),
      to_tree(Rest, State#state{sizes = Sizes0});
    [<<>>] ->
      compute_sizes(?ROOT, State)
  end.

compute_sizes(Root, #state{sub_dirs = Dirs,
                           sizes = _Sizes} = State) ->
  lists:foldl(fun(SubDir, Acc) ->
                  Acc0 = compute_sizes(SubDir, Acc),
                  SubDirSize = maps:get(SubDir, Acc0#state.sizes),
                  Acc0#state{sizes =
                               maps:update_with(Root,
                                                fun(Old) -> Old + SubDirSize end,
                                                Acc0#state.sizes)}
              end, State, maps:get(Root, Dirs)).


-ifdef(TEST).

day07_test() ->
  {1543140,1117448} = solve().

-endif.
