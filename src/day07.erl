-module(day07).

-export([solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(TMP_DIR, "/tmp/day07").

solve() ->
  Bin = input:get(7),
  %% Bin = <<"$ cd /\n"
  %%         "$ ls\n"
  %%         "dir a\n"
  %%         "14848514 b.txt\n"
  %%         "8504156 c.dat\n"
  %%         "dir d\n"
  %%         "$ cd a\n"
  %%         "$ ls\n"
  %%         "dir e\n"
  %%         "29116 f\n"
  %%         "2557 g\n"
  %%         "62596 h.lst\n"
  %%         "$ cd e\n"
  %%         "$ ls\n"
  %%         "584 i\n"
  %%         "$ cd ..\n"
  %%         "$ cd ..\n"
  %%         "$ cd d\n"
  %%         "$ ls\n"
  %%         "4060174 j\n"
  %%         "8033020 d.log\n"
  %%         "5626152 d.ext\n"
  %%         "7214296 k\n">>,
  Lines = binary:split(Bin, <<"\n">>, [global]),
  {ok, Cwd} = file:get_cwd(),
  try
    to_tree(Lines),
    {part1(?TMP_DIR), part2(?TMP_DIR)}
  after
    file:set_cwd(Cwd),
    file:del_dir_r(?TMP_DIR)
  end.

to_tree([Line|Rest]) ->
  case binary:split(Line, <<" ">>, [global]) of
    [<<"$">>, <<"cd">>, <<"/">>] ->
      file:del_dir_r(?TMP_DIR),
      file:make_dir(?TMP_DIR),
      file:set_cwd(?TMP_DIR),
      to_tree(Rest);
    [<<"$">>, <<"cd">>, <<"..">>] ->
      {ok, Cwd} = file:get_cwd(),
      ok = file:set_cwd(filename:join(Cwd, "..")),
      to_tree(Rest);
    [<<"$">>, <<"cd">>, Dir] ->
      ok = file:set_cwd(Dir),
      to_tree(Rest);
    [<<"$">>, <<"ls">>] ->
      to_tree(Rest);
    [<<"dir">>, Dir] ->
      ok = case file:make_dir(binary_to_list(Dir)) of
             ok -> ok;
             {error, eexist} -> ok
           end,
      to_tree(Rest);
    [Size, File] ->
      Bits = binary_to_integer(Size) * 8,
      Binary = <<0:Bits>>,
      file:write_file(File, Binary),
      ?assertEqual(binary_to_integer(Size), filelib:file_size(File)),
      to_tree(Rest);
    [<<>>] ->
      ok
  end.

rec_size(Name) ->
  case filelib:is_dir(Name) of
    true ->
      {ok, Dirs} = file:list_dir(Name),
      lists:foldl(fun(D, Acc) ->
                      F = filename:join(Name, D),
                      rec_size(F) + Acc
                  end, 0, Dirs);
    false ->
      filelib:file_size(Name)
  end.

fold_dirs(Fun, Acc, Root) ->
  case filelib:is_dir(Root) of
    false ->
      Acc;
    true ->
      Acc0 = Fun(Root, Acc),
      {ok, Dirs} = file:list_dir(Root),
      lists:foldl(fun(D, InnerAcc) ->
                      fold_dirs(Fun, InnerAcc, filename:join(Root, D))
                  end, Acc0, Dirs)
  end.

part1(Root) ->
  fold_dirs(fun(Dir, Acc) ->
                case rec_size(Dir) of
                  N when N =< 100000 ->
                    Acc + N;
                  _ ->
                    Acc
                end
            end, 0, Root).

part2(Root) ->
  Used = rec_size(Root),
  Total = 70000000,
  CurrentFree = Total - Used,
  NeededFree = 30000000,
  fold_dirs(fun(Dir, Acc) ->
                RecSize = rec_size(Dir),
                FreeIfDeleted = (CurrentFree + RecSize),
                if FreeIfDeleted >= NeededFree ->
                    min(Acc, RecSize);
                   true ->
                    Acc
                end
            end, infinity, Root).

-ifdef(TEST).

day07_test() ->
  {1543140,1117448} = solve().

-endif.
