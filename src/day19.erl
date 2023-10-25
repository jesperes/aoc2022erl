-module(day19).

-compile([export_all, nowarn_export_all]).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

%% Variable names
%%
%% O: Ore
%% C: Clay
%% B: Obsidian
%% G: Geodes
%%
%% OR: Number of ore robots
%% CR: Number of clay robots
%% BR: Number of obsidian robots
%% GR: Number of geode robots
%%
%% OOC: Ore robot ore cost
%% COC: Clay robot ore cost
%% BOC: Obsidian robot ore cost
%% BCC: Obsidian robot clay cost
%% GOC: Geode robot ore cost
%% GBC: Geode robot obsidian cost
%%
%% MaxOR: Maximum ore cost of any robot
%% MaxCR: Maximum clay cost of any robot
%% MaxBR: Maximum obsidian cost of any robot


%% These fields must come in the same order as they appear in the
%% blueprint, or the parser will not work.
-record(blueprint, { nr :: integer()
                   , ore_robot_c :: integer()
                   , clay_robot_c :: integer()
                   , obs_robot_ore_c :: integer()
                   , obs_robot_clay_c :: integer()
                   , geo_robot_ore_c :: integer()
                   , geo_robot_obs_c :: integer()
                   , max_obs_r :: integer()
                   , max_clay_r :: integer()
                   , max_ore_r :: integer()
                   }).

parse(Bin) ->
  lists:foldl(
    fun(<<>>, Acc) -> Acc;
       (Line, Acc) ->
        {match, Matches} =
          re:run(Line, "(\\d+)", [global, {capture, all_but_first, binary}]),

        Bp = #blueprint{} =
          list_to_tuple([blueprint] ++
                          lists:map(fun erlang:binary_to_integer/1,
                                    lists:flatten(Matches)) ++
                          [0, 0, 0]),

        Bp0 =
          Bp#blueprint{max_obs_r = Bp#blueprint.geo_robot_obs_c - 1,
                       max_clay_r = Bp#blueprint.obs_robot_clay_c - 2,
                       max_ore_r =
                         lists:max([Bp#blueprint.clay_robot_c,
                                    Bp#blueprint.obs_robot_ore_c,
                                    Bp#blueprint.geo_robot_ore_c])},

        [Bp0|Acc]
    end, [], binary:split(Bin, <<"\n">>, [global])).

plus(A, B) ->
  A + B.
mult(A, B) ->
  A * B.

solve() ->
  Parent = self(),
  Bin = input:get(19),
  Bps = lists:reverse(parse(Bin)),
  {Bps2, _} = lists:split(3, lists:reverse(parse(Bin))),

  %% Test all blueprints in parallel
  Pids =
    lists:map(fun(Bp) ->
                  spawn(fun() -> Parent ! {part1, search1(Bp)} end)
              end, Bps) ++
    lists:map(fun(Bp) ->
                  spawn(fun() -> Parent ! {part2, search2(Bp)} end)
              end, Bps2),

  lists:foldl(
    fun(_, {P1, P2}) ->
        receive
          {part1, Level} -> {P1 + Level, P2};
          {part2, Level} -> {P1, P2 * Level}
        end
    end, {0, 1}, Pids).

search1(Bp) ->
  search(Bp, 24) * Bp#blueprint.nr.

search2(Bp) ->
  search(Bp, 32).

search(Bp, Minute) ->
  {G, _} = dfs(Bp, #{}, [], Minute, 0, 0, 0, 0, 1, 0, 0, 0),
  G.

dfs(_Bp, Cache, _SkipList, _Min = 1, _O, _C, _B, G, _OR, _CR, _BR, GR) ->
  {G + GR, maps:update_with(states, fun(Old) -> Old + 1 end, 1, Cache)};
dfs(Bp, CacheIn, SkipList, Min, O, C, B, G, OR, CR, BR, GR) ->
  #blueprint{ore_robot_c = OOC,
             clay_robot_c = COC,
             obs_robot_ore_c = BOC,
             obs_robot_clay_c = BCC,
             geo_robot_ore_c = GOC,
             geo_robot_obs_c = GBC,
             max_ore_r = MaxOR,
             max_clay_r = MaxCR,
             max_obs_r = MaxBR} = Bp,

  Cache = maps:update_with(states, fun(Old) -> Old + 1 end, 1, CacheIn),
  CanBuildGeo = B >= GBC andalso O >= GOC,
  CanBuildObs = C >= BCC andalso O >= BOC andalso OR < MaxBR,
  CanBuildClay = O >= COC andalso CR < MaxCR,
  CanBuildOre = O >= OOC andalso OR < MaxOR,

  SkipOre = lists:member(ore, SkipList),
  SkipClay = lists:member(clay, SkipList),
  SkipObs = lists:member(obs, SkipList),

  Key = {O, C, B, G, OR, CR, BR, GR},
  case maps:get(Key, Cache, undefined) of
    Value when is_integer(Cache) ->
      {Value, Cache};

    %% If we can build a geode robot, do only that
    _ when CanBuildGeo ->
      dfs(Bp, Cache, [], Min - 1, O + OR - GOC, C + CR, B + BR - GBC, G + GR, OR, CR, BR, GR + 1);

    _ ->
      %% Build obsidian
      {Max0, Cache0} =
        ?IF(CanBuildObs andalso not SkipObs,
            dfs(Bp, Cache, [], Min - 1, O + OR - BOC, C + CR - BCC, B + BR, G + GR, OR, CR, BR + 1, GR),
            {0, Cache}),

      %% Build clay
      {Max1, Cache1} =
        ?IF(CanBuildClay andalso not SkipClay,
            dfs(Bp, Cache0, [], Min - 1, O + OR - COC, C + CR, B + BR, G + GR, OR, CR + 1, BR, GR),
            {0, Cache0}),

      %% Build ore
      {Max2, Cache2} =
        ?IF(CanBuildOre andalso not SkipOre,
            dfs(Bp, Cache1, [], Min - 1, O + OR - OOC, C + CR, B + BR, G + GR, OR + 1, CR, BR, GR),
            {0, Cache1}),

      %% Build nothing, just let robots mine new resources. In this
      %% case pass down a `SkipList' to avoid building resources next
      %% turn that we could've build this time -- if we could have
      %% built it this time, there is no point in waiting, just to
      %% build that same resource next time.
      SkipList0 =
        ?IF(CanBuildOre, [ore], []) ++
        ?IF(CanBuildClay, [clay], []) ++
        ?IF(CanBuildObs, [obs], []),
      {Max3, Cache3} = dfs(Bp, Cache2, SkipList0, Min - 1, O + OR, C + CR, B + BR, G + GR, OR, CR, BR, GR),

      %% Collect the maximum values found
      MaxOut = lists:max([Max0, Max1, Max2, Max3]),
      {MaxOut, Cache3}
  end.

-ifdef(TEST).

day19_test_() ->
  ?_assertEqual({1382, 31740}, solve()).

-endif.
