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

solve() ->
  {solve(1), solve(2)}.

solve(1) ->
  ?debugFmt("Starting part 1", []),
  Parent = self(),
  Bin = input:get(19),
  Bps = lists:reverse(parse(Bin)),
  Pids1 =
    lists:map(
      fun(Bp) ->
          spawn(fun() ->
                    Parent ! search(Bp, 24) * Bp#blueprint.nr
                end)
      end, Bps),

  lists:foldl(
    fun(_, Acc) ->
        receive
          N -> N + Acc
        end
    end, 0, Pids1);
solve(2) ->
  ?debugFmt("Starting part 2", []),
  Parent = self(),
  Bin = input:get(19),
  Bps = lists:reverse(parse(Bin)),
  Pids1 =
    lists:map(
      fun(Bp) ->
          spawn(fun() ->
                    Parent ! search(Bp, 32)
                end)
      end, Bps),

  lists:foldl(
    fun(_, Acc) ->
        receive
          N -> N * Acc
        end
    end, 1, Pids1).

search(Bp, Minute) ->
  {Time, {G, Cache}} =
    timer:tc(fun() ->
                 dfs(Bp, #{}, Minute, 0, 0, 0, 0, 1, 0, 0, 0)
             end),
  ?debugFmt("[~p] Blueprint ~p got ~p geodes in ~p minutes, took ~p millis, states explored = ~p",
            [self(), Bp#blueprint.nr, G, Minute, Time / 1000.0, maps:get(states, Cache)]),
  G.

dfs(_Bp, Cache, _Min = 1, _O, _C, _B, G, _OR, _CR, _BR, GR) ->
  {G + GR, maps:update_with(states, fun(Old) -> Old + 1 end, 1, Cache)};
dfs(Bp, CacheIn, Min, O, C, B, G, OR, CR, BR, GR) ->
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

  Key = {O, C, B, G, OR, CR, BR, GR},
  case maps:get(Key, Cache, undefined) of
    Value when is_integer(Cache) ->
      {Value, Cache};

    %% If we can build a geode robot, do only that
    _ when CanBuildGeo ->
      dfs(Bp, Cache, Min - 1, O + OR - GOC, C + CR, B + BR - GBC, G + GR, OR, CR, BR, GR + 1);

    %% If there are 2 minutes left and we are not building an obsidian
    %% robot, we can short-circuit here.
    _ when Min == 2 andalso not CanBuildObs ->
      {G + GR * 2, Cache};

    _ ->
      {Max0, Cache0} =
        if CanBuildObs ->
            dfs(Bp, Cache, Min - 1, O + OR - BOC, C + CR - BCC, B + BR, G + GR, OR, CR, BR + 1, GR);
           true -> {0, Cache}
        end,

      {Max1, Cache1} =
        if CanBuildClay ->
            dfs(Bp, Cache0, Min - 1, O + OR - COC, C + CR, B + BR, G + GR, OR, CR + 1, BR, GR);
           true -> {0, Cache0}
        end,

      {Max2, Cache2} =
        if CanBuildOre ->
            dfs(Bp, Cache1, Min - 1, O + OR - OOC, C + CR, B + BR, G + GR, OR + 1, CR, BR, GR);
           true -> {0, Cache1}
        end,

      {Max3, Cache3} = dfs(Bp, Cache2, Min - 1, O + OR, C + CR, B + BR, G + GR, OR, CR, BR, GR),
      {lists:max([Max0, Max1, Max2, Max3]), Cache3}
  end.

-ifdef(TEST).

day19_test_() ->
  [{timeout, 60000, fun() -> 1382 = solve(1) end},
   {timeout, 60000, fun() -> 31740 = solve(2) end}].

-endif.
