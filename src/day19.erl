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
  Parent = self(),
  Bin = input:get(19),
  Bps = lists:reverse(parse(Bin)),
  %% {Bps2, _} = lists:split(3, Bps),
  Bps2 = [],

  AllRuns =
    lists:map(fun(Bp) -> {part1, Bp} end, Bps) ++
    lists:map(fun(Bp) -> {part2, Bp} end, Bps2),

  Pids =
    lists:map(
      fun({part1, Bp}) ->
          spawn(fun() ->
                    Parent ! {part1, search(Bp, 24) * Bp#blueprint.nr}
                end);
         ({part2, Bp}) ->
          spawn(fun() ->
                    Parent ! {part2, search(Bp, 32)}
                end)
      end, AllRuns),

  lists:foldl(
    fun(_, {P1, P2}) ->
        receive
          {part1, N} -> {N + P1, P2};
          {part2, N} -> {P2, N + P2}
        end
    end, {0, 0}, Pids).



solve0() ->
  Bps = parse(input:get(19)),
  lists:foreach(fun(#blueprint{nr = 21} = Bp) ->
                    ?assertEqual(13, search(Bp, 24));
                   (_) ->
                    ok
                end, Bps).

search(Bp, Minute) ->
  {Time, {G, _}} =
    timer:tc(fun() ->
                 dfs(Bp, #{}, Minute, 0, 0, 0, 0, 1, 0, 0, 0)
             end),
  io:format("[~p] Blueprint ~p got ~p geodes in ~p minutes, took ~p millis~n",
            [self(), Bp#blueprint.nr, G, Minute, Time / 1000.0]),
  G.

dfs(_Bp, Cache, _Min = 1, _O, _C, _B, G, _OR, _CR, _BR, GR) ->
  {G + GR, Cache};
dfs(Bp, Cache, Min, O, C, B, G, OR, CR, BR, GR) ->
  #blueprint{ore_robot_c = OOC,
             clay_robot_c = COC,
             obs_robot_ore_c = BOC,
             obs_robot_clay_c = BCC,
             geo_robot_ore_c = GOC,
             geo_robot_obs_c = GBC,
             max_ore_r = MaxOR,
             max_clay_r = MaxCR,
             max_obs_r = MaxBR} = Bp,

  Key = {O, C, B, G, OR, CR, BR, GR},
  case maps:get(Key, Cache, undefined) of
    Value when is_integer(Cache) ->
      {Value, Cache};
    _ ->
      if B >= GBC andalso O >= GOC ->
          dfs(Bp, Cache, Min - 1, O + OR - GOC, C + CR, B + BR - GBC, G + GR, OR, CR, BR, GR + 1);
         true ->
          %% No need to explore other branches if we have already
          %% built a geode robot
          {Max0, Cache0} =
            if C >= BCC andalso O >= BOC andalso BR < MaxBR ->
                dfs(Bp, Cache, Min - 1, O + OR - BOC, C + CR - BCC, B + BR, G + GR, OR, CR, BR + 1, GR);
               true -> {0, Cache}
            end,

          if Min == 2 ->
              %% If there are 2 minutes left and we are not building an
              %% obsidian robot, we can short-circuit here.
              {G + GR * 2, Cache0};
             true ->
              {Max1, Cache1} =
                if O >= COC andalso CR < MaxCR ->
                    dfs(Bp, Cache0, Min - 1, O + OR - COC, C + CR, B + BR, G + GR, OR, CR + 1, BR, GR);
                   true -> {0, Cache0}
                end,

              {Max2, Cache2} =
                if O >= OOC andalso OR < MaxOR ->
                    dfs(Bp, Cache1, Min - 1, O + OR - OOC, C + CR, B + BR, G + GR, OR + 1, CR, BR, GR);
                   true -> {0, Cache1}
                end,

              {Max3, Cache3} = dfs(Bp, Cache2, Min - 1, O + OR, C + CR, B + BR, G + GR, OR, CR, BR, GR),
              {lists:max([Max0, Max1, Max2, Max3]), Cache3}
          end
      end
  end.

-ifdef(TEST).

day19_test_() ->
  {timeout, 6000, fun() -> {1382, 0} = solve() end}.

-endif.
