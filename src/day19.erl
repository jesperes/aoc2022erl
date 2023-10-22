-module(day19).

-compile([export_all, nowarn_export_all]).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

%% These fields must come in the same order as they appear in the
%% blueprint, or the parser will not work.
-record(blueprint, { nr :: integer()
                   , ore_robot_c :: integer()
                   , clay_robot_c :: integer()
                   , obs_robot_ore_c :: integer()
                   , obs_robot_clay_c :: integer()
                   , geo_robot_ore_c :: integer()
                   , geo_robot_obs_c :: integer()
                   }).

-record(limits, { max_obs_r :: integer()
                , max_clay_r :: integer()
                , max_ore_r :: integer()
                }).

-record(state, { min
               , ore = 0 :: integer()
               , clay = 0 :: integer()
               , obs = 0 :: integer()
               , geo = 0 :: integer()
               , ore_r = 1 :: integer()
               , clay_r = 0 :: integer()
               , obs_r = 0 :: integer()
               , geo_r = 0 :: integer()
               }).

parse(Bin) ->
  lists:foldl(
    fun(<<>>, Acc) -> Acc;
       (Line, Acc) ->
        {match, Matches} =
          re:run(Line, "(\\d+)", [global, {capture, all_but_first, binary}]),

        Bp = #blueprint{} =
          list_to_tuple([blueprint|lists:map(fun erlang:binary_to_integer/1,
                                             lists:flatten(Matches))]),

        Limits = #limits{} =
          list_to_tuple([limits|
                         [Bp#blueprint.geo_robot_obs_c,
                          Bp#blueprint.obs_robot_clay_c,
                          lists:max([
                            Bp#blueprint.clay_robot_c,
                            Bp#blueprint.obs_robot_ore_c,
                            Bp#blueprint.geo_robot_ore_c])]]),
        [{Bp, Limits}|Acc]
    end, [], binary:split(Bin, <<"\n">>, [global])).

solve() ->
  Bin = input:get(19),
  Bps = parse(Bin),
  lists:fold(
    fun({Bp, Limits}, Acc) ->
        search(Bp, Limits, 24) * Bp#blueprint.nr + Acc
    end, 0, Bps).

search(Bp, Limits, Minute) ->
  {Time, {_, Geo}} = timer:tc(fun() ->
                          do_search(Bp, Limits, _Cache = #{}, #state{min = Minute})
                      end),
  io:format("Blueprint ~p got ~p geodes in ~p minutes, took ~p millis~n",
            [Bp#blueprint.nr, Geo, Minute, Time / 1000.0]),
  Geo.

-spec do_search(#blueprint{}, #limits{}, map(), #state{}) ->
        {map(), integer()}.
do_search(_Bp, _Limits, Cache, #state{min = Min, geo = Geo} = _State) when Min == 0 ->
  {Cache, Geo};
do_search(Bp, Limits, Cache, State) ->
  case maps:get(State, Cache, undefined) of
    undefined ->
      {NewCache, NewMax} =
        lists:foldl(fun(Fun, {CacheIn, Max}) ->
                        %% io:format("Recursing: ~p ~p~n", [Fun, NextState]),
                        {CacheOut, NewMax} = Fun(Bp, Limits, CacheIn, State),
                        {CacheOut, lists:max([Max, NewMax])}
                    end, {Cache, 0},
                    [fun maybe_build_geo_r/4,
                     fun maybe_build_obs_r/4,
                     fun maybe_build_clay_r/4,
                     fun maybe_build_ore_r/4,
                     fun no_build/4]),

      CacheOut = maps:put(State, NewMax, NewCache),
      {CacheOut, NewMax};
    Value ->
      {Cache, Value}
  end.

%% Maybe build geode robot
maybe_build_geo_r(
  #blueprint{geo_robot_obs_c = ObsC, geo_robot_ore_c = OreC} = Bp,
  Limits,
  Cache,
  #state{ore = Ore, obs = Obs} = State)
  when
    Obs >= ObsC andalso Ore >= OreC ->

  State0 = State#state{
             min = State#state.min - 1,
             ore = State#state.ore + State#state.ore_r - OreC,
             clay = State#state.clay + State#state.clay_r,
             obs = State#state.obs + State#state.obs_r - ObsC,
             geo = State#state.geo + State#state.geo_r,
             %% no change in ore_r
             %% no change in clay_r
             %% no change in obs_r
             geo_r = State#state.geo_r + 1
            },
  do_search(Bp, Limits, Cache, State0);
maybe_build_geo_r(_Bp, _, Cache, _State) ->
  {Cache, 0}.

%% Maybe build obsidian robot
maybe_build_obs_r(
  #blueprint{obs_robot_ore_c = OreC, obs_robot_clay_c = ClayC} = Bp,
  #limits{max_obs_r = MaxObs} = Limits,
  Cache,
  #state{ore = Ore, clay = Clay, obs_r = ObsR} = State)
  when
    Ore >= OreC andalso Clay >= ClayC andalso ObsR < MaxObs ->
  State0 = State#state{
             min = State#state.min - 1,
             ore = State#state.ore + State#state.ore_r - OreC,
             clay = State#state.clay + State#state.clay_r - ClayC,
             obs = State#state.obs + State#state.obs_r,
             geo = State#state.geo + State#state.geo_r,
             %% no change in ore_r
             %% no change in clay_r
             obs_r = State#state.obs_r + 1
             %% no change in geo_r
            },
  do_search(Bp, Limits, Cache, State0);
maybe_build_obs_r(_Bp, _Limits, Cache, _State) ->
  {Cache, 0}.

%% Maybe build clay robot
maybe_build_clay_r(
  #blueprint{clay_robot_c = OreC} = Bp,
  #limits{max_clay_r = MaxClay} = Limits,
  Cache,
  #state{clay_r = ClayR, ore = Ore} = State)
  when
    Ore >= OreC andalso ClayR < MaxClay ->
  State0 = State#state{
             min = State#state.min - 1,
             ore = State#state.ore + State#state.ore_r - OreC,
             clay = State#state.clay + State#state.clay_r,
             obs = State#state.obs + State#state.obs_r,
             geo = State#state.geo + State#state.geo_r,
             %% no change in ore_r
             clay_r = State#state.clay_r + 1
             %% no change in obs_r
             %% no change in geo_r
            },
  do_search(Bp, Limits, Cache, State0);

maybe_build_clay_r(_Bp, _Limits, Cache, _State) ->
  {Cache, 0}.

%% Maybe build ore robot
maybe_build_ore_r(
  #blueprint{ore_robot_c = OreC} = Bp,
  #limits{max_ore_r = MaxOre} = Limits,
  Cache,
  #state{ore_r = OreR, ore = Ore} = State)
  when
    Ore >= OreC andalso OreR < MaxOre ->
  State0 = State#state{
             min = State#state.min - 1,
             ore = State#state.ore + State#state.ore_r - OreC,
             clay = State#state.clay + State#state.clay_r,
             obs = State#state.obs + State#state.obs_r,
             geo = State#state.geo + State#state.geo_r,
             ore_r = State#state.ore_r + 1
             %% no chanbe in clay_r
             %% no change in obs_r
             %% no change in geo_r
            },
  do_search(Bp, Limits, Cache, State0);
maybe_build_ore_r(_Bp, _Limits, Cache, _State) ->
  {Cache, 0}.

%% Don't build anything
no_build(Bp, Limits, Cache, State) ->
  State0 = State#state{
             min = State#state.min - 1,
             ore = State#state.ore + State#state.ore_r,
             clay = State#state.clay + State#state.clay_r,
             obs = State#state.obs + State#state.obs_r,
             geo = State#state.geo + State#state.geo_r
             %% no change in ore_R
             %% no change in clay_r
             %% no change in obs_r
             %% no change in geo_r
            },
  do_search(Bp, Limits, Cache, State0).

-ifdef(TEST).

day19_test() ->
  ok.
  %% ok = solve().

-endif.
