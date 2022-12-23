-module(day19).

-compile([export_all, nowarn_export_all]).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

-define(int(X), binary_to_integer(X)).

-record(blueprint, { ore_robot_cost
                   , clay_robot_cost
                   , obsidian_robot_cost
                   , geode_robot_cost
                   }).

-record(state, { remaining_minutes = 24
               , geode = 0
               , geode_robots = 0
               , obsidian = 0
               , obsidian_robots = 0
               , clay = 0
               , clay_robots = 0
               , ore = 0
               , ore_robots = 1
               }).

parse(Bin) ->
  lists:foldl(
    fun(<<>>, Acc) -> Acc;
       (Line, Acc) ->
        {match, [ Id
                , OreRobot_OreCost
                , ClayRobot_OreCost
                , ObsidianRobot_OreCost
                , ObsidianRobot_ClayCost
                , GeodeRobot_OreCost
                , GeodeRobot_ObsidianCost
                ]} =
          re:run(Line,
                 "Blueprint (.*): "
                 "Each ore robot costs (.*) ore. "
                 "Each clay robot costs (.*) ore. "
                 "Each obsidian robot costs (.*) ore and (.*) clay. "
                 "Each geode robot costs (.*) ore and (.*) obsidian.",
                 [{capture, all_but_first, binary}]),
        Blueprint = #blueprint{ ore_robot_cost = ?int(OreRobot_OreCost)
                              , clay_robot_cost = ?int(ClayRobot_OreCost)
                              , obsidian_robot_cost = {?int(ObsidianRobot_OreCost),
                                                       ?int(ObsidianRobot_ClayCost)}
                              , geode_robot_cost = {?int(GeodeRobot_OreCost),
                                                    ?int(GeodeRobot_ObsidianCost)}},
        maps:put(?int(Id), Blueprint, Acc)
    end, #{}, binary:split(Bin, <<"\n">>, [global])).

solve() ->
  %% Bin = input:get(19),
  Bin = <<"Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n"
          "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.\n">>,
  Blueprints = parse(Bin),
  quality_level(maps:get(1, Blueprints)).
  %% maps:fold(fun(_Id, Blueprint, Acc) ->
  %%                 Acc + quality_level(Blueprint)
  %%             end, 0, Blueprints).

quality_level(Blueprint) ->
  Root = #state{},
  Queue = gb_sets:from_list([Root]),
  Explored = sets:from_list([Root]),
  bfs(Queue, Explored, Blueprint).

bfs(Queue, Explored, Blueprint) ->
  {Node, Queue0} = gb_sets:take_largest(Queue),
  erlang:display(Node),

  case Node#state.remaining_minutes of
    Min when Min == 0 ->
      Node;
    _Min ->
      Nodes = maybe_build_ore_robot(Node, Blueprint, [Node]),
      Nodes0 = maybe_build_clay_robot(Node, Blueprint, Nodes),
      Nodes1 = maybe_build_obsidian_robot(Node, Blueprint, Nodes0),
      Nodes2 = maybe_build_geode_robot(Node, Blueprint, Nodes1),
      Nodes3 = lists:map(fun collect_resources/1, Nodes2),
      %% io:format("~p~n", [Nodes3]),
      {QueueOut, ExploredOut} =
        lists:foldl(
          fun(N, {Q, E} = Acc) ->
              case sets:is_element(N, E) of
                true -> Acc;
                false ->
                  {gb_sets:add_element(N, Q),
                   sets:add_element(N, E)}
              end
          end, {Queue0, Explored}, Nodes3),
      %% io:format("Queue = ~p~n", [gb_sets:to_list(QueueOut)]),
      bfs(QueueOut, ExploredOut, Blueprint)
  end.

%% Build ore robot
maybe_build_ore_robot(#state{ore = Available, ore_robots = Robots} = State,
                      #blueprint{ore_robot_cost = Cost}, Acc) when Cost =< Available ->
  [State#state{ore = Available - Cost, ore_robots = Robots + 1}|Acc];
maybe_build_ore_robot(_, _, Acc) -> Acc.

%% Build clay robot
maybe_build_clay_robot(#state{ore = Available, clay_robots = Robots} = State,
                      #blueprint{clay_robot_cost = Cost}, Acc) when Cost =< Available ->
  [State#state{ore = Available - Cost, clay_robots = Robots + 1}|Acc];
maybe_build_clay_robot(_, _, Acc) -> Acc.

%% Build obsidian robot
maybe_build_obsidian_robot(#state{ore = AvailableOre, clay = AvailableClay, obsidian_robots = Robots} = State,
                           #blueprint{obsidian_robot_cost = {OreCost, ClayCost}}, Acc) when
    OreCost =< AvailableOre andalso
    ClayCost =< AvailableClay ->
  [State#state{ore = AvailableOre - OreCost,
               clay = AvailableClay - ClayCost,
               obsidian_robots = Robots + 1}|Acc];
maybe_build_obsidian_robot(_, _, Acc) -> Acc.

%% Build geode robot
maybe_build_geode_robot(#state{ore = AvailableOre, obsidian = AvailableObsidian, geode_robots = Robots} = State,
                        #blueprint{geode_robot_cost = {OreCost, ObsidianCost}}, Acc) when
    OreCost =< AvailableOre andalso
    ObsidianCost =< AvailableObsidian ->
  [State#state{ore = AvailableOre - OreCost,
               obsidian = AvailableObsidian - ObsidianCost,
               geode_robots = Robots + 1}|Acc];
maybe_build_geode_robot(_, _, Acc) -> Acc.

collect_resources(Node) ->
  Node#state{ore = Node#state.ore + Node#state.ore_robots,
             clay = Node#state.clay + Node#state.clay_robots,
             obsidian = Node#state.obsidian + Node#state.obsidian_robots,
             geode = Node#state.geode + Node#state.geode_robots,
             remaining_minutes = Node#state.remaining_minutes - 1}.

-ifdef(TEST).

day19_test() ->
  ok = solve().

-endif.
