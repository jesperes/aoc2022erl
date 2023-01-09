-module(day16).

-export([ solve/0
        ]).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(MAXDIST, 10000).

%% Ideas borrowed from
%% https://www.reddit.com/r/adventofcode/comments/zn6k1l/comment/j2xhog7/

solve() ->
  Bin = input:get(16),

  Lines = binary:split(Bin, <<"\n">>, [global]),
  Input =
    lists:foldl(
      fun(<<>>, Acc) -> Acc;
         (Line, Acc) ->
          {match, [Valve, FlowRate, Tunnels]} =
            re:run(Line, "Valve (.*) has flow rate=(.*); tunnels? leads? to valves? (.*)",
                   [{capture, all_but_first, binary}]),
          [{binary_to_atom(Valve), binary_to_integer(FlowRate),
            lists:map(fun binary_to_atom/1,
                      binary:split(Tunnels, <<", ">>, [global]))}|Acc]
      end, [], Lines),

  Graph = lists:foldl(fun({Valve, _FlowRate, Tunnels}, Acc) ->
                          maps:put(Valve, Tunnels, Acc)
                      end, #{}, Input),

  Valves = maps:keys(Graph),

  Indices = maps:from_list(
              lists:map(fun({Idx, Valve}) -> {Valve, 1 bsl Idx} end,
                        lists:enumerate(0, Valves))),

  Flows = lists:foldl(
            fun({_, 0, _}, Acc) -> Acc;
               ({Valve, FlowRate, _}, Acc) ->
                maps:put(Valve, FlowRate, Acc)
            end, #{}, Input),

  %% Use floyd-warshall to compute the distance between any pair of
  %% valves
  Dists =
    lists:foldl(
      fun({V, L}, Acc) ->
          case lists:member(L, maps:get(V, Graph)) of
            true -> maps:put({V, L}, 1, Acc);
            false -> maps:put({V, L}, ?MAXDIST, Acc)
          end
      end, #{}, [{V, L} || V <- Valves, L <- Valves]),

  Dists0 =
    lists:foldl(fun({K, I, J}, Acc) ->
                    DistIJ = maps:get({I, J}, Acc),
                    DistIK = maps:get({I, K}, Acc),
                    DistKJ = maps:get({K, J}, Acc),
                    maps:put({I, J}, min(DistIJ, DistIK + DistKJ), Acc)
                end,
                Dists,
                [{K, I, J} || K <- Valves,
                              I <- Valves,
                              J <- Valves]),

  InputData = {Flows, Dists0, Indices},

  MaxFlows = visit('AA', 30, 0, 0, #{}, InputData),
  Part1 = lists:max(maps:values(MaxFlows)),

  Visited = visit('AA', 26, 0, 0, #{}, InputData),
  VisitedList = maps:to_list(Visited),
  Part2 = lists:max([V1 + V2 || {B1, V1} <- VisitedList,
                                {B2, V2} <- VisitedList,
                                (B1 band B2) =:= 0]),

  {Part1, Part2}.


%% Valve    -- the valve we are visiting
%% Minutes  -- how many minutes have passed at this point
%% Bitmask  -- a bitmask indicating which valves are open at this point
%% Pressure -- current pressure
%% Answer   -- a map where the keys are sets of valves (as bitmasks) and
%%             the values are the maximum pressure found with those
%%             particular valves open.
visit(Valve, Minutes, Bitmask, Pressure, Answer, {Flows, Dists, Indices} = InputData) ->
  maps:fold(
    fun(Valve2, Flow, AnswerIn) ->
        case Minutes - maps:get({Valve, Valve2}, Dists) - 1 of
          RemMins when RemMins =< 0 ->
            AnswerIn;
          RemMins ->
            I = maps:get(Valve2, Indices),
            case Bitmask band I of
              0 ->
                visit(Valve2, RemMins, Bitmask bor I,
                      Pressure + Flow * RemMins,
                      AnswerIn, InputData);
              _ ->
                AnswerIn
            end
        end
    end,
    _Answer = maps:update_with(
                Bitmask,
                fun(Old) ->
                    max(Old, Pressure)
                end, Pressure, Answer),
    Flows).

-ifdef(TEST).

day13_test() ->
  {1376, 1933} = solve().

-endif.
