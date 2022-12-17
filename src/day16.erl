-module(day16).

-export([ solve/0
        ]).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  Bin = input:get(16),
  %%   Bin = <<"Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
  %% Valve BB has flow rate=13; tunnels lead to valves CC, AA
%% Valve CC has flow rate=2; tunnels lead to valves DD, BB
%% Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
%% Valve EE has flow rate=3; tunnels lead to valves FF, DD
%% Valve FF has flow rate=0; tunnels lead to valves EE, GG
%% Valve GG has flow rate=0; tunnels lead to valves FF, HH
%% Valve HH has flow rate=22; tunnel leads to valve GG
%% Valve II has flow rate=0; tunnels lead to valves AA, JJ
%% Valve JJ has flow rate=21; tunnel leads to valve II">>,
  Lines = binary:split(Bin, <<"\n">>, [global]),
  Valves =
    lists:foldl(
      fun(<<>>, Acc) -> Acc;
         (Line, Acc) ->
          {match, [Valve, FlowRate, Tunnels]} =
            re:run(Line, "Valve (.*) has flow rate=(.*); tunnels? leads? to valves? (.*)",
                   [{capture, all_but_first, binary}]),
          Tunnels0 = binary:split(Tunnels, <<", ">>, [global]),
          digraph:add_vertex(Acc, Valve, FlowRate),
          lists:foreach(fun(T) ->
                            digraph:add_edge(Acc, Valve, T)
                        end, Tunnels0)
          %% [{Valve, binary_to_integer(FlowRate), Tunnels0}|Acc]
      end, digraph:new(), Lines).

to_dot(Filename, Valves) ->
  Str =
    "digraph {\n" ++
    lists:map(
      fun({Valve, FlowRate, Tunnels}) ->
          io_lib:format("  ~s [label=\"~s ~w\"];~n", [Valve, Valve, FlowRate]) ++
          lists:map(fun(T) ->
                        io_lib:format("  ~s -> ~s;~n", [Valve, T])
                    end, Tunnels)
      end, Valves) ++
    "}\n",
  file:write_file(Filename, Str).

-ifdef(TEST).

day13_test() ->
  ok.

-endif.
