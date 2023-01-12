%% Some common code to cut-n-paste into AoC solutions

-module(template).

-export([ solve/0
        ]).

-compile([ export_all
         , nowarn_export_all
         ]).

-include_lib("eunit/include/eunit.hrl").

-define(int(X), binary_to_integer(X)).
-define(atom(X), binary_to_atom(X)).
-define(match(Subject, RE), re:run(Subject, RE, [{capture, all_but_first, binary}])).

%% Delta coordinates for N/S/E/W
-define(DELTAS, [{0, -1}, {0, 1}, {-1, 0}, {1, 0}]).

%% State record for various processing tasks
-record(state, {}).

%% Some commonly used types
-type coord() :: {X :: integer(), Y :: integer()}.
-type coord3d() :: {X :: integer(), Y :: integer(), Z :: integer()}.

solve() ->
  Day = 1,
  Bin = input:get(Day),

  %% Split input into lines
  Lines = binary:split(Bin, <<"\n">>, [global]),

  %% Split lines of input into words
  _Words = lists:foldl(
             fun(<<>>, Acc) -> Acc; %% Ignore trailing empty line
                (Line, Acc) ->
                 [binary:split(Line, <<" ">>, [global])|Acc]
             end, [], Lines),

  %% Split lines using regexp
  _RegexWords =
    lists:foldl(
      fun(<<>>, Acc) -> Acc; %% Ignore trailing empty line
         (Line, Acc) ->
          {match, [A, B]} = ?match(Line, "(.*): (.*)"),
          [{A, B}|Acc]
      end, [], Lines),

  %% Split lines into a map
  _Map =
    lists:foldl(
      fun(<<>>, Acc) -> Acc; %% Ignore trailing empty line
         (Line, Acc) ->
          {match, [A, B]} = ?match(Line, "(.*): (.*)"),
          maps:put(?atom(A), B, Acc)
      end, #{}, Lines),

  %% Iterate with state over the input
  StateIn = #state{},
  StateOut = lists:foldl(
               fun(Line, State0) ->
                   modify_state(Line, State0)
               end, StateIn, Lines),

  {StateOut, Bin}.

modify_state(_Line, State) ->
  State.

%% Utility functions
%% =============================================================================

%% Manhattan distance
-spec dist(A :: coord(), B :: coord()) -> integer().
dist({X0, Y0}, {X1, Y1}) ->
  abs(X0 - X1) + abs(Y0 - Y1).

-spec dist3d(A :: coord3d(), B :: coord3d()) -> integer().
dist3d({X0, Y0, Z0}, {X1, Y1, Z1}) ->
  abs(X0 - X1) + abs(Y0 - Y1) + abs(Z0 - Z1).

%% Tests
%% =============================================================================

-ifdef(TEST).

solve_test() ->
  ok.

-endif.
