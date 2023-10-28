-module(day20).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

-define(ENCRYPTION_KEY, 811589153).

solve() ->
  Numbers =
    lists:foldr(
      fun(<<>>, Acc) -> Acc;
         (Bin, Acc) ->
          [binary_to_integer(Bin)|Acc]
      end, [], binary:split(input:get(20), <<"\n">>, [global])),

  {Queue, List} = make_queue(Numbers),
  Queue1 = mix(Queue, List),
  find_p1(Queue1).

  %% Numbers2 = lists:map(fun(N) -> N * ?ENCRYPTION_KEY end, Numbers),
  %% lists:foldl(fun(_, Acc) ->

  %%             end,


find_p1(Queue) ->
  Q0 = rotate_number_to_front(0, Queue),
  Q1 = rotate(-1000, Q0),
  {_, A} = queue:head(Q1),
  Q2 = rotate(-1000, Q1),
  {_, B} = queue:head(Q2),
  Q3 = rotate(-1000, Q2),
  {_, C} = queue:head(Q3),
  A + B + C.

make_queue(Numbers) ->
  Len = length(Numbers),
  List = lists:zip(lists:seq(0, Len - 1), Numbers),
  {queue:from_list(List), List}.

mix(Queue, List) ->
  Len = length(List),
  lists:foldl(
    fun({Idx, Num}, QueueIn) ->
        ?assertEqual(Len, queue:len(QueueIn)),
        Q1 = rotate_index_to_front(Idx, QueueIn),
        {{value, Head}, Q2} = queue:out(Q1),
        Len0 = Len - 1, %% Queue is 1 element shorter right now
        Shift = (Num + Len0 * 3) rem Len0, %% * 3 to avoid issues with rem and negative numbers
        Q3 = rotate(-Shift, Q2),
        Q4 = queue:in(Head, Q3),
        Q4
      end, Queue, List).

rotate_index_to_front(Idx, Queue) ->
  case queue:head(Queue) of
    {Idx0, _} when Idx == Idx0 ->  Queue;
    _ -> rotate_index_to_front(Idx, rotate(-1, Queue))
  end.

rotate_number_to_front(Num, Queue) ->
  case queue:head(Queue) of
    {_, N} when N == Num ->  Queue;
    _ -> rotate_number_to_front(Num, rotate(-1, Queue))
  end.

%% If viewing the queue with the head to the left and the tail to the
%% right, rotating by a negative amount means leftwards, i.e. popping
%% the head and inserting it at the tail, and vice versa.
rotate(0, Queue) -> Queue;
rotate(N, Queue) when N > 0 ->
  {{value, Rear}, Q0} = queue:out_r(Queue),
  Q1 = queue:in_r(Rear, Q0),
  rotate(N - 1, Q1);
rotate(N, Queue) when N < 0 ->
  {{value, Head}, Q0} = queue:out(Queue),
  Q1 = queue:in(Head, Q0),
  rotate(N + 1, Q1).

unzipped(L) ->
  {_, L0} = lists:unzip(L),
  L0.

-ifdef(TEST).

rotate_to_front_test() ->
  Q = queue:from_list([{0,foo},
                       {1,bar},
                       {2,gurka},
                       {3,banan}]),

  Q1 = rotate_index_to_front(2, Q),
  ?assertEqual({value, {2, gurka}}, queue:peek(Q1)),

  Q2 = rotate_index_to_front(0, Q),
  ?assertEqual({value, {0, foo}}, queue:peek(Q2)),

  Q3 = rotate_index_to_front(3, Q),
  ?assertEqual({value, {3, banan}}, queue:peek(Q3)).

rotate_test() ->
  Q = queue:from_list([{0,foo},
                       {1,bar},
                       {2,gurka},
                       {3,banan}]),

  lists:foreach(
    fun({Shift, ExpectedHead}) ->
        ?assertEqual(ExpectedHead, queue:head(rotate(Shift, Q)))
    end, [{0, {0, foo}},
          {-1, {1, bar}},
          {-2, {2, gurka}},
          {-3, {3, banan}},
          {-4, {0, foo}},
          {-5, {1, bar}},
          {1, {3, banan}},
          {2, {2, gurka}}]).

mix_test() ->
  Numbers = [1, 2, -3, 3, -2, 0, 4],
  {Queue, List} = make_queue(Numbers),
  Q = mix(Queue, List),
  L = unzipped(queue:to_list(rotate_number_to_front(1, Q))),
  ?assertEqual([1, 2, -3, 4, 0, 3, -2], L).

%% find_p1_test() ->
%%   Numbers = [1, 2, -3, 4, 0, 3, -2],
%%   Len = length(Numbers),
%%   List = lists:zip(lists:seq(1, Len), Numbers),
%%   Queue = queue:from_list(List),
%%   ?assertEqual(3, find_p1(Queue)).

day20_test() ->
  ?assertEqual({7278, 14375678667089}, solve()).

-endif.
