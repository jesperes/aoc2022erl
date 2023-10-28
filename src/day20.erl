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
  P1 = sum3(Queue1),

  Numbers2 = lists:map(fun(N) -> N * ?ENCRYPTION_KEY end, Numbers),
  {Queue2, List2} = make_queue(Numbers2),
  Queue3 = lists:foldl(fun(_N, Q) ->
                           %% io:format(standard_error, "P2 round ~p~n", [N]),
                           mix(Q, List2)
                       end, Queue2, lists:seq(1, 10)),
  P2 = sum3(Queue3),
  {P1, P2}.


sum3(Queue) ->
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

-ifdef(TEST).

day20_test_() ->
  {timeout, 600, ?_assertEqual({7278, 14375678667089}, solve())}.

-endif.
