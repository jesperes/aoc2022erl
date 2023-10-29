-module(ring).

-export([from_list/1,
         to_list/1,
         shift/2,
         insert/2,
         remove/1,
         peek/1,
         move_element_to_head/2,
         length/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-record(ring, { head = 0 :: integer()
              , length = 0 :: integer()
              , elements = [] :: list(term())
              }).

-opaque ring() :: #ring{}.

-export_type([ring/0]).

-spec from_list(List :: list()) -> ring().
from_list(List) ->
  #ring{head = 0, length = erlang:length(List), elements = List}.

%% Convert the ring into a list where the first elements in the list
%% is the head of the ring.
-spec to_list(Ring :: ring()) -> list().
to_list(#ring{head = Head, elements = Elements}) ->
  {A, B} = lists:split(Head, Elements),
  B ++ A.

-spec length(Ring :: ring()) -> integer().
length(#ring{length = Len}) ->
   Len.

%% @doc Shift the head of the ring `N' steps. Negative means
%% leftwards, i.e. removing N elements from the head and inserting
%% them at the end, and positive shifts means rightwards.
-spec shift(N :: integer(), Ring :: ring()) -> ring().
shift(0, Ring) -> Ring;
shift(N, #ring{head = Head, length = Len} = Ring) ->
  NewHead =
    case (Head - N) rem Len of
      H when H < 0 -> Len + H;
      H -> H
    end,
  Ring#ring{head = NewHead}.

-spec move_element_to_head(Elem :: term(), Ring :: ring()) -> ring().
move_element_to_head(Elem, #ring{elements = Elements} = Ring) ->
  Ring#ring{head = find_elem(Elem, Elements, 0)}.

find_elem(Elem, [Elem|_], Idx) ->
  Idx;
find_elem(Elem, [_|Elems], Idx) ->
  find_elem(Elem, Elems, Idx + 1).

-spec peek(Ring :: ring()) -> term().
peek(#ring{head = Head, elements = Elems}) ->
  lists:nth(Head + 1, Elems).

%% Insert an element at the head. All elements are moved one step
%% rightwards.
-spec insert(Element :: term(), Ring :: ring()) -> ring().
insert(Element, #ring{head = Head,
                      length = Length,
                      elements = Elements}) ->
  {A, B} = lists:split(Head, Elements),
  #ring{head = Head,
        length = Length + 1,
        elements = A ++ [Element] ++ B}.

-spec remove(Ring :: ring()) -> {Element :: term(), Ring0 :: ring()}.
remove(#ring{head = Head,
             length = Length,
             elements = Elements}) ->
  {A, [HeadElem|B]} = lists:split(Head, Elements),
  {HeadElem, #ring{head = Head, length = Length - 1, elements = A ++ B}}.


-ifdef(TEST).
shift_test() ->
  %% Positive, i.e. rightwards
  ?assertEqual([4,1,2,3], to_list(shift(1, from_list([1,2,3,4])))),
  %% Negative, i.e. leftwards
  ?assertEqual([2,3,4,1], to_list(shift(-1, from_list([1,2,3,4])))).

shift2_test() ->
  Ring = from_list([1,2,3,4,5]),
  lists:foreach(fun(Offset) ->
                    R0 = shift(-Offset, shift(Offset, Ring)),
                    ?assertEqual(to_list(R0), to_list(Ring))
                end, lists:seq(-100, 100)).

insert_test() ->
  R = from_list([1,2,3,4]),
  R0 = shift(-2, R),
  R1 = insert(42, R0),
  R2 = shift(2, R1),
  ?assertEqual([1,2,42,3,4], to_list(R2)).

remove_test() ->
  R = from_list([1,2,3,4]),
  {1, R0} = remove(R),
  ?assertEqual([2,3,4], to_list(R0)).

-endif.
