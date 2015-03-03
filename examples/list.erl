-module(list).
-export([list_length/1, reverse/1]).

list_length([]) ->
  0;
list_length([First | Rest]) ->
  1 + list_length(Rest).

reverse(List) ->
  reverse(List, []).

reverse([Head | Rest], Reverted_List) ->
  reverse(Rest, [Head | Reverted_List]);
reverse([], List) ->
  List.
