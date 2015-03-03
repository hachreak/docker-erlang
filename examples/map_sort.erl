-module(map_sort).
-export([convert_list_to_c/1]).

% Example:
% map_sort:convert_list_to_c([{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}}, {paris, {f,28}}, {london, {f, 36}}]).
% [{moscow,{c,-10}},
%  {stockholm,{c,-4}},
%  {paris,{c,-2}},
%  {london,{c,2}},
%  {cape_town,{c,21}}]

convert_to_c({Name, {f, Temp}}) ->
  {Name, {c, trunc((Temp - 32)*5/9)}};
convert_to_c({Name, {c, Temp}}) ->
  {Name, {c, Temp}}.

convert_list_to_c(List) ->
  ConvertedList = lists:map(fun convert_to_c/1, List),
  lists:sort(fun({_, {c, Temp1}}, {_, {c, Temp2}}) ->
    Temp1 < Temp2 end, ConvertedList).
