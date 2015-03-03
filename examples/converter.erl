-module(converter).
-export([convert/2, convert_length/1]).

convert(M, inch) ->
  M / 2.54;
convert(M, centimeter) ->
  M * 2.54.

convert_length({centimeter, X}) ->
  {inch, X / 2.54};
convert_length({inch, X}) ->
  {centimeter, X * 2.54}.
