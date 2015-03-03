-module(converter).
-export([convert/2, convert_length/1, convert_length2/1]).

convert(M, inch) ->
  M / 2.54;
convert(M, centimeter) ->
  M * 2.54.

convert_length({centimeter, X}) ->
  {inch, X / 2.54};
convert_length({inch, X}) ->
  {centimeter, X * 2.54}.

convert_length2(Length) ->
  case Length of
    {centimeter, X} ->
      {inch, X / 2.54};
    {inch, Y} ->
      {centimeter, Y * 2.54}
  end.
