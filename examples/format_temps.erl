-module(format_temps).
-export([format_temps/1]).

%% Only this function is exported
format_temps([]) ->
  %% No output for empty list
  ok;
format_temps([City | Rest]) ->
  print_temp(convert_to_celsius(City)),
  format_temps(Rest).

% No convertion needed
convert_to_celsius({Name, {c, Temp}}) ->
  {Name, {c, Temp}};
% Do the conversion
convert_to_celsius({Name, {f, Temp}}) ->
  {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
  io:format("~15w ~w c~n", [Name, Temp]).
