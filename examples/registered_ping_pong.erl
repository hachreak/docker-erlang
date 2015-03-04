-module(registered_ping_pong).
-export([start/0, ping/1, pong/0]).

ping(0) ->
  % end
  pong ! finished,
  io:format("[ping] end~n", []);
ping(N) ->
  % send msg
  pong ! {ping, self()},
  % recv msg
  receive
    pong ->
      io:format("[ping] received pong~n", [])
  end,
  % loop
  ping(N - 1).

pong() ->
  % recv msg
  receive
    finished ->
      io:format("[pong] finished~n", []);
    {ping, Ping_PID} ->
      io:format("[pong] received ping~n", []),
      % send msg
      Ping_PID ! pong,
      % loop
      pong()
  end.

start() ->
  register(pong, spawn(registered_ping_pong, pong, [])),
  spawn(registered_ping_pong, ping, [3]).
