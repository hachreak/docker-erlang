-module(ping_pong).
-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
  Pong_PID ! finished,
  io:format("Ping finished~n", []);
ping(N, Pong_PID) ->
  % send msg ping + "my pid" to Pong
  Pong_PID ! {ping, self()},
  % wait a msg
  receive
    % received a pong
    pong ->
      io:format("Ping received pong~n", [])
  end,
  % restart with N - 1
  ping(N - 1, Pong_PID).

pong() ->
  % Wait a msg
  receive
    % received a msg finished
    finished ->
      io:format("Pong finished~n", []);
    % received a msg ping + pid
    {ping, Ping_PID} ->
      io:format("Pong received ping~n", []),
      % send msg pong
      Ping_PID ! pong,
      % restart
      pong()
  end.

start() ->
  Pong_PID = spawn(ping_pong, pong, []),
  spawn(ping_pong, ping, [3, Pong_PID]).
