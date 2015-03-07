-module(ping_pong_linked).
-export([start/1, ping/2, pong/0]).

% "Ping" calls exit(ping) when it finishes and this will cause an exit signal to be sent to
% "pong" which will also terminate.
%
%
% Example:
%
% erlang@9a56af8b9001:/var/www$ erl -sname s1
% Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]
%
% Eshell V5.9.1  (abort with ^G)
% (s1@9a56af8b9001)1> c(ping_pong_linked).
% {ok,ping_pong_linked}
% (s1@9a56af8b9001)2> ping_pong_linked:start(s1@9a56af8b9001).
% [pong] ping received
% <0.46.0>
% [ping] pong received
% [pong] ping received
% [ping] pong received
% [pong] ping received
% [ping] pong received

ping(N, Pong_Pid) ->
  link(Pong_Pid),
  ping1(N, Pong_Pid).

ping1(0, _) ->
  % the exit of ping, triggers the exit of pong
  exit(ping);
ping1(N, Pong_Pid) ->
  Pong_Pid ! {ping, self()},
  receive
    pong ->
      io:format("[ping] pong received~n", [])
  end,
  ping1(N - 1, Pong_Pid).

pong() ->
  receive
    {ping, Ping_Pid}->
      io:format("[pong] ping received~n", []),
      Ping_Pid ! pong
  end,
  pong().

start(Ping_Node) ->
  Pong_Pid = spawn(ping_pong_linked, pong, []),
  spawn(Ping_Node, ping_pong_linked, ping, [3, Pong_Pid]).

