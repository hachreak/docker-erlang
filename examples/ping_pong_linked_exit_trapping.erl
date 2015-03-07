-module(ping_pong_linked_exit_trapping).
-export([start/1, ping/2, pong/0]).

% "Ping" calls exit(ping) when it finishes and this will cause an exit signal to be sent to
% "pong" which will also terminate.
% Pong know when Ping exit and print the reasons.
%
%
% Example:
%
% erlang@9a56af8b9001:/var/www$ erl -sname s1
% Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]
%
% Eshell V5.9.1  (abort with ^G)
% (s1@9a56af8b9001)1> c(ping_pong_linked_exit_trapping).
% {ok,ping_pong_linked_exit_trapping}
% (s1@9a56af8b9001)2> ping_pong_linked_exit_trapping:start(s1@9a56af8b9001).
% [pong] ping received
% <0.46.0>
% [ping] pong received
% [pong] ping received
% [ping] pong received
% [pong] ping received
% [ping] pong received
% pong exiting, got {'EXIT',<0.46.0>,ping}

ping(N, Pong_Pid) ->
  link(Pong_Pid),
  ping1(N, Pong_Pid).

ping1(0, _) ->
  % the exit of ping, triggers a msg to pong with the reasons
  exit(ping);
ping1(N, Pong_Pid) ->
  Pong_Pid ! {ping, self()},
  receive
    pong ->
      io:format("[ping] pong received~n", [])
  end,
  ping1(N - 1, Pong_Pid).

pong() ->
  process_flag(trap_exit, true),
  pong1().

pong1() ->
  receive
    {ping, Ping_Pid}->
      io:format("[pong] ping received~n", []),
      Ping_Pid ! pong,
      pong1();
    {'EXIT', From, Reason} ->
      io:format("pong exiting, got ~p~n", [{'EXIT', From, Reason}])
  end.

start(Ping_Node) ->
  Pong_Pid = spawn(ping_pong_linked_exit_trapping, pong, []),
  spawn(Ping_Node, ping_pong_linked_exit_trapping, ping, [3, Pong_Pid]).

