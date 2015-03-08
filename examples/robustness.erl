-module(robustness).
-export([start_server/0, server/0, client/2, infinite_send/3]).

% Try robustness of erlang:
%
% (1) Server
% erlang@f2df835e32d7:/var/www$ erl -sname server -setcookie 'hello'
% Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]
%
% Eshell V5.9.1  (abort with ^G)
% (server@f2df835e32d7)1> c(robustness).
% {ok,robustness}
% (server@f2df835e32d7)2> robustness:start_server().
% [server] start
% true
% [server] msg from <9229.45.0>: "hello"
% [server] wait for messages..
% [server] msg from <9229.45.0>: "hello"
% [server] wait for messages..
% [server] msg from <9229.45.0>: "hello"
% [server] wait for messages..
% [server] unexpected on client <9229.38.0>.. recover the error!
% [server] wait for messages..
%
% (2) Client
% erlang@de802115749b:/var/www$ erl -sname client -setcookie 'hello'
% Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]
%
% Eshell V5.9.1  (abort with ^G)
% (client@de802115749b)1> c(robustness).
% {ok,robustness}
% (client@de802115749b)2> robustness:client(server@f2df835e32d7,"hello").
% [client] <0.45.0> exit, got normal
% {<0.38.0>,stop}

start_server() ->
  register(server, spawn(robustness, server, [])).

server() ->
  io:format("[server] wait for messages..~n", []),
  receive
    {From, stop} ->
      % the client unexpected end, recover the situation
      io:format("[server] unexpected on client ~p.. recover the error!~n", [From]);
    {From, Msg} ->
      % new mesg from client received
      io:format("[server] msg from ~p: ~p~n", [From, Msg])
  end,
  server().

wait_events(ServerNode) ->
  receive
    % "unexpected" exit discovered..
    {'EXIT', From, Reason} ->
      io:format("[client] ~p exit, got ~p~n", [From, Reason]),
      % alert the server!
      {server, ServerNode} ! {self(), stop}
  end.

% simulate a "unexpected" error.. (ending the process)
infinite_send(_, _, 0) ->
  ok;
infinite_send(ServerNode, Msg, N) ->
  {server, ServerNode} ! {self(), Msg},
  infinite_send(ServerNode, Msg, N - 1).

client(ServerNode, Msg) ->
  % I want to be warned if a process fail
  process_flag(trap_exit, true),
  % Start my process
  register(myclient, spawn_link(robustness, infinite_send, [ServerNode, Msg, 3])),
  % Wait for "unexpected" problems..
  wait_events(ServerNode).
