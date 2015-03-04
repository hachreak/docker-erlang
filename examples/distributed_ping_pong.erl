-module(distributed_ping_pong).
-export([start_ping/1, start_pong/0, ping/2, pong/0]).

% 1) Start containers:
%
% shell1> docker run -i -t -v `pwd`/examples:/var/www:rw --rm myerlang /bin/bash
% shell2> docker exec -it CONTAINER_ID bash
%
% `docker ps` to know the container id.
%
% 2) Run environment
%
% shell1> erl -sname ping -setcookie 'mysecret'
% shell2> erl -sname pong -setcookie 'mysecret'
%
% 3) Run script in shell2
%
% (pong@3ce2c05f0ff7)1> c(distributed_ping_pong).
% {ok,distributed_ping_pong}
% (pong@3ce2c05f0ff7)2> distributed_ping_pong:start_pong().
% true
%
% 4) Run script in shell1
%
% (ping@3ce2c05f0ff7)1> c(distributed_ping_pong).
% {ok,distributed_ping_pong}
% (ping@3ce2c05f0ff7)2> distributed_ping_pong:start_ping(pong@3ce2c05f0ff7).
%
% 5) Have fun! :)

ping(0, Pong_Node) ->
  % end
  {pong, Pong_Node} ! finished,
  io:format("[ping] end~n", []);
ping(N, Pong_Node) ->
  % send msg
  {pong, Pong_Node} ! {ping, self()},
  % recv msg
  receive
    pong ->
      io:format("[ping] received pong~n", [])
  end,
  % loop
  ping(N - 1, Pong_Node).

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

start_pong() ->
  register(pong, spawn(distributed_ping_pong, pong, [])).

start_ping(Pong_Node) ->
  spawn(distributed_ping_pong, ping, [3, Pong_Node]).
