%%% The server listen link_ex if it crash and restart it
%%% automatically.
%%%
%%% e.g.
%%%
%%% erlang@723ddd152a38:/var/www/examples$ erl
%%% Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]
%%%
%%% Eshell V5.9.1  (abort with ^G)
%%% 1> c(link_ex2).
%%% {ok,link_ex2}
%%% 2> li
%%% link_ex2    lists
%%% 2> link_ex2:start_link().
%%% {ok,<0.39.0>}
%%% 3> link_ex2:ping().
%%% Got it!
%%% ok
%%% 4> link_ex2:ping_error().
%%% Pid <0.40.0> died with Reason { ... }
%%% 5> link_ex2:ping().
%%% Got it!
%%% ok
%%% 6>
%%%
%%% After link_ex crash when received "ping_error" msg,
%%% link_ex2 restart link_ex, to be able to continous to reply
%%% to ping msg.

-module(link_ex2).

-behaviour(gen_server).

-export([start_link/0, ping/0, ping_error/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

ping_error() ->
  % generate a error in the server
  gen_server:cast(?SERVER, ping_error).

ping() ->
  gen_server:cast(?SERVER, ping).

init([]) ->
  process_flag(trap_exit, true),
  link_ex:start_link(),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(ping, State) ->
  link_ex:ping(),
  {noreply, State};
handle_cast(ping_error, State) ->
  link_ex ! a_message_i_dont_understand,
  {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
  io:format("Pid ~p died with Reason ~p~n", [Pid, Reason]),
  link_ex:start_link(),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
