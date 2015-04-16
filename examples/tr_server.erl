%%% @doc This module defines a server process that listens for incoming
%%%      TCP connections and allows the user to execute commands via
%%%      that TCP stream
%%% @end
%%%
%%% Try it:
%%%
%%% sh1> erl -sname erly -setcookie 'hello'
%%% erly> c(tr_server).
%%% erly> tr_server:start_link().
%%%
%%% sh2> telnet localhost 1055
%%% Trying 127.0.0.1...
%%% Connected to localhost.
%%% Escape character is '^]'.
%%% init:stop().
%%% ok
%%% Connection closed by foreign host.
%%%

-module(tr_server).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, get_count/0, stop/0]).
%% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).

%%% API

%% @doc
%% Start the server
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%%   Pid = pid()
%% @end
start_link(Port) ->
  % init server: the function block until the gen_server process
  % is initialized by init/1 behaviour interface (callback) function.
  % note: the tuple {local, ?SERVER} locally registers this process as
  % the name of the macro ?SERVER.
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @spec start_link() -> {ok, Pid}
%% @equiv start_link(Port::integer())
start_link() ->
  start_link(?DEFAULT_PORT).

%% @doc fetch the number of requests made to this server.
%% @spec get_count() -> {ok, Count}
%% where
%%   Count = integer()
%% @end
get_count() ->
  % Send a synchronous message to a gen_server process.
  get_server:call(?SERVER, get_count).

%% @doc stop the server.
%% @spec stop() -> ok
%% @end
stop() ->
  % Send a asynchronous message to a gen_server process.
  gen_server:cast(?SERVER, stop).

%% Generic server callback definitions

% Callback for gen_server:start_list/4.
% start_link prepare us to be hooked into the powerful fault tolerant
% process supervisor structures of OTP.
% Initialize all the functionality.
init([Port]) ->
  % create the tcp socker listener.
  % active -> true : tells gen_tcp to send all incoming tcp data to our
  % process as messages in our process mailbox (handled by handle_info/2).
  {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
  % note: 0 is a timeout value. Says to our generic server that as soon as
  % we are finished with init, we should generate a timeout message which will
  % force us to handle that timeout msg as soon as init returns.
  {ok, #state{port = Port, lsock = LSock}, 0}.

% Callback for gen_server:call/2.
% It's invoked every time a msg is received in a gen_server process mailbox
% that was sent from the gen_server:call function.
% arguments:
%   - the message sent directly via gen_server:call
%   - From variable
%   - state of the server
% In our case, we interrogate the state to retrieve the request count and
% we return it.
% The response received by the caller of gen_server:call is
% {ok, State#state.request_count}.
% Note: we return also the state, unchanged.
% Note: gen_server:call has an automatic timeout associated with it.
%
handle_call(get_count, _From, State) ->
  {reply, {ok, State#state.request_count}, State}.

% Callback for gen_server:cast/2.
% args:
%   - the msg sent via the cast
%   - the server state
handle_cast(stop, State) ->
  {stop, ok, State}.

% Handle all the msg not coming from API cast or call.
% It's also handle TCP streams.
handle_info({tcp, Socket, RawData}, State) ->
  RequestCount = State#state.request_count,
  try
    % Clean the data: it removes newline carriage return pair.
    MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    % Split out the module, function and argument portion of the text
    % that comes in over the socket.
    {match, [M, F, A]} = re:run(MFA, "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
                                [{capture, [1,2,3], list}, ungreedy]),
    % Execute the function we wish to call.
    Result = apply(list_to_atom(M), list_to_atom(F), args_to_terms(A)),
    % The result of the execution is returned back through the TCP connection.
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
  catch
    % Handle any exception
    _C:E ->
      gen_tcp:send(Socket, io_lib:fwrite("~p~n", [E]))
  end,
  % Update request count.
  {noreply, State#state{request_count = RequestCount + 1}};
% Handled if a timeout is triggered between the invocation of callback
% functions.
% E.g. init/1 contains a timeout = 0 that it'll trigger a timeout msg
% after 0 milliseconds.
handle_info(timeout, #state{lsock = LSock} = State) ->
  % Start accepting TCP connections.
  {ok, _Sock} = gen_tcp:accept(LSock),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%% Internal functions

args_to_terms([]) ->
  [];
args_to_terms(RawArgs) ->
  {ok, Tokens, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
  {ok, Args} = erl_parse:parse_term(Tokens),
  Args.
