-module(sc_event_logger).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {}).

add_handler() ->
  sc_event:add_handler(?MODULE, []).

delete_handler() ->
  sc_event:delete_handler(?MODULE, []).

% Implementation of gen_event API

init([]) ->
  {ok, #state{}}.

% note: we can recognize events from the API in sc_event.
% in our case, the logger print msg on error_logger.
handle_event({create, {Key, Value}}, State) ->
  error_logger:info_msg("create(~w, ~w)", [Key, Value]),
  {ok, State};
handle_event({replace, {Key, Value}}, State) ->
  error_logger:info_msg("replace(~w, ~w)", [Key, Value]),
  {ok, State};
handle_event({lookup, Key}, State) ->
  error_logger: info_msg("lookup(~w)", [Key]),
  {ok, State};
handle_event({delete, Key}, State) ->
  error_logger: info_msg("delete(~w)", [Key]),
  {ok, State}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
