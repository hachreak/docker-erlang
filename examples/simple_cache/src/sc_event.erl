% the API module will serve to encapsulate the protocols and start
% our event manager.
-module(sc_event).

-export([start_link/0, lookup/1, create/2,
         replace/2, delete/1, add_handler/2]).

-define(SERVER, ?MODULE).

start_link() ->
  % the function returns all that's required to run under the supervisor.
  gen_event:start_link({local, ?SERVER}).

% easily add event handlers to the eventing system.
add_handler(Handler, Args) ->
  % we register the event manager as ?SERVER
  gen_event:add_handler(?SERVER, Handler, Args).

% eventing API
%
% e.g. in our code we can use sc_event:lookup(Key) to notify the
%      event "lookup" instead of use gen_event:notify(sc_event, {lookup, Key}).

create(Key, Value) ->
  gen_event:notify(?SERVER, {create, {Key, Value}}).

lookup(Key) ->
  gen_event:notify(?SERVER, {lookup, Key}).

delete(Key) ->
  gen_event:notify(?SERVER, {delete, Key}).

replace(Key, Value) ->
  gen_event:notify(?SERVER, {replace, {Key, Value}}).
