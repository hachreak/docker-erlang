-module(tr_app).

-behaviour(application).

-export([start/2, stop/1]).

% Called when the application start up and it must
% return the pid of the root supervisor
start(_Type, _StartArgs) ->
  case tr_sup:start_link() of
    {ok, Pid} -> {ok, Pid};
    Other -> {error, Other}
  end.

stop(_State) ->
  ok.
