%%%
%%% Contains the supervisor behaviour implementation.
%%%

% simple_one_for_one: can only start one type of child but it can
% do it dynamically at runtime.
%
% The supervisor can start sc_element processes at runtime when required
% to do so.
%
% supervisor
%  |  |   |__ child
%  |  |
%  |  |___ child
%  |
%  |___ child
%
-module(sc_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  % start supervisor container registering it under the name in ?SERVER
  % and indicating that the callback module (the implementation)
  % associated with it is ?MODULE which of course is sc_sup itself.
  % Note: it invokes sc_sup:init() callback function.
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  ElementSup = {sc_element_sup, {sc_element_sup, start_link, []},
                permanent, 2000, supervisor, [sc_element]},

  EventManager = {sc_event, {sc_event, start_link, []},
                  permanent, 2000, worker, [sc_event]},

  Children = [ElementSup, EventManager],
  RestartStrategy = {one_for_one, 4, 3600},
  {ok, {RestartStrategy, Children}}.
