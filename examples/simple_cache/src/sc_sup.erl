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

-export([start_link/0, start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  % start supervisor container registering it under the name in ?SERVER
  % and indicating that the callback module (the implementation)
  % associated with it is ?MODULE which of course is sc_sup itself.
  % Note: it invokes sc_sup:init() callback function.
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Key, Value) ->
  % the call results in the arguments Key and Value being sento to the
  % start_link function of sc_element.
  supervisor:start_child(?SERVER, [Key, Value]).

init([]) ->
  % our children are temporary, meaning that if they die, they are dead.
  % The supervisor will not restart them.
  % This supervisor is in many way just a factory for sc_element servers.
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 0,
  MaxSecondsBetweenRestart = 1,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestart},

  % if a child dies, it's not to be restarted under any circumstance.
  Restart = temporary,
  % supervisor shuts down it does not wait for it's children to shutdown
  % cleanly but instead just kills them forcefully.
  Shutdown = brutal_kill,
  % all children of supervisor are worker processes.
  Type = worker,

  % single child specification for the sc_element process.
  % supervisor container will not start it automatically but will instead
  % wait for an explicit request to start the child.
  % first tuple indicates that we will start the child of this supervisor by
  % calling sc_element:start_link appending [Key, Value] onto the default
  % argument list supplied in the child spec.
  AChild = {sc_element, {sc_element, start_link, []},
            Restart, Shutdown, Type, [sc_element]},

  {ok, {SupFlags, [AChild]}}.
