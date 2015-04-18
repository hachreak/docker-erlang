-module(tr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  % start up the supervisor
  % arguments:
  %   - tuple that contains the name of registered process and
  %     where its registered.
  %     In this example, we are registering the application
  %     as a local process under the name tr_sup.
  %   - the name of the module that implements the supervisor (tr_sup)
  %   - list of arguments that get passed to the init function on start up
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% We tell to the supervisor what to start and how to start it
% (what process to supervise and how).
init([]) ->
  % description of the process you want the Supervisor to supervise
  %  - id used internally to name and reference the process supervised
  %  - name of module/function and arguments used to start the server
  %  - type of process under supervision (we chosen permanent because
  %    we expect that it'll be a long lived process) (the other options
  %    are transient and temporary).
  %  - type of shutdown when the process is killed. The number indicates
  %    that this is a soft shutdown and the process will have that number
  %    of milliseconds to actually stop. In this example, after a kill
  %    request, for whatever reason, the tr_server process will have
  %    200 ms to shutdown.
  %  - it indicates whether the process is a supervisor or a worker.
  %    For this process we are just going to use worker.
  %    Worker process are defined as any process in a supervision tree
  %    that is not implement the supervisor behaviour.
  %  - the list of modules that this module uses. It's only used during
  %    hot code upgrades to the system. It indicates to the system in
  %    what order to upgrade modules.
  Server = {tr_server, {tr_server, start_link, []},
            permanent, 2000, worker, [tr_server]},
  %  - atom 'ok'
  %  - tuple 2 values:
  %    + tuple 3 values:
  %      * restart strategy. We chhose a one_for_one strategy that means
  %        that if a process dies only that process is restarted.
  %      * maximum number of restarts allowed. If we exceeded that restart
  %        count the supervisor itself would terminate and propagate the
  %        failure up the supervision chain.
  %      * timeframe for those restarts.
  %    + list of child descriptions.
  {ok, {{one_for_one, 0, 1}, [Server]}}.
