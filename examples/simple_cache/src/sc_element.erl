%%%
%%% Contains the code for the process that stores
%%% cached values.
%%% Element storage space and store an element.
%%%

-module(sc_element).

-behaviour(gen_server).

% Our API
-export([start_link/2, create/2, create/1, fetch/1, replace/2, delete/1]).
% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
% longest time that a key value pair can live in the
% cache without being evicted.
-define(DEFAULT_LEASE_TIME, 60 * 60 * 24). % iday default (in seconds)

-record(state, {value, lease_time, start_time}).

create(Value, LeaseTime) ->
  % delegate the process to startup to the
  % sc_sup:start_child/2
  sc_element_sup:start_child(Value, LeaseTime).

create(Value) ->
  create(Value, ?DEFAULT_LEASE_TIME).

% called by supervisor when it needs to start a
% new child process dynamically.
%
% When a new element is to be inserted into the
% system, the storage gets created and the element
% stored:
%
% sc_element:create/2/1
%  -> sc_sup:start_child/2
%    -> sc_element:start_link/2
%      -> sc_element:init/1
start_link(Value, LeaseTime) ->
  gen_server:start_link(?MODULE, [Value, LeaseTime], []).

% it will be called by gen_server and return a
% fully initialized state record to the gen_server
% container.
init([Value, LeaseTime]) ->
  % we capture the time at which this sc_element
  % process is starting in seconds.
  StartTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  {ok,
   #state{value = Value, lease_time = LeaseTime,
          start_time = StartTime},
   % timeout position: how we manage lease times.
   % we just use the gen_server timeout.
   time_left(StartTime, LeaseTime)}.

% Amount of time a Value has left to leave.
time_left(_StartTime, infinity) ->
  % out value live forever.
  % note: atom have the convenient property of
  %       being greater than any number when you
  %       compare them with operators like > or <.
  infinity;
time_left(StartTime, LeaseTime) ->
  CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  TimeElapsed = CurrentTime - StartTime,
  case LeaseTime - TimeElapsed of
    % kill our sc_element process by sending it into
    % the timeout clause od handle_info
    Time when Time =< 0 -> 0;
    % if the time elapsed is less than the lease
    % time, we the return the time remaining.
    Time                -> Time * 1000
  end.

replace(Pid, Value) ->
  % async msg to the process identified by Pid.
  % handled by sc_element:handle_cast function.
  gen_server:cast(Pid,  {replace, Value}).

fetch(Pid) ->
  % sync msg to the process identified by Pid.
  % handled by sc_element:handle_call function.
  gen_server:call(Pid, fetch).

delete(Pid) ->
  gen_server:cast(Pid, delete).

handle_cast({replace, Value}, State) ->
  #state{lease_time = LeaseTime,
         start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
  % the "stop" atom cause gen_server to shut
  % itself down.
  % because we set the process as "temporary", the
  % supervisor doesn't try to restart it no matter
  % what it may return.
  % if we set the process as "transient", any other
  % return value here could be considered an
  % abnormal close and the supervisor would try to
  % restart it.
  %
  % the reason for the stoppage is include next
  % and it's "normal". This is a special value
  % that tells the system that the server shutdown
  % normally.
  % why use normal? the reason is that SASL will
  % automatically log shutdown of any behaviour that
  % shut down with anything else.
  %
  % note: there is one more callback that is called
  % prior to shutting down a gen_server:
  %   ?MODULE:terminate/2
  {stop, normal, State}.

handle_call(fetch, _From, State) ->
  #state{value = Value,
         lease_time = LeaseTime,
         start_time = StartTime} = State,
  % note: if we forget to set the timeout value in
  % a handle function, the timeout will revert to
  % infinity by default.
  TimeLeft = time_left(StartTime, LeaseTime),
  {reply, {ok, Value}, State, TimeLeft}.


% terminate is called as a kind of catch all
% cleanup function. in this case we are cleaning
% up all traces of the value associated with the
% process by terminating the process.
terminate(_Reason, _State) ->
  % cleaning the store.
  sc_store:delete(self()),
  ok.

handle_info(timeout, State) ->
  {stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
