-module(resource_discovery).

-behaviour(gen_server).

-export([start_link/0, add_local_resource/2, add_target_resource_type/1,
         fetch_resources/1, trade_resources/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%               "I have" touples, " I want" list,      "found resource" touples
-record(state, {local_resources, target_resource_types, resources}).

% locally register this module as ?SERVER
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% ?MODULE:start_link() ->
%   gen_server:start_link() ->
%     ?MODULE:init()
init([]) ->
  % define our init server state.
  {ok, #state{resources=dict:new(),
              target_resource_types=[],
              local_resources=dict:new()}}.

add_local_resource(Type, Instance) ->
  % async cast: save a available resource = resource type + resource instance
  % broadcast to the rest of the erlang cloud.
  gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

add_target_resource_type(Type) ->
  % store a resource type in alist that identifies which sorts of resource
  % instances we are interested in caching and consuming
  gen_server:cast(?SERVER, {add_target_resource_type, Type}).

% add_local_resource() ->
%   gen_server:cast() ->
%     handle_cast()
handle_cast({add_local_resource, {Type, Instance}}, State) ->
  % load local resources list saved in State
  LocalResources = State#state.local_resources,
  % NewLocalResources = LocalResources + {Type, Instance}
  NewLocalResources = add_resource(Type, Instance, LocalResources),
  % return NewLocalResources
  {noreply, State#state{local_resources = NewLocalResources}};
% add_target_resource_type() ->
%   gen_server:cast() ->
%     handle_cast()
handle_cast({add_target_resource_type, Type}, State) ->
  TargetTypes = State#state.target_resource_types,
  NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
  {noreply, State#state{target_resource_types = NewTargetTypes}};
% trade_resources() ->
%   gen_server:cast() ->
%     handle_cast()
handle_cast(trade_resources, State) ->
  LocalResources = State#state.local_resources,
  lists:foreach(
    fun(Node) ->
      gen_server:cast({?SERVER, Node},
                      {trade_resources, {node(), LocalResources}})
    end,
    [node() | nodes()]),
  {noreply, State};
handle_cast({trade_resources, {ReplyTo, RemoteResources}}, State) ->
  #state{local_resources = LocalResources,
         target_resource_types = TargetTypes,
         resources = Resources} = State,
  FilteredRemotes = resources_for_types(TargetTypes, RemoteResources),
  NewResources = add_resources(FilteredRemotes, Resources),
  case ReplyTo of
    noreply ->
      ok;
    _ ->
      gen_server:cast({?SERVER, ReplyTo},
                      {trade_resources, {noreply, LocalResources}})
  end,
  {noreply, State#state{resources = NewResources}}.

add_resources([{Type, Identifier} | T], Dict) ->
  add_resources(T, add_resource(Type, Identifier, Dict));
add_resources([], Dict) ->
  Dict.

% update dict with + {Type, Resource}
add_resource(Type, Resource, Dict) ->
  case dict:find(Type, Dict) of
    {ok, ResourceList} ->
      NewList = [Resource | lists:delete(Resource, ResourceList)],
      dict:store(Type, NewList, Dict);
    error ->
      dict:store(Type, [Resource], Dict)
  end.

resources_for_types(Types, ResourceTuples) ->
  Fun = fun(Type, Acc) ->
            case dict:find(Type, ResourceTuples) of
              {ok, List} ->
                [{Type, Resource} || Resource <- List] ++ Acc;
              error ->
                Acc
            end
        end,
  lists:foldl(Fun, [], Types).

% it returns a list of all resource instances we have cached for a
% particular resource type.
fetch_resources(Type) ->
  gen_server:call(?SERVER, {fetch_resources, Type}).

% fetch_resources() ->
%   gen_server:call() ->
%     handle_call()
handle_call({fetch_resources, Type}, _From, State) ->
  {reply, dict:find(Type, State#state.resources), State}.

trade_resources() ->
  gen_server:cast(?SERVER, trade_resources).
handle_info(ok = _Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
