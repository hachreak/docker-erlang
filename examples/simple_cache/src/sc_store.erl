%%%
%%% Contains the code used to implement the storage system for our
%%% key to pid mappings.
%%% These mappings allow us to take a key and map it to the
%%% sc_element process that is holding the value associated with it.
%%%
%%% To implement this store we are going to use a very useful Erlang
%%% storage system called ETC (Erlang Term/Tuple Storage: a fast,
%%% in-memory storage space for Erlang data).
%%% ETS is as fast as it is because it's implemented as a set of built
%%% in functions, meaning it's a part of the ERTS (Erlang Runtime System)
%%% and implemented directly in the VM in C code.
%%% It's a nice choice for data that:
%%%  1. needs to be persistent as long as the VM is alive.
%%%  2. single VM, no replicated data.
%%%  3. data which needs to be fetched in low millisecond time ranges.
%%%  4. needs to be accessed by a number of different processes.
%%%  5. flat data without foreign key relationships.
%%%
%%% sc_store module serves as an abstraction for whatevere storage
%%% mechanism we as using to store the key to ID mapping data.
%%% We decoupling the application from the choice of storage system
%%% allowing us to change gears later on and use something else.
%%%

-module(sc_store).

% CRUD api
-export([
         init/0,
         insert/2, % Create and Update
         delete/1, % Delete
         lookup/1  % Read
        ]).

-define(TABLE_ID, ?MODULE).

init() ->
  % create the ETS table that will store key to pid mappings.
  % we register a name for the table (TABLE_ID) and use it to
  % access without use a table handler.
  % "public" allow our table to be accessed by any process, not
  % just the one that created it, which is important if all sc_element
  % processes among others are going to use the sc_store.
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

insert(Key, Pid) ->
  % ets accept only tuple!
  % note: the first element in a ETS record tuple is by
  % default the key, the resto the body (the key is unique).
  ets:insert(?TABLE_ID, {Key, Pid}).

% note: lookup on a key is ETS is a constant time operation
% (very fast).
lookup(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] -> {ok, Pid};
    []           -> {error, not_found}
  end.

delete(Pid) ->
  % note: '_' match anything symbol (as an atom)
  % this match (and delete it) anything that contains in
  % second position the process id Pid.
  ets:match_delete(?TABLE_ID, {'_', Pid}).
