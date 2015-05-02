-module(simple_cache).

-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_event:replace(Key, Value),
      sc_element:replace(Pid, Value);
    {error, _Reason} ->
      sc_event:create(Key, Value),
      {ok, Pid} = sc_element:create(Value),
      sc_store:insert(Key, Pid)
  end.

lookup(Key) ->
  sc_event:lookup(Key),
  try
    % if the key indeed maps to a pid then the sc_element
    {ok, Pid} = sc_store:lookup(Key),
    % process is queried with that pid
    {ok, Value} = sc_element:fetch(Pid),
    % and the value returned.
    {ok, Value}
  catch
    % in other case, a simple error is returned.
    % it can be the case of "no mapping" but could also be
    % the case of a sc_element is killed or dies before the
    % query is executed.
    _Class:_Exception ->
      {error, not_found}
  end.

delete(Key) ->
  sc_event:delete(Key),
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      % delete operation delegated to sc_element:delete/1.
      % the reason for that is because in the case of an
      % accidental death of sc_element process, the use of
      % terminate in our sc_element gen_server implentation
      % will take care of cleanup then too.
      sc_element:delete(Pid);
    {error, _Reason} ->
      ok
  end.
