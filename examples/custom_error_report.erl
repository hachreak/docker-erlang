-module(custom_error_report).
-behaviour(gen_event).
-export([register_with_logger/0]).
-export([init/1, handle_event/2, handle_call/2,
        handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-define(SERVER, ?MODULE).

% Example of logger (it uses gen_event behaviour).
% note: the interface of gen_event is very similar to gen_server.

register_with_logger() ->
  error_logger:add_report_handler(?MODULE).

init([]) ->
  {ok, #state{}}.

% where we get our events.
handle_event({error, _Gleader, {Pid, Format, Data}}, State) ->
  io_lib:fwrite("ERROR <~p> ~s", [Pid, io_lib:format(Format, Data)]),
  {ok, State};
handle_event({error_report, _Gleader, {Pid, std_error, Report}}, State) ->
  io_lib:fwrite("ERROR <~p> ~p", [Pid, Report]),
  {ok, State};
handle_event({error_report, _Gleader, {Pid, Type, Report}}, State) ->
  io_lib:fwrite("ERROR <~p> ~p ~p", [Pid, Type, Report]),
  {ok, State};
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) ->
  io_lib:fwrite("WARNING <~p> ~s", [Pid, io_lib:format(Format, Data)]),
  {ok, State};
handle_event({warning_report, _Gleader, {Pid, std_warning, Report}}, State) ->
  io_lib:fwrite("WARNING <~p> ~p", [Pid, Report]),
  {ok, State};
handle_event({warning_report, _Gleader, {Pid, Type, Report}}, State) ->
  io_lib:fwrite("WARNING <~p> ~p ~p", [Pid, Type, Report]),
  {ok, State};
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) ->
  io_lib:fwrite("INFO <~p> ~s", [Pid, io_lib:format(Format, Data)]),
  {ok, State};
handle_event({info_report, _Gleader, {Pid, std_info, Report}}, State) ->
  io_lib:fwrite("WARNING <~p> ~p", [Pid, Report]),
  {ok, State};
handle_event({info_report, _Gleader, {Pid, Type, Report}}, State) ->
  io_lib:fwrite("ERROR <~p> ~p ~p", [Pid, Type, Report]),
  {ok, State};
handle_event(_Event, State) ->
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

