%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

% Application name
{application, tcp_rpc,
 % description
 [{description, "RPC server for Erlang and OTP in action"},
  % version
  {vsn, "0.1.0"},
  % list of modules in your application
  {modules, [tr_app,
             tr_sup,
             tr_server]},
  % registered name for the process root supervisor
  {registered, [tr_sup, tr_server]},
  % dependencies (order doesn't matter)
  {applications, [kernel, stdlib]},
  % you tell to the OTP system how to start your application
  % (module name and optional arguments)
  % (note: better if you use a config file instead for the arguments)
  {mod, {tr_app, []}}
 ]}.
