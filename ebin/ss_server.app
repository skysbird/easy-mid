{application, ss_server,
 [
  {description, "Initial ss server"},
  {vsn, "1.0"},
  {id, "ss_server"},
  {modules,      [ss_server_ssl, ss_server_ssl_fsm]},
  {registered,   [ss_server_ssl_sup, ss_client_sup]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {ss_server_app, []}},
  {env, []}
 ]
}.
