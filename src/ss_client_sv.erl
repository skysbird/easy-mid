%%%-------------------------------------------------------------------
%%% @author skysbird <skysbird@skysbird-PC>
%%% @copyright (C) 2011, skysbird
%%% @doc
%%%
%%% @end
%%% Created : 19 May 2011 by skysbird <skysbird@skysbird-PC>
%%%-------------------------------------------------------------------
-module(ss_client_sv).

-behaviour(supervisor).

%% API
-export([start_link/1,start_client/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%%%===================================================================
%%% API functions
%%%===================================================================

start_client()->
    io:format("start ssl fsm client process...\n"),
    supervisor:start_child(ss_client_sv, []).
    %%ss_server_fsm:start_link().
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Module) ->
    io:format("starting client supervisor...\n"),
    supervisor:start_link({local, ss_client_sv}, ?MODULE, [Module]).

%%    Id = make_ref(), 

%%    ChildSpec = {Id, {gen_server, start_link, [server, [], []]}, permanent, 4000, worker, [server]},   
%%    supervisor:start_child(ss_client_sup, ChildSpec).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Module]) ->
   {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.
%%%===================================================================
%%% Internal functions
%%%===================================================================
