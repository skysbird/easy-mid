%%%-------------------------------------------------------------------
%%% @author skysbird <skysbird@skysbird-PC>
%%% @copyright (C) 2011, skysbird
%%% @doc
%%%
%%% @end
%%% Created : 19 May 2011 by skysbird <skysbird@skysbird-PC>
%%%-------------------------------------------------------------------
-module(ss_server_sv).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_link_test/1]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link([Port,Module]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port,Module]).

start_link_test([Port,Module]) ->
    {ok,Pid}=supervisor:start_link({local, ?SERVER}, ?MODULE, [Port,Module]),
    unlink(Pid).
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
init([Port,Module]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = ?MAX_RESTART,
    MaxSecondsBetweenRestarts = ?MAX_TIME,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
        
    %%TCP Listener
    Tl = {ss_server_ssl_sup, {ss_server_ssl, start_link, [Port,Module]},
    	      permanent, 2000, worker, [ss_server_ssl]},
    %% Client supervisor
    Cs = {ss_client_sup, {ss_client_sv, start_link, [Module]},
    permanent, infinity, supervisor, []},
    {ok, {SupFlags, [Tl,Cs]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
