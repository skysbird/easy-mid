%%%-------------------------------------------------------------------
%%% @author skysbird <skysbird@skysbird-PC>
%%% @copyright (C) 2011, skysbird
%%% @doc
%%%
%%% @end
%%% Created : 19 May 2011 by skysbird <skysbird@skysbird-PC>
%%%-------------------------------------------------------------------
-module(ss_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEF_PORT,    2222).
%%parameters process function
get_app_env(Opt,Default)->
    case application:get_env(application:get_application(),Opt) of
	{ok,Val}->
	    Val;
	_ ->
	    case init:get_argument(Opt) of
		[[Val|_]]->
		    Val;
		error ->
		    Default
	    end
    end.



%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    ListenPort = get_app_env(listen_port,?DEF_PORT),
    case ss_server_sv:start_link([ListenPort,ss_server_ssl_fsm]) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    exit(normal),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
