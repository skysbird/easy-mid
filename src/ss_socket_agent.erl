%%%-------------------------------------------------------------------
%%% @author skysbird <skysbird@skysbird-PC>
%%% @copyright (C) 2011, skysbird
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2011 by skysbird <skysbird@skysbird-PC>
%%%-------------------------------------------------------------------
-module(ss_socket_agent).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([forward/3,kick/4,close/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state,{}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

forward(Node,SocketPid,Message)->
    %io:format("got rpc request ~p ~p ~p",[Node,SocketPid,Message]),
    case ets:lookup(socket_list,SocketPid) of
        [{SocketPid,Socket}]->
            io:format("find socket in ets ~p ~p ~p ",[Socket,Node,node()]),
            Node1 = list_to_atom(Node),
            if Node1 == node()->
                    log4erl:log(info,"using local socket to send ~p ~ts",[Socket,Message]),
                    log4erl:log(info,"using local socket to send ~p",[Socket]),
                    spawn(fun()->
                          ssl:send(Socket,Message)
                      end);
                true->
                    gen_server:call({?MODULE,Node},{forward,Socket,Message})
            end;
        _->
           log4erl:log(info,"not find SocketPid ~p",[SocketPid])
    end,
    {noreplay,#state{}}.


close(Node,SocketPid)->
    %io:format("got rpc request ~p ~p ~p",[Node,SocketPid,Message]),
    case ets:lookup(socket_list,SocketPid) of
        [{SocketPid,Socket}]->
            io:format("find socket in ets to close~p ~p ~p ",[Socket,Node,node()]),
            Node1 = list_to_atom(Node),
            if Node1 == node()->
                    log4erl:log(info,"using local socket to close ~p",[Socket]),
                    spawn(fun()->
                          ssl:close(Socket)
                      end);
                true->
                    gen_server:call({?MODULE,Node},{close,Socket})
            end;
        _->
           log4erl:log(info,"not find SocketPid ~p",[SocketPid])
    end,
    {noreplay,#state{}}.


kick(Node,SocketFSM,Socket,Message)->
    gen_server:call({?MODULE,Node},{kick,SocketFSM,Socket,Message}).

init([]) ->
    %register(ss_socket_agent_process,spawn_link(fun()->loop()end)),
    {ok, #state{}}.


handle_call({forward,Socket,Msg}, _From, State) ->
    spawn(fun()->
		  ssl:send(Socket,Msg)
	  end),
    {noreply, State};

handle_call({close,Socket}, _From, State) ->
    spawn(fun()->
		  ssl:close(Socket)
	  end),
    {noreply, State};

handle_call({kick,SocketFSM,Socket,Msg}, _From, State) ->
    spawn(fun()->
		  ss_server_xml:send_message(Socket,Msg),
		  SocketFSM!{kick,Socket}
	  end),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
