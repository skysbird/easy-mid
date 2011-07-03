%%%-------------------------------------------------------------------
%%% @author skysbird <skysbird@skysbird-PC>
%%% @copyright (C) 2011, skysbird
%%% @doc
%%%
%%% @end
%%% Created : 21 May 2011 by skysbird <skysbird@skysbird-PC>
%%%-------------------------------------------------------------------
-module(ss_server_ssl).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

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
start_link(Port,Module) ->
    gen_server:start_link( ?MODULE, [Port,Module], []).

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
init([Port,Module]) ->
    start_ssl(Port,Module),
    {ok, #state{}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
start_ssl(Port,Module) when is_integer(Port),is_atom(Module) ->
%    process_flag(trap_exit,true),
    io:format("start ssl listener server\n"),
    ssl:start(),
    ssl:seed("seed-every-time"),
    {ok, ListenSocket} = ssl:listen(Port, [
        {ssl_imp, new},
        {active, false},
	{reuseaddr,true},
        {backlog,1000000},
%        {verify, 0},
	{packet, line},
        {certfile, "../cert/cacert.pem"},
        {keyfile, "../cert/privkey.pem"}
    ]),
    io:format("ready to accept connections at port ~p ~p\n", [Port,ListenSocket]),
    spawn_link(fun()-> par_connect(ListenSocket,Module,1)end).


    
par_connect(ListenSocket,Module,N)->
    {ok, Socket} = ssl:transport_accept(ListenSocket), 
    spawn(fun()->par_connect(ListenSocket,Module,N+1)end),
    %io:format("~p\n",[Info]),
    io:format("accepted connection from ~p--------~p\n", [ssl:peername(Socket),N]),
    server_loop(Socket,Module).

   
server_loop(Socket,Module) ->
    ssl:ssl_accept(Socket),
    {ok, Pid} = ss_client_sv:start_client(),
    ssl:controlling_process(Socket, Pid),
    Module:set_socket(Pid, Socket).
