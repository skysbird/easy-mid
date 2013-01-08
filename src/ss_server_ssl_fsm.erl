%%%-------------------------------------------------------------------
%%% @author skysbird <skysbird@skysbird-PC>
%%% @copyright (C) 2011, skysbird
%%% @doc
%%%
%%% @end
%%% Created : 20 May 2011 by skysbird <skysbird@skysbird-PC>
%%%-------------------------------------------------------------------
-module(ss_server_ssl_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1,  handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_DATA'/2
]).

-define(SERVER, ?MODULE).
-record(state, {
                socket,    % client socket
                addr       % client address
               }).

-define(TIMEOUT, 120000).

set_socket(Pid, Socket) when is_pid(Pid) ->
    io:format("send event\n"),
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    io:format("start fsm...\n"),
    %gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).
    %for already started problem
    gen_fsm:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %%process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{}}.

'WAIT_FOR_SOCKET'({socket_ready, Socket}, State)  ->
    %% Now we own the socket
    %% inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    %% for test
    ssl:setopts(Socket, [{active, once}]),
    %{sslsocket,new_ssl,<0.63.0>}
    SocketPid = case Socket of
        {sslsocket,new_ssl,Pid} -> io:format("socket pid is ~p\n",[Pid]),
        pid_to_list(Pid)
    end,
    ets:insert(socket_list,{SocketPid,Socket}),
    case  ssl:peername(Socket) of
        {ok, {IP, _Port}} -> io:format("~p,~p,socket in\n",[IP,_Port]),
            {next_state, 'WAIT_FOR_DATA', State#state{socket=Socket, addr=IP}, ?TIMEOUT};
        {error,closed} -> 
            %tell process node disconnect 
            Data = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><package uid=\"\"><request type=\"logout\" id=\"\" ></request></package>">>,
            Term = {Data,node(),list_to_binary(SocketPid)},
            {p, 'ss1@192.168.0.117'} ! Term,
            ets:delete(socket_list,SocketPid),
            log4erl:log(info,"connection closed"),
            {stop, normal, State}
    end;

'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #state{socket=S} = State) ->
    io:format("data came\n"),
    io:format("route request to process module\n"),
    Term = {Data,node(),list_to_binary(get_socket_pid(S))},
    {p, 'ss1@192.168.0.117'} ! Term,
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'(timeout, #state{socket=S} = State) ->
    io:format("data timeout\n"),
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    SocketPid = get_socket_pid(S),
    %tell process node disconnect 
    Data = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><package uid=\"\"><request type=\"logout\" id=\"\" ></request></package>">>,
    Term = {Data,node(),list_to_binary(SocketPid)},
    {p, 'ss1@192.168.0.117'} ! Term,
    ets:delete(socket_list,SocketPid),
    {stop, normal, State};

'WAIT_FOR_DATA'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(Event, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.


handle_info({ssl, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    io:format("handle_info ~p\n",[StateName]),
    ssl:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({ssl_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    log4erl:log(info,"~p Client ~p disconnected.\n", [self(), Addr]),

    SocketPid = get_socket_pid(Socket), 
    %tell process node disconnect 
    Data = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><package uid=\"\"><request type=\"logout\" id=\"\" ></request></package>">>,
    Term = {Data,node(),list_to_binary(SocketPid)},
    {p, 'ss1@192.168.0.117'} ! Term,
    ets:delete(socket_list,SocketPid),
    {stop, normal, StateData};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, StateName, State) ->
    io:format("what fuck ~p\n",[Info]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #state{socket=Socket}) ->
    (catch ssl:close(Socket)),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_socket_pid(Socket)->
    SocketPid = case Socket of
        {sslsocket,new_ssl,Pid} -> io:format("socket pid is ~p\n",[Pid]),
        pid_to_list(Pid)
    end,
    SocketPid.

   


