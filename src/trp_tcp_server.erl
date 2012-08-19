%%% -------------------------------------------------------------------
%%% @author  xiaobin xu <xiaobin@firevale.com>
%%% @copyright  2012  xiaobin xu.
%%% @doc  
%%% -------------------------------------------------------------------

-module(trp_tcp_server).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("trp.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, stop/0, reset_sock/2, clear_sock/1, request_send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pid=undefined, sock=undefined}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

reset_sock(Sock, Pid) ->
    gen_server:call(?MODULE, {reset_sock, Sock, Pid}). 

clear_sock(Pid) ->
    gen_server:call(?MODULE, {clear_sock, Pid}).

request_send(FromSock, Bin) ->
    gen_server:call(?MODULE, {request_send, FromSock, Bin}).



%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------

init([]) ->
    {ok, Port} = application:get_env(tcps_port),
    socket_server:start_link('TCP SERVER', Port, fun handle_sock_conn/2, fun handle_sock_disconn/2),
    {ok, #state{}}.


%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({reset_sock, Sock, Pid}, _From, #state{pid=undefined}) ->
    ?DEBUG("reset client connection ~p -> ~p", [Sock, Pid]),
    {reply, ok, #state{sock=Sock, pid=Pid}};

handle_call({reset_sock, Sock, Pid}, _From, #state{sock=_OSock, pid=OPid}) ->
    ?DEBUG("reset client connection ~p -> ~p", [Sock, Pid]),
    gen_server:cast(OPid, {stop, "connection reset by another peer"}),
    {reply, ok, #state{sock=Sock, pid=Pid}};


handle_call({clear_sock, Pid}, From, #state{sock=OSock, pid=OPid}=State) ->
    case OPid =:= Pid of
        true ->
            ?DEBUG("[From: ~p] clear client connection ~p -> ~p =====> undefined -> undefined", [From, OSock, OPid]),
            {reply, ok, #state{sock=undefined, pid=undefined}};
        
        false ->
            {reply, ok, State}
        end;

handle_call({request_send, FromSock, Bin}, _From, #state{pid=Pid}=State) ->
    case Pid of
        undefined ->
            {reply, {error, "no client connection available"}, State};
        _ ->
            gen_server:call(Pid, {request_send, FromSock, Bin}),
            {reply, ok, State}
        end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, "Stop by command", State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
    ?DEBUG("tcp server terminated: ~p", [Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% return {ok, Pid}
handle_sock_conn(Sock, _ConnNum) -> 
    ?DEBUG("Sock connect by peer: ~p", [inet:peername(Sock)]),
    inet:setopts(Sock, [binary, {packet, 0}, {active, once}]),
    trp_tcp_conn:start_link(Sock). 

handle_sock_disconn(_Pid, _Why) ->
    ok.
