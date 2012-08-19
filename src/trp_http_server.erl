%%% -------------------------------------------------------------------
%%% @author  xiaobin xu <xiaobin@firevale.com>
%%% @copyright  2012  xiaobin xu.
%%% @doc  
%%% -------------------------------------------------------------------

-module(trp_http_server).
-behaviour(gen_server).


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("trp.hrl").


%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, stop/0, register_sock_pid/2, unregister_sock_pid/2, get_pid_by_sock/1, get_conn_pid/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {conn_pid=undefined}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

register_sock_pid(Sock, Pid) ->
    gen_server:call(?MODULE, {register_sock_pid, Sock, Pid}).

unregister_sock_pid(Sock, Pid) ->
    gen_server:call(?MODULE, {unregister_sock_pid, Sock, Pid}).

get_pid_by_sock(Sock) ->
    gen_server:call(?MODULE, {get_pid_by_sock, Sock}).

get_conn_pid() ->
    gen_server:call(?MODULE, get_conn_pid).

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
    {ok, Port} = application:get_env(https_port),
    ets:new(http_sockets, [named_table, set]),
    socket_server:start_link('HTTP SERVER', 
                             Port, 
                             fun handle_sock_conn/2, 
                             fun handle_sock_disconn/2,
                             [binary, {packet, 0},{reuseaddr, true},{active, once}]),
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
handle_call({register_sock_pid, Sock, Pid}, _From, State) ->
    ets:insert(http_sockets, {Sock, Pid}),
    {reply, ok, State#state{conn_pid=Pid}};


handle_call({unregister_sock_pid, Sock, _Pid}, _From, State) ->
    ets:delete(http_sockets, Sock),
    {reply, ok, State#state{conn_pid=undefined}};

handle_call({get_pid_by_sock, Sock}, _From, State) ->
    case ets:lookup(http_sockets, Sock) of
        [{Sock, Pid}] ->
            {reply, {ok, Pid}, State};
        _ ->
            {reply, undefined, State}
        end;

handle_call(get_conn_pid, _From, #state{conn_pid=Pid}=State) ->
    {reply, {ok, Pid}, State};

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
    ?DEBUG("http server terminated: ~p", [Reason]),
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
    inet:setopts(Sock, [binary, {packet, line}, {active, once}]),
    {ok, Pid} = trp_http_conn:start_link(Sock), 
    register_sock_pid(Sock, Pid),
    {ok, Pid}.

handle_sock_disconn(_Pid, _Why) ->
    ok.

