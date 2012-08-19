%%% -------------------------------------------------------------------
%%% @author  xiaobin xu <xiaobin@firevale.com>
%%% @copyright  2012  xiaobin xu.
%%% @doc  
%%% -------------------------------------------------------------------

-module(trp_http_conn).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("trp.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sock=undefined, scheme=http, ssl_check=false}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Sock) ->
    gen_server:start_link(?MODULE, [Sock], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

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
init([Sock]) ->
    {ok, #state{sock=Sock}}.


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
handle_cast({send_response, <<"disconn">>}, State) ->
    ?DEBUG("===============> web server disconnected......", []),
    {stop, normal, State};

handle_cast({send_response, Bin}, #state{sock = Sock} = State) ->
    ?DEBUG("===============> send response message to http client: ~n ~p", [Bin]),
    ok = gen_tcp:send(Sock, Bin),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({tcp, Socket, <<"\r\n">>}, #state{sock=Socket, scheme=https} = State) ->
    ok = trp_tcp_server:request_send(Socket, <<"\r\n">>),
    inet:setopts(Socket, [{active, once}, {packet, 0}]),
    {noreply, State};

handle_info({tcp, Socket, Bin}, #state{sock=Socket, ssl_check=false} = State) ->
    ?DEBUG("==============> http request received from http client: ~n ~p", [Bin]),
    ok = trp_tcp_server:request_send(Socket, Bin),
    inet:setopts(Socket, [{active, once}]),
    case binary:match(Bin, <<"CONNECT">>) of
        nomatch ->
            {noreply, State};
        _ ->
            {noreply, State#state{scheme=https, ssl_check=true}}
        end;

handle_info({tcp, Socket, Bin}, #state{sock=Socket, ssl_check=true} = State) ->
    ?DEBUG("==============> http request received from http client: ~n ~p", [Bin]),
    ok = trp_tcp_server:request_send(Socket, Bin),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, _Socket}, #state{sock=Sock} = State) ->
    ?DEBUG("http connection closed by http client"),
    trp_http_server:unregister_sock_pid(Sock, self()),
    {stop, normal, State};

handle_info(Info, #state{sock=Socket} = State) ->
    ?DEBUG("received: ~p", [Info]),
    inet:setopts(Socket, [{active, false}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, #state{sock=Sock}) ->
    gen_tcp:close(Sock),
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

