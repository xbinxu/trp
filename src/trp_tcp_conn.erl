%%% -------------------------------------------------------------------
%%% @author  xiaobin xu <xiaobin@firevale.com>
%%% @copyright  2012  xiaobin xu.
%%% @doc  
%%% -------------------------------------------------------------------

-module(trp_tcp_conn).
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

-record(state, {sock=undefined, rest_bin = <<>>, ping_timer=undefined}).

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
    TimerRef = erlang:start_timer(60000, self(), ping),
    {ok, #state{sock=Sock, ping_timer=TimerRef}}.


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
handle_call({request_send, FromSock, Bin}, _From, #state{sock=Sock} = State) ->
    ?DEBUG("=====> deliver request to client: ~n ~p ~n", [Bin]),
    send_message(FromSock, Sock, Bin),
    {reply, ok, State};

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
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, #state{sock=Socket, rest_bin=RestBin} = State) ->
    case binary:match(Bin, <<"tgw_l7_forward">>) of
        nomatch ->
            {ok, NewRestBin, MsgL} = tcp_message_parser:process_received_msg(<<RestBin/binary, Bin/binary>>), 
            lists:foreach(fun(Msg) -> handle_binary_message(Socket, Msg) end, MsgL),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{rest_bin=NewRestBin}};
        _ ->
            ?DEBUG("client connection established: ~n ~p ~n", [Bin]),
            trp_tcp_server:reset_sock(Socket, self()),
            ok= gen_tcp:send(Socket, <<"ok">>),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State}
        end;

handle_info({tcp_closed, _Socket}, State) ->
    ?DEBUG("tcp connection closed"),
    trp_tcp_server:clear_sock(self()),
    {stop, "connection to TRP client closed", State};


handle_info({timeout, TimerRef, ping}, #state{sock=Socket, ping_timer=TimerRef}=State) ->
    ok = send_message(Socket, <<"ping">>),
    erlang:cancel_timer(TimerRef),
    NewTimerRef = erlang:start_timer(60000, self(), ping),
    {noreply, State#state{ping_timer=NewTimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, #state{sock=Sock}) ->
    trp_tcp_server:clear_sock(self()),
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
send_message(FromSock, Sock, Bin) ->
    MsgBody = term_to_binary({FromSock, Bin}),
    send_message(Sock, MsgBody).

send_message(Sock, Bin) ->
    MsgLeng = byte_size(Bin),
    Package = <<MsgLeng:4/native-unit:8, Bin/binary>>,
    ok = gen_tcp:send(Sock, Package).    

handle_binary_message(Sock, <<"ping">>) ->
    send_message(Sock, <<"pong">>);

handle_binary_message(_Sock, <<"pong">>) ->
    ok;

handle_binary_message(_Sock, MsgBody) ->
    {ConnId, Msg} = binary_to_term(MsgBody),
    ?DEBUG("receive message from client, origin connId: ~p ~n ~p ~n", [ConnId, Msg]),
    case trp_http_server:get_pid_by_sock(ConnId)  of
        undefined ->
            ok;
        {ok, Pid} ->
            gen_server:cast(Pid, {send_response, Msg}) 
        end.
