%%% -------------------------------------------------------------------
%%% @author  xiaobin xu <xiaobin@firevale.com>
%%% @copyright  2012  xiaobin xu.
%%% @doc  
%%% -------------------------------------------------------------------

-module(trp_tcpc_conn).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("trp.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, stop/0, send_message/2, delete_conn/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {proxy_socket = undefined, rest_bin = <<>>, timer=undefined}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

send_message(Msg, ConnId) ->
    gen_server:cast(?MODULE, {send_message, Msg, ConnId}).

delete_conn(ConnId) ->
    gen_server:call(?MODULE, {delete_conn, ConnId}).

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
    process_flag(trap_exit, true),
    
    {ok, PAddr} = application:get_env(domain_name),
    {ok, PPort} = application:get_env(tcps_port),
    {ok, PSock} = gen_tcp:connect(PAddr, PPort, [binary, {packet, 0}, {active, once}]),
    
    ets:new(sock_table, [named_table, set]),
    DomainName = list_to_binary(PAddr),
    ok = gen_tcp:send(PSock, <<"tgw_l7_forward\r\nHost: ", DomainName/binary, ":8002\r\n\r\n">>),
    
    TimerRef = erlang:start_timer(60000, self(), ping),
    
    {ok, #state{proxy_socket = PSock, rest_bin = <<>>, timer = TimerRef}}.


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
handle_call({delete_conn, ConnId}, _From, State) ->
    ets:delete(sock_table, ConnId),
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
handle_cast({send_message, Msg, ConnId}, #state{proxy_socket=PSock} = State) ->
   ?DEBUG("~n~n send message to proxy server: ~p ~n ~n", [Msg]),
   ok = send_message_i(ConnId, PSock, Msg),
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
handle_info({tcp, Socket, <<"ok">>}, #state{proxy_socket=Socket} = State) ->
    ?DEBUG("tcp connection with proxy server established", []),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp, Socket, Bin}, #state{proxy_socket=Socket, rest_bin=RestBin} = State) ->
    {ok, NewRestBin, MsgL} = tcp_message_parser:process_received_msg(<<RestBin/binary, Bin/binary>>), 
    lists:foreach(fun(Msg) -> handle_binary_message(Socket, Msg) end, MsgL),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{rest_bin=NewRestBin}};

handle_info({tcp_closed, _}, State) ->
    {stop, "tcp conn closed", State};

handle_info({timeout, TimerRef, ping}, #state{proxy_socket=PSock, timer=TimerRef}=State) ->
    ok = send_message_i(PSock, <<"ping">>),
    erlang:cancel_timer(TimerRef),
    NewTimerRef = erlang:start_timer(60000, self(), ping),
    {noreply, State#state{timer=NewTimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, #state{proxy_socket=Sock}) ->
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
send_message_i(ConnId, Sock, Bin) ->
    MsgBody = term_to_binary({ConnId, Bin}),
    send_message_i(Sock, MsgBody).

send_message_i(Sock, Bin) ->
    MsgLeng = byte_size(Bin),
    Package = <<MsgLeng:4/native-unit:8, Bin/binary>>,
    ok = gen_tcp:send(Sock, Package).    

handle_binary_message(Sock, <<"ping">>) ->
    send_message_i(Sock, <<"pong">>);

handle_binary_message(_Sock, <<"pong">>) ->
    ok;

handle_binary_message(_Sock, MsgBody) ->
    {ConnId, Msg} = binary_to_term(MsgBody),
    ?DEBUG("Receive Message From Proxy Server ~p: ~p ~n", [ConnId, Msg]),
    case ets:lookup(sock_table, ConnId) of
        [] ->
            {WAddr, WPort} = parse_addr_port(binary_to_list(Msg)),
            ?DEBUG("web server: ~p port: ~p", [WAddr, WPort]),
            {ok, Pid} = trp_httpc_conn:start_link(ConnId, WAddr, WPort),
            ets:insert(sock_table, {ConnId, Pid}),
            gen_server:cast(Pid, {send_message, Msg});
        [{ConnId, Pid}] ->
            gen_server:cast(Pid, {send_message, Msg})
        end.

%%ping_message() ->
%%    {ok, PAddr} = application:get_env(domain_name),
%%    DomainName = list_to_binary(PAddr),
%%    <<"tgw_l7_forward\r\nHost: ", DomainName/binary, ":8002\r\n\r\n">>.

parse_addr_port(HttpRequest) ->
    ?DEBUG("parse request: ~p", [HttpRequest]),
    case re:run(HttpRequest, "^[^ ]+ ([^ ]+) [^ ]+", [{capture, [1], list}]) of
        {match, [URI]} ->
            case http_uri:parse(URI) of
                {ok, {_Scheme, _UserInfo, Host, Port, _Path, _Query}} ->
                    {Host, Port};
                _ ->
                    default_addr_port()
                end;
        _ ->
            default_addr_port()
        end.
                
default_addr_port() ->
    {ok, Addr} = application:get_env(web_server_addr),
    {ok, Port} = application:get_env(web_server_port),
    {Addr, Port}.
    
    
    

