-module(socket_server).
-author("Jesse E.I. Farmer <jesse@20bits.com>").
-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start_link/5, start_link/4, stop/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {port,
                      loop,
                      on_exit,
                      conn_num = 0,
                      ip=any,
                      lsocket=null,
                      sock_opt=undefined}).

start_link(Name, Port, Loop, OnExit, SockOption) ->
    State = #server_state{port = Port, loop = Loop, on_exit = OnExit, sock_opt=SockOption},
    gen_server:start_link({local, Name}, ?MODULE, State, []).
    
start_link(Name, Port, Loop, OnExit) ->
    State = #server_state{port = Port, loop = Loop, on_exit = OnExit},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

stop(Name) ->
    gen_server:cast(Name, stop).

init(State = #server_state{port=Port, sock_opt=SockOption}) ->
    process_flag(trap_exit, true),
    Option = case SockOption of 
                 undefined -> ?TCP_OPTIONS;
                 _ -> SockOption
                 end,
    case gen_tcp:listen(Port, Option) of
        {ok, LSocket} ->
            NewState = State#server_state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
        end.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    {noreply, accept(State)}.

accept_loop({Server, LSocket, Loop, ConnNum}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    % Let the server spawn a new process and replace this loop
    % with the echo loop, to avoid blocking 
    gen_server:cast(Server, {accepted, self()}),
    {ok, Pid} = Loop(Socket, ConnNum),
    ok = gen_tcp:controlling_process(Socket, Pid).

%% To be more robust we should be using spawn_link and trapping exits
accept(State = #server_state{lsocket=LSocket, conn_num=ConnNum, loop = Loop}) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop, ConnNum}]),
    State#server_state{conn_num = ConnNum+1}.

%% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> 
    {noreply, State}.

handle_info({'EXIT', Pid, Why}, #server_state{on_exit = OnExit} = State) ->
    OnExit(Pid, Why),
    {noreply, State};
handle_info(_Msg, Library) -> 
    {noreply, Library}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.