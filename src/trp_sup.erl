
-module(trp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("trp.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    case application:get_env(mode) of
        {ok, client} ->
            io:format("start as client mode...~n", []),
            {ok, { {one_for_one, 10, 10}, [?CHILD(trp_tcpc_conn, worker)]} };
        {ok, server} ->
            io:format("start as server mode...~n", []),
            {ok, { {one_for_one, 5, 10}, [?CHILD(trp_http_server, worker), 
                                          ?CHILD(trp_tcp_server, worker)]} }
        end.
            

