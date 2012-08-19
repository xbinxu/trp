%% Author: xiaobin
%% Created: Jul 27, 2012
%% Description: TODO: Add description to tcp_message_parser
-module(tcp_message_parser).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([process_received_msg/1]).

%%
%% API Functions
%%

process_received_msg(Bin) when is_binary(Bin) ->
    process_received_msg(Bin, []).

process_received_msg(Bin, L) ->
    case Bin of
        <<MsgBodySize:4/native-unit:8, Extra/binary>> ->
            case Extra of
                <<MsgBody:MsgBodySize/binary, Rest/binary>> ->
                   process_received_msg(Rest, [MsgBody | L]); 
                <<_/binary>> ->
                    {ok, Bin, lists:reverse(L)}
            end;
        
        <<_SomeBin/binary>> ->
            {ok, Bin, lists:reverse(L)}
    end.    



%%
%% Local Functions
%%

