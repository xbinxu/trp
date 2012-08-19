-ifndef(_trp_included).
-define(_trp_included, yeah).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

%% Logging mechanism

-define(FMTSTR(Format, Args),
        lists:flatten(io_lib:format(Format, Args))).

-define(DEBUG(Message),
        lager:debug(Message)).

-define(DEBUG(Format, Args),
        lager:debug(Format, Args)).

-define(INFO_MSG(Message),
        lager:info(Message)).

-define(INFO_MSG(Format, Args),
        lager:info(Format, Args)).

-define(WARNING_MSG(Message),
        lager:warning(Message)).

-define(WARNING_MSG(Format, Args),
        lager:warning(Format, Args)).

-define(ERROR_MSG(Message),
        lager:error(Message)).

-define(ERROR_MSG(Format, Args),
        lager:error(Format, Args)).

-define(CRITICAL_MSG(Message),
        lager:critical(Message)).

-define(CRITICAL_MSG(Format, Args),
        lager:critical(Format, Args)).

-endif.