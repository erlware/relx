-module(rlx_log).

-export([log/4]).

log(Level, Msg, Args, State) ->
    case rlx_state:log_fun(Level, State) of
        no_log_funs when is_function(Msg) ->
            io:format(Msg(), Args);
        no_log_funs ->
            io:format(Msg, Args);
        no_level_fun ->
            ok;
        Fun ->
            Fun(Msg, Args)
    end.
