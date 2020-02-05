-module(rlx_log).

-export([log/3]).

log(_Level, Msg, Args) ->
    io:format(Msg, Args).
