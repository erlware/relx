%%%-------------------------------------------------------------------
%% @doc shellcheck_test public API
%% @end
%%%-------------------------------------------------------------------

-module(shellcheck_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    shellcheck_test_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
