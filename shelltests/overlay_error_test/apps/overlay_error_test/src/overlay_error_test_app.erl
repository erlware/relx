%%%-------------------------------------------------------------------
%% @doc overlay_error_test public API
%% @end
%%%-------------------------------------------------------------------

-module(overlay_error_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    overlay_error_test_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
