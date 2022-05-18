%%%-------------------------------------------------------------------
%% @doc upgrade_test public API
%% @end
%%%-------------------------------------------------------------------

-module(upgrade_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    upgrade_test_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
