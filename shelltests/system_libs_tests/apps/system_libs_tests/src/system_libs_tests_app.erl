%%%-------------------------------------------------------------------
%% @doc system_libs_tests public API
%% @end
%%%-------------------------------------------------------------------

-module(system_libs_tests_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    system_libs_tests_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
