%%%-------------------------------------------------------------------
%% @doc force_nodetool_tests public API
%% @end
%%%-------------------------------------------------------------------

-module(force_nodetool_tests_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    force_nodetool_tests_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
