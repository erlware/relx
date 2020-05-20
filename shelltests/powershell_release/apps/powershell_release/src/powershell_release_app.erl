%%%-------------------------------------------------------------------
%% @doc powershell_release public API
%% @end
%%%-------------------------------------------------------------------

-module(powershell_release_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    powershell_release_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
