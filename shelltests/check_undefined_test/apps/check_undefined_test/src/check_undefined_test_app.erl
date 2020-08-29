%%%-------------------------------------------------------------------
%% @doc check_undefined_test public API
%% @end
%%%-------------------------------------------------------------------

-module(check_undefined_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% calls to functions that should result in warnings during release building
    crypto:start(),
    notamodule:dosomething(),

    check_undefined_test_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
