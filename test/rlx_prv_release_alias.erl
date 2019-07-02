-module(rlx_prv_release_alias).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, test_release_alias).
-define(DEPS, [app_discover]).

%%============================================================================
%% API
%%============================================================================

-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    State1 = rlx_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {deps, ?DEPS}])),
    {ok, State1}.

-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State) ->
    rlx_prv_release:do(State).

-spec format_error(ErrorDetail::term()) -> iolist().
format_error(ErrorDetail) ->
    rlx_prv_release:format_error(ErrorDetail).
