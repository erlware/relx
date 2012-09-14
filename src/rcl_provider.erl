%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  A module that supports providing state manipulation services to the system.
%%% @end
%%%-------------------------------------------------------------------
-module(rcl_provider).

%% API
-export([new/2, do/2, format_error/2, format/1]).

-export_type([t/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-opaque t() :: {?MODULE, module()}.

-callback init(rcl_state:t()) -> {ok, rcl_state:t()} | {error, Reason::term()}.
-callback do(rcl_state:t()) -> {error, Reason::term()} | {ok, rcl_state:t()}.
-callback format_error({error, Reason::term()}) -> iolist().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create a new provider object from the specified module. The
%% module should implement the provider behaviour.
%%
%% @param ModuleName The module name.
%% @param State0 The current state of the system
-spec new(module(), rcl_state:t()) ->
                 {t(), {ok, rcl_state:t()} | {error, Reason::term()}}.
new(ModuleName, State0) when is_atom(ModuleName) ->
    State1 = ModuleName:init(State0),
    case code:which(ModuleName) of
        non_existing ->
            erlang:error({non_existing, ModuleName});
        _ ->
            ok
    end,
    {{?MODULE, ModuleName}, State1}.

%% @doc Manipulate the state of the system, that new state
%%
%% @param Provider the provider object
%% @param State the current state of the system
-spec do(Provider::t(), rcl_state:t()) ->
                {error, Reason::term()} | {ok, rcl_state:t()}.
do({?MODULE, Mod}, State) ->
    Mod:do(State).

%% @doc format an error produced from a provider.
-spec format_error(t(), {error, Reason::term()}) -> iolist().
format_error({?MODULE, Mod}, Error) ->
    Mod:format_error(Error).


%% @doc print the provider module name
%%
%% @param T - The provider
%% @return An iolist describing the provider
-spec format(t()) -> iolist().
format({?MODULE, Mod}) ->
    erlang:atom_to_list(Mod).
