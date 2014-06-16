%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  A module that supports providing state manipulation services to the system.
%%% @end
%%%-------------------------------------------------------------------
-module(rlx_provider).

%% API
-export([new/2,
         do/2,
         impl/1,
         get_provider/2,
         get_target_providers/2,
         format_error/1,
         format_error/2,
         format/1]).

-export_type([t/0]).

-include("relx.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type t() :: record(provider).

-type provider_name() :: atom().

-ifdef(no_callback_support).

%% In the case where R14 or lower is being used to compile the system
%% we need to export a behaviour info
-export([behaviour_info/1]).
-spec behaviour_info(atom()) -> [{atom(), arity()}] | undefined.
behaviour_info(callbacks) ->
    [{init, 1},
     {do, 1},
     {format_error, 1}];
behaviour_info(_) ->
    undefined.

-else.

-callback init(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
-callback do(rlx_state:t()) ->  {ok, rlx_state:t()} | relx:error().
-callback format_error(Reason::term()) -> iolist().

-endif.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create a new provider object from the specified module. The
%% module should implement the provider behaviour.
%%
%% @param ModuleName The module name.
%% @param State0 The current state of the system
-spec new(atom(), rlx_state:t()) ->
                 {ok, rlx_state:t()} | relx:error().
new(ModuleName, State0) when is_atom(ModuleName) ->
    case code:which(ModuleName) of
        non_existing ->
            ?RLX_ERROR({non_existing, ModuleName});
        _ ->
            ModuleName:init(State0)
    end.

%% @doc Manipulate the state of the system, that new state
%%
%% @param Provider the provider object
%% @param State the current state of the system
-spec do(Provider::t(), rlx_state:t()) ->
                {ok, rlx_state:t()} | relx:error().
do(Provider, State) ->
    (Provider#provider.provider_impl):do(State).

%%% @doc get the name of the module that implements the provider
%%% @param Provider the provider object
-spec impl(Provider::t()) -> module().
impl(Provider) ->
    Provider#provider.name.

%% @doc format an error produced from a provider.
-spec format_error(Reason::term()) -> iolist().
format_error({provider_not_found, ModuleName}) ->
    io_lib:format("Provider ~p not found", [ModuleName]);
format_error({non_existing, ModuleName}) ->
    io_lib:format("~p does not exist in the system", [ModuleName]);
format_error({cycle_fault, Msg}) ->
    Msg.

%% @doc format an error produced from a provider.
-spec format_error(t(), Reason::term()) -> iolist().
format_error(#provider{provider_impl=Mod}, Error) ->
    Mod:format_error(Error).

%% @doc print the provider module name
%%
%% @param T - The provider
%% @return An iolist describing the provider
-spec format(t()) -> iolist().
format(#provider{provider_impl=Mod}) ->
    erlang:atom_to_list(Mod).

get_target_providers(Target, State) ->
    Providers = rlx_state:providers(State),
    Provider = get_provider(Target, Providers),
    process_deps(Provider, Providers).

-spec get_provider(provider_name(), [t()]) -> t().
get_provider(ProviderName, [Provider = #provider{name = ProviderName} | _]) ->
    Provider;
get_provider(ProviderName, [_ | Rest]) ->
    get_provider(ProviderName, Rest);
get_provider(ProviderName, _) ->
    format_error({provider_not_found, ProviderName}).

process_deps(Provider, Providers) ->
    {DepChain, _, _} = process_deps(Provider, Providers, []),
    ['NONE' | Rest] =
        reorder_providers(lists:flatten([{'NONE', Provider#provider.name} | DepChain])),
    Rest.

process_deps(Provider, Providers, Seen) ->
    case lists:member(Provider, Seen) of
        true ->
            {[], Providers, Seen};
        false ->
            Deps = Provider#provider.deps,
            DepList = lists:map(fun(Dep) ->
                                        {Dep, Provider#provider.name}
                                end, Deps),
            {NewDeps, _, NewSeen} =
                lists:foldl(fun(Arg, Acc) ->
                                    process_dep(Arg, Acc)
                            end,
                                                {[], Providers, Seen}, Deps),
            {[DepList | NewDeps], Providers, NewSeen}
    end.

process_dep(ProviderName, {Deps, Providers, Seen}) ->
    Provider = get_provider(ProviderName, Providers),
    {NewDeps, _, NewSeen} = process_deps(Provider, Providers, [ProviderName | Seen]),
    {[Deps | NewDeps], Providers, NewSeen}.

%% @doc Reorder the providers according to thier dependency set.
reorder_providers(OProviderList) ->
    case rlx_topo:sort(OProviderList) of
        {ok, ProviderList} ->
            ProviderList;
        {error, {rlx_topo, {cycle, _}}} ->
            format_error({cycle_fault,
                          "There was a cycle in the provider list. "
                          "Unable to complete build!"})
    end.
