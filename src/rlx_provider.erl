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
         format_error/1,
         format_error/2,
         format/1]).

-export_type([t/0]).

-include_lib("relx/include/relx.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-opaque t() :: {?MODULE, module()}.


-ifdef(have_callback_support).

-callback init(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
-callback do(rlx_state:t()) ->  {ok, rlx_state:t()} | relx:error().
-callback format_error(Reason::term()) -> iolist().

-else.

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

-endif.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create a new provider object from the specified module. The
%% module should implement the provider behaviour.
%%
%% @param ModuleName The module name.
%% @param State0 The current state of the system
-spec new(module(), rlx_state:t()) ->
                 {t(), {ok, rlx_state:t()}} | relx:error().
new(ModuleName, State0) when is_atom(ModuleName) ->
    State1 = ModuleName:init(State0),
    case code:which(ModuleName) of
        non_existing ->
            ?RLX_ERROR({non_existing, ModuleName});
        _ ->
            {{?MODULE, ModuleName}, State1}
    end.

%% @doc Manipulate the state of the system, that new state
%%
%% @param Provider the provider object
%% @param State the current state of the system
-spec do(Provider::t(), rlx_state:t()) ->
                {ok, rlx_state:t()} | relx:error().
do({?MODULE, Mod}, State) ->
    Mod:do(State).

%%% @doc get the name of the module that implements the provider
%%% @param Provider the provider object
-spec impl(Provider::t()) -> module().
impl({?MODULE, Mod}) ->
    Mod.

%% @doc format an error produced from a provider.
-spec format_error(Reason::term()) -> iolist().
format_error({non_existing, ModuleName}) ->
    io_lib:format("~p does not exist in the system", [ModuleName]).

%% @doc format an error produced from a provider.
-spec format_error(t(), Reason::term()) -> iolist().
format_error({?MODULE, Mod}, Error) ->
    Mod:format_error(Error).

%% @doc print the provider module name
%%
%% @param T - The provider
%% @return An iolist describing the provider
-spec format(t()) -> iolist().
format({?MODULE, Mod}) ->
    erlang:atom_to_list(Mod).
