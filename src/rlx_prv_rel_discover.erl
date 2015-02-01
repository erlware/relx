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
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012 Erlware, LLC.
%%%
-module(rlx_prv_rel_discover).
-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("relx.hrl").

-define(PROVIDER, rel_discover).
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
do(State0) ->
    LibDirs = get_lib_dirs(State0),
    AppMeta = rlx_state:available_apps(State0),
    case rlx_rel_discovery:do(State0, LibDirs, AppMeta) of
        {ok, Releases} ->
            {ok, rlx_state:realized_releases(State0, lists:foldl(fun add/2,
                                                                 ec_dictionary:new(ec_dict),
                                                                 Releases))};
        Error ->
            Error
    end.

-spec format_error(any()) -> iolist().
format_error(_) ->
    "".

%%%===================================================================
%%% Internal Functions
%%%===================================================================
%% @doc only add the release if its not documented in the system
add(Rel, Dict) ->
    RelName = rlx_release:name(Rel),
    RelVsn = rlx_release:vsn(Rel),
    ec_dictionary:add({RelName, RelVsn}, Rel, Dict).

get_lib_dirs(State) ->
    LibDirs0 = rlx_state:lib_dirs(State),
    case rlx_state:get(State, default_libs, true) of
        false ->
            LibDirs0;
        true ->
            lists:flatten([LibDirs0,
                           add_release_output_dir(State)])
    end.

add_release_output_dir(State) ->
    case rlx_state:get(State, disable_discover_release_output, false) of
        true ->
            [];
        false ->
            Output = erlang:iolist_to_binary(rlx_state:base_output_dir(State)),
            case ec_file:exists(erlang:binary_to_list(Output)) of
                true ->
                    Output;
                false ->
                    []
            end
    end.
