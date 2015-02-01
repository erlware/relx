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
%%% @doc This provider uses the lib_dir setting of the state. It searches the
%%% Lib Dirs looking for all OTP Applications that are available. When it finds
%%% those OTP Applications it loads the information about them and adds them to
%%% the state of available apps. This implements the provider behaviour.
-module(rlx_prv_app_discover).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("relx.hrl").

-define(PROVIDER, app_discover).
-define(DEPS, []).

%%============================================================================
%% API
%%============================================================================

-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    State1 = rlx_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {deps, ?DEPS}])),
    {ok, State1}.

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State0) ->
    LibDirs = dedup(get_lib_dirs(State0)),
    case rlx_app_discovery:do(State0, LibDirs) of
        {ok, AppMeta} ->
            State1 = rlx_state:available_apps(State0, AppMeta),
            {ok, State1};
        Error ->
            Error
    end.

%% @doc this is here to comply with the signature. However, we do not actually
%% produce any errors and so simply return an empty string.
-spec format_error(any()) -> iolist().
format_error(_) ->
    "".

%%%===================================================================
%%% Internal Functions
%%%===================================================================

get_lib_dirs(State) ->
    LibDirs0 = rlx_state:lib_dirs(State),
    case rlx_state:get(State, default_libs, true) of
        false ->
            LibDirs0;
        true ->
            Output = erlang:iolist_to_binary(rlx_state:base_output_dir(State)),
            OutputDir = case ec_file:exists(binary_to_list(Output)) of
                            true ->
                                Output;
                            false ->
                                []
                        end,
            lists:flatten([LibDirs0,
                           add_common_project_dirs(State),
                           add_system_lib_dir(State),
                           OutputDir])
    end.

-spec add_common_project_dirs(rlx_state:t()) -> [file:name()].
add_common_project_dirs(State) ->
    %% Check to see if there is a rebar.config. If so then look for a deps
    %% dir. If both are there then we add that to the lib dirs.
    case rlx_state:get(State, disable_project_subdirs, false) of
        true ->
            [];
        false ->
            Root = rlx_state:root_dir(State),
            Apps = filename:join(Root, "apps"),
            Lib = filename:join(Root, "lib"),
            Deps = filename:join(Root, "deps"),
            Rebar3Deps = filename:join(Root, "_deps"),
            Ebin = filename:join(Root, "ebin"),
            lists:foldl(fun(Dir, LibDirs) ->
                                case ec_file:exists(Dir) of
                                    true ->
                                        [erlang:iolist_to_binary(Dir) | LibDirs];
                                    false ->
                                        LibDirs
                                end
                        end, [], [Rebar3Deps, Deps, Lib, Apps, Ebin])
    end.

-spec add_system_lib_dir(rlx_state:t()) -> [file:name()].
add_system_lib_dir(State) ->
    ExcludeSystem = rlx_state:get(State, discover_exclude_system, false),
    case rlx_state:get(State, system_libs, true) of
        Atom when is_atom(Atom) ->
            case ExcludeSystem of
                true ->
                    [];
                false ->
                    erlang:iolist_to_binary(code:lib_dir())
            end;
        SystemLibs ->
            erlang:iolist_to_binary(SystemLibs)
    end.

%% Order matters so this slow dedup needs to be used
dedup([]) ->
    [];
dedup([H|T]) ->
    [H | [X || X <- dedup(T), X /= H]].
