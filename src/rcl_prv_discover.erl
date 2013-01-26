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
%%% the state of available apps. This implements the rcl_provider behaviour.
-module(rcl_prv_discover).
-behaviour(rcl_provider).

-export([init/1,
         do/1,
         format_error/1]).

-include_lib("relcool/include/relcool.hrl").

%%============================================================================
%% API
%%============================================================================
-spec init(rcl_state:t()) -> {ok, rcl_state:t()}.
init(State) ->
    {ok, State}.

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rcl_state:t()) -> {ok, rcl_state:t()} | relcool:error().
do(State) ->
    LibDirs = get_lib_dirs(State),
    case rcl_app_discovery:do(State, LibDirs) of
        {ok, AppMeta} ->
            {ok, rcl_state:available_apps(State, AppMeta)};
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
    LibDirs0 = rcl_state:lib_dirs(State),
    case rcl_state:get(State, disable_default_libs, false) of
        true ->
            LibDirs0;
        false ->
            add_current_dir(State, LibDirs0)
    end.

-spec add_current_dir(rcl_state:t(), [file:name()]) -> [file:name()].
add_current_dir(State, LibDirs) ->
    %% Check to see if there is a rebar.config. If so then look for a deps
    %% dir. If both are there then we add that to the lib dirs.
    Root = rcl_state:root_dir(State),
    add_system_lib_dir(State, [filename:absname(Root) | LibDirs]).

-spec add_system_lib_dir(rcl_state:t(), [file:name()]) -> [file:name()].
add_system_lib_dir(State, LibDirs) ->
    ExcludeSystem = rcl_state:get(State, discover_exclude_system, false),

    case ExcludeSystem of
        true ->
            LibDirs;
        false ->
            SystemLibDir0 = code:lib_dir(),
            case filelib:is_dir(SystemLibDir0) of
                true ->
                    [SystemLibDir0 | LibDirs];
                false ->
                    LibDirs
            end
    end.
