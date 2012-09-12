%%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
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
%%% @doc Provides state management services for the relcool tool. Generally,
%%% those things that are fixed have a direct api. Those things that are mutable
%%% have a more mutable api.
-module(rcl_state).

-export([new/2,
         log/1,
         output_dir/1,
         lib_dirs/1,
         goals/1,
         config_files/1,
         providers/1,
         providers/2,
         add_release/2,
         get_release/3,
         releases/1,
         default_release/1,
         default_release/3,
         available_apps/1,
         available_apps/2,
         format/1,
         format/2]).


-export_type([t/0,
              app_descriptor/0,
              releases/0,
              cmd_args/0]).

-record(state_t, {log :: rcl_log:t(),
                  output_dir :: file:name(),
                  lib_dirs=[] :: [file:name()],
                  config_files=[] :: [file:filename()],
                  goals=[] :: [depsolver:constraint()],
                  providers = [] :: [rcl_provider:t()],
                  available_apps = [] :: [app_descriptor()],
                  default_release :: {rcl_release:name(), rcl_release:vsn()},
                  releases :: ec_dictionary:dictionary({ReleaseName::atom(),
                                                        ReleaseVsn::string()},
                                                       rcl_release:t())}).

%%============================================================================
%% types
%%============================================================================

-type app_descriptor() :: {rcl_release:app_name(), rcl_release:app_vsn(), file:name()}.

-type releases() :: ec_dictionary:dictionary({rcl_release:name(),
                                              rcl_release:vsn()},
                                             rcl_release:t()).
-type cmd_args() :: proplists:proplist().
-opaque t() :: record(state_t).

%%============================================================================
%% API
%%============================================================================
%% @doc Create a new 'log level' for the system
-spec new(proplists:proplist(), [file:filename()]) -> t().
new(PropList, Targets) when erlang:is_list(PropList) ->
    State0 =
        #state_t{log = proplists:get_value(log, PropList, rcl_log:new(error)),
                 output_dir=proplists:get_value(output_dir, PropList, ""),
                 lib_dirs=get_lib_dirs(proplists:get_value(lib_dirs, PropList, [])),
                 config_files=Targets,
                 goals=proplists:get_value(goals, PropList, []),
                 providers = [],
                 releases=ec_dictionary:new(ec_dict)},
    create_logic_providers(State0).

%% @doc get the current log state for the system
-spec log(t()) -> rcl_log:t().
log(#state_t{log=LogState}) ->
    LogState.

-spec output_dir(t()) -> file:name().
output_dir(#state_t{output_dir=OutDir}) ->
    OutDir.

-spec lib_dirs(t()) -> [file:name()].
lib_dirs(#state_t{lib_dirs=LibDir}) ->
    LibDir.

-spec goals(t()) -> [depsolver:constraints()].
goals(#state_t{goals=TS}) ->
    TS.

-spec config_files(t()) -> [file:filename()].
config_files(#state_t{config_files=ConfigFiles}) ->
    ConfigFiles.

-spec providers(t()) -> [rcl_provider:t()].
providers(#state_t{providers=Providers}) ->
    Providers.

-spec providers(t(), [rcl_provider:t()]) -> t().
providers(M, NewProviders) ->
    M#state_t{providers=NewProviders}.

-spec add_release(t(), rcl_release:t()) -> t().
add_release(M=#state_t{releases=Releases}, Release) ->
    M#state_t{releases=ec_dictionary:add({rcl_release:name(Release),
                                          rcl_release:vsn(Release)},
                                         Release,
                                         Releases)}.

-spec get_release(t(), rcl_release:name(), rcl_release:vsn()) -> rcl_release:t().
get_release(#state_t{releases=Releases}, Name, Vsn) ->
    ec_dictionary:get({Name, Vsn}, Releases).

-spec releases(t()) -> releases().
releases(#state_t{releases=Releases}) ->
    Releases.

-spec default_release(t()) -> {rcl_release:name(), rcl_release:vsn()}.
default_release(#state_t{default_release=Def}) ->
    Def.

-spec default_release(t(), rcl_release:name(), rcl_release:vsn()) -> t().
default_release(M, Name, Vsn) ->
    M#state_t{default_release={Name, Vsn}}.

-spec available_apps(t()) -> [app_descriptor()].
available_apps(#state_t{available_apps=Apps}) ->
    Apps.

-spec available_apps(t(), [app_descriptor()]) -> t().
available_apps(M, NewApps) ->
    M#state_t{available_apps=NewApps}.

-spec format(t()) -> iolist().
format(Mod) ->
    format(Mod, 0).

-spec format(t(), non_neg_integer()) -> iolist().
format(#state_t{log=LogState, output_dir=OutDir, lib_dirs=LibDirs,
                goals=Goals, config_files=ConfigFiles,
               providers=Providers},
       Indent) ->
    [rcl_util:indent(Indent),
     <<"state:\n">>,
     rcl_util:indent(Indent + 1), <<"log: ">>, rcl_log:format(LogState), <<",\n">>,
     rcl_util:indent(Indent + 1), "config files: \n",
     [[rcl_util:indent(Indent + 2), ConfigFile, ",\n"] || ConfigFile <- ConfigFiles],
     rcl_util:indent(Indent + 1), "goals: \n",
     [[rcl_util:indent(Indent + 2), depsolver:format_constraint(Goal), ",\n"] || Goal <- Goals],
     rcl_util:indent(Indent + 1), "output_dir: ", OutDir, "\n",
     rcl_util:indent(Indent + 1), "lib_dirs: \n",
     [[rcl_util:indent(Indent + 2), LibDir, ",\n"] || LibDir <- LibDirs],
     rcl_util:indent(Indent + 1), "providers: \n",
     [[rcl_util:indent(Indent + 2), rcl_provider:format(Provider), ",\n"] || Provider <- Providers]].

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec get_lib_dirs([file:name()]) -> [file:name()].
get_lib_dirs(CmdDirs) ->
    case os:getenv("ERL_LIBS") of
        false ->
            CmdDirs;
        EnvString ->
            [Lib || Lib <- re:split(EnvString, ":|;"),
                     filelib:is_dir(Lib)] ++ CmdDirs
    end.

-spec create_logic_providers(t()) -> t().
create_logic_providers(State0) ->
    {ConfigProvider, {ok, State1}} = rcl_provider:new(rcl_prv_config, State0),
    {DiscoveryProvider, {ok, State2}} = rcl_provider:new(rcl_prv_discover, State1),
    State2#state_t{providers=[ConfigProvider, DiscoveryProvider]}.

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    LogState = rcl_log:new(error),
    RCLState = new([{log, LogState}], []),
    ?assertMatch(LogState, log(RCLState)).

-endif.
