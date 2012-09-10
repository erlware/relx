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
         format/1,
         format/2]).

-export_type([t/0,
              cmd_args/0]).

-record(?MODULE, {log :: rcl_log:t(),
                  output_dir :: file:name(),
                  lib_dirs=[] :: [file:name()],
                  config_files=[] :: [file:filename()],
                  goals=[] :: [depsolver:constraint()]}).

%%============================================================================
%% types
%%============================================================================

-type cmd_args() :: proplists:proplist().
-opaque t() :: record(?MODULE).

%%============================================================================
%% API
%%============================================================================
%% @doc Create a new 'log level' for the system
-spec new(proplists:proplist(), [file:filename()]) -> t().
new(PropList, Targets) when erlang:is_list(PropList) ->
    #?MODULE{log = proplists:get_value(log, PropList, rcl_log:new(error)),
             output_dir=proplists:get_value(output_dir, PropList, ""),
             lib_dirs=proplists:get_value(lib_dirs, PropList, []),
             config_files=Targets,
             goals=proplists:get_value(goals, PropList, [])}.

%% @doc get the current log state for the system
-spec log(t()) -> rcl_log:t().
log(#?MODULE{log=LogState}) ->
    LogState.

-spec output_dir(t()) -> file:name().
output_dir(#?MODULE{output_dir=OutDir}) ->
    OutDir.

-spec lib_dirs(t()) -> [file:name()].
lib_dirs(#?MODULE{lib_dirs=LibDir}) ->
    LibDir.

-spec goals(t()) -> [depsolver:constraints()].
goals(#?MODULE{goals=TS}) ->
    TS.

-spec config_files(t()) -> [file:filename()].
config_files(#?MODULE{config_files=ConfigFiles}) ->
    ConfigFiles.

-spec format(t()) -> iolist().
format(Mod) ->
    format(Mod, 0).

-spec format(t(), non_neg_integer()) -> iolist().
format(#?MODULE{log=LogState, output_dir=OutDir, lib_dirs=LibDirs,
                goals=Goals, config_files=ConfigFiles},
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
     [[rcl_util:indent(Indent + 2), LibDir, ",\n"] || LibDir <- LibDirs]].

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
