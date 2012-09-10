%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
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
%%% @doc Trivial utility file to help handle common tasks
-module(rcl_cmd_args).

-export([args2state/1]).

%%============================================================================
%% types
%%============================================================================


%%============================================================================
%% API
%%============================================================================
-spec args2state({error, Reason::term()} | {[getopt:option()], [string()]}) ->
                        {error, Reason::term()} |
                        {ok, {rcl_state:t(), [string()]}}.
args2state(Error={error, _}) ->
    Error;
args2state({ok, {Opts, Targets}}) ->
    RelName = proplists:get_value(relname, Opts, undefined),
    RelVsn = proplists:get_value(relvsn, Opts, undefined),
    case create_log(Opts,
                    [{relname, RelName},
                     {relvsn, RelVsn}]) of
        Error = {error, _} ->
            Error;
        {ok, CommandLineConfig} ->
            case validate_configs(Targets) of
                Error = {error, _} ->
                    Error;
                {ok, Configs} ->
                    {ok, {rcl_state:new(CommandLineConfig, Configs), Configs}}
            end
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec validate_configs([file:filename()]) ->
                              {ok, [file:filename()]} | {error, Reason::term()}.
validate_configs(Configs) ->
    Result =
        lists:foldl(fun(_Config, Err = {error, _}) ->
                            Err;
                       (Config, Acc) ->
                            case filelib:is_regular(Config) of
                                true ->
                                    [filename:absname(Config) | Acc];
                                false ->
                                    {error, {invalid_config_file, Config}}
                            end
                    end, [], Configs),
    case Result of
        {error, _} ->
            Result;
        _ ->
            %% Order may be important so lets make sure they remain in the same
            %% order they came in as
            {ok, lists:reverse(Result)}
    end.

-spec create_log([getopt:option()], rcl_state:cmd_args()) ->
                        {ok, rcl_state:cmd_args()} | {error, Reason::term()}.
create_log(Opts, Acc) ->
    LogLevel = proplists:get_value(log_level, Opts, 0),
    if
        LogLevel >= 0, LogLevel =< 2 ->
            create_goals(Opts, [{log, rcl_log:new(LogLevel)} | Acc]);
        true ->
            {error, {invalid_log_level, LogLevel}}
    end.

-spec create_goals([getopt:option()], rcl_state:cmd_args()) ->
                          {ok, rcl_state:cmd_args()} | {error, Reason::term()}.
create_goals(Opts, Acc) ->
    case convert_goals(proplists:get_all_values(goals, Opts), []) of
        Error={error, {failed_to_parse, _Spec}} ->
            Error;
        {ok, Specs} ->
            create_output_dir(Opts, [{goals, Specs} | Acc])
    end.

-spec convert_goals([string()], [depsolver:constraint()]) ->
                           {error,{failed_to_parse, string()}} |
                           {ok,[depsolver:constraint()]}.
convert_goals([], Specs) ->
    %% Reverse the specs because order matters to depsolver
    {ok, lists:reverse(Specs)};
convert_goals([RawSpec | Rest], Acc) ->
    case rcl_goal:parse(RawSpec) of
        {ok, Spec} ->
            convert_goals(Rest, [Spec | Acc]);
        {fail, _} ->
            {error, {failed_to_parse, RawSpec}}
    end.
-spec create_output_dir([getopt:option()], rcl_state:cmd_args()) ->
                               {ok, rcl_state:cmd_args()} | {error, Reason::term()}.
create_output_dir(Opts, Acc) ->
    OutputDir = proplists:get_value(output_dir, Opts, "./relcool_output"),
    case filelib:is_dir(OutputDir) of
        false ->
            case rcl_util:mkdir_p(OutputDir) of
                ok ->
                    create_lib_dirs(Opts, [{output_dir, OutputDir} | Acc]);
                {error, _} ->
                    {error, {unable_to_create_output_dir, OutputDir}}
            end;
        true ->
            create_lib_dirs(Opts, [{output_dir, OutputDir} | Acc])
    end.

-spec create_lib_dirs([getopt:option()], rcl_state:cmd_args()) ->
                               {ok, rcl_state:cmd_args()} | {error, Reason::term()}.
create_lib_dirs(Opts, Acc) ->
    Dirs = proplists:get_all_values(lib_dir, Opts),
    case check_lib_dirs(Dirs) of
        Error = {error, _} ->
            Error;
        ok ->
            {ok, [{lib_dirs, [filename:absname(Dir) || Dir <- Dirs]} | Acc]}
    end.

-spec check_lib_dirs([string()]) -> ok | {error, {Reason::atom(), Dir::string()}}.
check_lib_dirs([]) ->
    ok;
check_lib_dirs([Dir | Rest]) ->
    case filelib:is_dir(Dir) of
        false ->
            {error, {not_directory, Dir}};
        true ->
            check_lib_dirs(Rest)
    end.
