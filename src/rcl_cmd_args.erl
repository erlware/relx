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
%%% @doc Trivial utility file to help handle common tasks
-module(rcl_cmd_args).

-export([args2state/1,
         format_error/1]).

-include_lib("relcool/include/relcool.hrl").

%%============================================================================
%% API
%%============================================================================
-spec args2state({error, Reason::term()} | {[getopt:option()], [string()]}) ->
                        {ok, {rcl_state:t(), [string()]}} |
                        relcool:error().
args2state({error, Detail}) ->
    ?RCL_ERROR({opt_parse, Detail});
args2state({ok, {Opts, Target}})
  when erlang:length(Target) == 0; erlang:length(Target) == 1 ->
    RelName = proplists:get_value(relname, Opts, undefined),
    RelVsn = proplists:get_value(relvsn, Opts, undefined),
    case convert_target(Target) of
        {ok, AtomizedTarget} ->
            case create_log(Opts, [{relname, RelName},
                                   {relvsn, RelVsn}]) of
                Error = {error, _} ->
                    Error;
                {ok, CommandLineConfig} ->
                    handle_config(Opts, AtomizedTarget, CommandLineConfig)
            end;
        Error ->
            Error
    end;
args2state({ok, {_Opts, Targets}}) ->
    ?RCL_ERROR({invalid_targets, Targets}).

-spec format_error(Reason::term()) -> iolist().
format_error({invalid_targets, Targets}) ->
    io_lib:format("One config must be specified! not ~p~n", [Targets]);
format_error({opt_parse, {invalid_option, Opt}}) ->
    io_lib:format("invalid option ~s~n", [Opt]);
format_error({opt_parse, Arg}) ->
    io_lib:format("~p~n", [Arg]);
format_error({invalid_option_arg, Arg}) ->
    case Arg of
        {goals, Goal} ->
            io_lib:format("Invalid Goal argument -g ~p~n", [Goal]);
        {relname, RelName} ->
            io_lib:format("Invalid Release Name argument -n ~p~n", [RelName]);
        {relvsn, RelVsn} ->
            io_lib:format("Invalid Release Version argument -n ~p~n", [RelVsn]);
        {output_dir, Outdir} ->
            io_lib:format("Invalid Output Directory argument -n ~p~n", [Outdir]);
        {lib_dir, LibDir} ->
            io_lib:format("Invalid Library Directory argument -n ~p~n", [LibDir]);
        {log_level, LogLevel} ->
            io_lib:format("Invalid Library Directory argument -n ~p~n", [LogLevel])
    end;
format_error({invalid_config_file, Config}) ->
    io_lib:format("Invalid configuration file specified: ~s", [Config]);
format_error({failed_to_parse, Spec}) ->
    io_lib:format("Unable to parse spec ~s", [Spec]);
format_error({not_directory, Dir}) ->
    io_lib:format("Library directory does not exist: ~s", [Dir]);
format_error({invalid_log_level, LogLevel}) ->
    io_lib:format("Invalid log level specified -V ~p, log level must be in the"
                 " range 0..2", [LogLevel]);
format_error({invalid_target, Target}) ->
    io_lib:format("Invalid action specified: ~s", [Target]).


%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec handle_config([getopt:option()], atom(), proplists:proplist()) ->
                           {ok, {rcl_state:t(), [string()]}} |
                           relcool:error().
handle_config(Opts, Target, CommandLineConfig) ->
    case validate_config(proplists:get_value(config, Opts, [])) of
        Error = {error, _} ->
            Error;
        {ok, Config} ->
            {ok, rcl_state:new([{config, Config} | CommandLineConfig], Target)}
    end.

-spec convert_target([string()]) -> {ok, release | relup} | relcool:error().
convert_target([]) ->
    {ok, release};
convert_target(["release"]) ->
    {ok, release};
convert_target(["relup"]) ->
    {ok, relup};
convert_target(Target) ->
    ?RCL_ERROR({invalid_target, Target}).

-spec validate_config(file:filename() | undefined) ->
                             {ok, file:filename() | undefined} | relcool:error().
validate_config(undefined) ->
    {ok, undefined};
validate_config("") ->
    {ok, undefined};
validate_config(Config) ->
    case filelib:is_regular(Config) of
        true ->
            filename:absname(Config);
        false ->
            ?RCL_ERROR({invalid_config_file, Config})
    end.

-spec create_log([getopt:option()], rcl_state:cmd_args()) ->
                        {ok, rcl_state:cmd_args()} | relcool:error().
create_log(Opts, Acc) ->
    LogLevel = proplists:get_value(log_level, Opts, 0),
    if
        LogLevel >= 0, LogLevel =< 2 ->
            create_goals(Opts, [{log, rcl_log:new(LogLevel)} | Acc]);
        true ->
            ?RCL_ERROR({invalid_log_level, LogLevel})
    end.

-spec create_goals([getopt:option()], rcl_state:cmd_args()) ->
                          {ok, rcl_state:cmd_args()} | relcool:error().
create_goals(Opts, Acc) ->
    case convert_goals(proplists:get_all_values(goals, Opts), []) of
        Error={error, _} ->
            Error;
        {ok, Specs} ->
            create_output_dir(Opts, [{goals, Specs} | Acc])
    end.

-spec convert_goals([string()], [rcl_depsolver:constraint()]) ->
                           {ok,[rcl_depsolver:constraint()]} |
                           relcool:error().
convert_goals([], Specs) ->
    %% Reverse the specs because order matters to rcl_depsolver
    {ok, lists:reverse(Specs)};
convert_goals([RawSpec | Rest], Acc) ->
    case rcl_goal:parse(RawSpec) of
        {ok, Spec} ->
            convert_goals(Rest, [Spec | Acc]);
        {fail, _} ->
            ?RCL_ERROR({failed_to_parse, RawSpec})
    end.
-spec create_output_dir([getopt:option()], rcl_state:cmd_args()) ->
                               {ok, rcl_state:cmd_args()} | relcool:error().
create_output_dir(Opts, Acc) ->
    OutputDir = proplists:get_value(output_dir, Opts, "./_rel"),
    create_lib_dirs(Opts, [{output_dir, filename:absname(OutputDir)} | Acc]).

-spec create_lib_dirs([getopt:option()], rcl_state:cmd_args()) ->
                               {ok, rcl_state:cmd_args()} | relcool:error().
create_lib_dirs(Opts, Acc) ->
    Dirs = proplists:get_all_values(lib_dir, Opts),
    case check_lib_dirs(Dirs) of
        Error = {error, _} ->
            Error;
        ok ->
            create_root_dir(Opts, [{lib_dirs, [filename:absname(Dir) || Dir <- Dirs]} | Acc])
    end.

-spec create_root_dir([getopt:option()], rcl_state:cmd_args()) ->
                               {ok, rcl_state:cmd_args()} | relcool:error().
create_root_dir(Opts, Acc) ->
    Dir = proplists:get_value(root_dir, Opts, undefined),
    case Dir of
        undefined ->
            {ok, Cwd} = file:get_cwd(),
            create_disable_default_libs(Opts, [{root_dir, Cwd} | Acc]);
        _ ->
            create_disable_default_libs(Opts, [{root_dir, Dir} | Acc])
    end.

-spec create_disable_default_libs([getopt:option()], rcl_state:cmd_args()) ->
                                         {ok, rcl_state:cmd_args()} | relcool:error().
create_disable_default_libs(Opts, Acc) ->
    Def = proplists:get_value(disable_default_libs, Opts, false),
    {ok, [{disable_default_libs, Def} | Acc]}.

-spec check_lib_dirs([string()]) -> ok | relcool:error().
check_lib_dirs([]) ->
    ok;
check_lib_dirs([Dir | Rest]) ->
    case filelib:is_dir(Dir) of
        false ->
            ?RCL_ERROR({not_directory, Dir});
        true ->
            check_lib_dirs(Rest)
    end.
