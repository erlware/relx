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

-spec format_error(Reason::term()) -> iolist().
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
                  " range 0..2", [LogLevel]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec validate_configs([file:filename()]) ->
                              {ok, [file:filename()]} | relcool:error().
validate_configs(Configs) ->
    Result =
        lists:foldl(fun(_Config, Err = {error, _}) ->
                            Err;
                       (Config, Acc) ->
                            case filelib:is_regular(Config) of
                                true ->
                                    [filename:absname(Config) | Acc];
                                false ->
                                    ?RCL_ERROR({invalid_config_file, Config})
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
            {ok, [{lib_dirs, [filename:absname(Dir) || Dir <- Dirs]} | Acc]}
    end.

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
