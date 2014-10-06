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
-module(rlx_cmd_args).

-export([args2state/2,
         format_error/1]).

-include("relx.hrl").

%%============================================================================
%% API
%%============================================================================
-spec args2state([getopt:option()], [string()]) ->
                        {ok, {rlx_state:t(), [string()]}} |
                        relx:error().
args2state(Opts, Targets) ->
    case convert_targets(Targets) of
        {ok, AtomizedTargets} ->
            case run_creates(Opts) of
                Error = {error, _} ->
                    Error;
                {ok, CommandLineConfig} ->
                    RelName = rlx_util:to_atom(proplists:get_value(relname, Opts, undefined)),
                    RelVsn = proplists:get_value(relvsn, Opts, undefined),
                    handle_config(Opts, AtomizedTargets,
                                 [{default_release, {RelName, RelVsn}} | CommandLineConfig])
            end;
        Error ->
            Error
    end.

-spec format_error(Reason::term()) -> iolist().
format_error({invalid_targets, Targets}) ->
    io_lib:format("One config must be specified! not ~p~n", [Targets]);
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
            io_lib:format("Invalid Log Level argument -n ~p~n", [LogLevel]);
        {path, Path} ->
            io_lib:format("Invalid code path argument -n ~p~n", [Path])
    end;
format_error({invalid_config_file, Config}) ->
    io_lib:format("Invalid configuration file specified: ~p", [Config]);
format_error({invalid_caller, Caller}) ->
    io_lib:format("Invalid caller specified: ~s", [Caller]);
format_error({failed_to_parse, Spec}) ->
    io_lib:format("Unable to parse spec ~s", [Spec]);
format_error({failed_to_parse_override, QA}) ->
    io_lib:format("Failed to parse app override ~s", [QA]);
format_error({not_directory, Dir}) ->
    io_lib:format("Library directory does not exist: ~s", [Dir]);
format_error({invalid_log_level, LogLevel}) ->
    io_lib:format("Invalid log level specified -V ~p, log level must be in the"
                 " range 0..3", [LogLevel]);
format_error({invalid_target, Target}) ->
    io_lib:format("Invalid action specified: ~s", [Target]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec handle_config(any(), [atom()], proplists:proplist()) ->
                           {ok, {rlx_state:t(), [string()]}} | relx:error().
handle_config(Opts, Targets, CommandLineConfig) ->
    case validate_config(proplists:get_value(config, Opts, [])) of
        Error = {error, _} ->
            Error;
        {ok, Config} ->
            case rlx_state:new(Config, CommandLineConfig, Targets) of
                {error, Error} ->
                    {error, Error};
                State ->
                    {ok, State}
            end
    end.

-spec convert_targets([string()]) -> {ok, release | relup} | relx:error().
convert_targets(Targets) ->
    convert_targets(Targets, []).

-spec convert_targets([string()], [rlx_state:action()]) ->
                             {ok, [rlx_state:action()]} | relx:error().
convert_targets([], []) ->
    {ok, [release]};
convert_targets([], Acc) ->
    {ok, Acc};
convert_targets(["release" | T], Acc) ->
    convert_targets(T, [release | Acc]);
convert_targets(["relup" | T], Acc) ->
    convert_targets(T, [relup | Acc]);
convert_targets(["tar" | T], Acc) ->
    convert_targets(T, [tar | Acc]);
convert_targets([Target | _T], _Acc) ->
    ?RLX_ERROR({invalid_target, Target}).

-spec validate_config(file:filename() | list() | undefined) ->
                             {ok, file:filename() | list() | undefined} | relx:error().
validate_config(undefined) ->
    {ok, undefined};
validate_config("") ->
    {ok, undefined};
validate_config(Config) ->
    case filelib:is_regular(Config) of
        true ->
            {ok, filename:absname(Config)};
        false ->
            case io_lib:printable_list(Config) of
                true ->
                    ?RLX_ERROR({invalid_config_file, Config});
                false when is_list(Config) ->
                    {ok, Config};
                false ->
                    ?RLX_ERROR({invalid_config_file, Config})
            end
    end.

run_creates(Opts) ->
    try
        Conf = lists:flatten(lists:foldl(fun(X, Acc) ->
                                                  [create(X, Opts) | Acc]
                                          end, [], proplists:get_keys(Opts))),
        {ok, Conf}
    catch
        throw:E ->
            E
    end.

create(log_level, Opts) ->
    LogLevel = proplists:get_value(log_level, Opts, 0),
    if
        LogLevel >= 0, LogLevel =< 3 ->
            {log, ec_cmd_log:new(LogLevel, command_line)};
        true ->
            throw(?RLX_ERROR({invalid_log_level, LogLevel}))
    end;
create(goal, Opts) ->
    Goals = proplists:get_value(goals, Opts, []) ++
        proplists:get_all_values(goal, Opts),
    case convert_goals(Goals, []) of
        Error={error, _} ->
            throw(Error);
        {ok, Specs} ->
            {goals, Specs}
    end;
create(goals, Opts) ->
    Goals = proplists:get_value(goals, Opts, []) ++
        proplists:get_all_values(goal, Opts),
    case convert_goals(Goals, []) of
        Error={error, _} ->
            throw(Error);
        {ok, Specs} ->
            {goals, Specs}
    end;
create(override, Opts) ->
    Overrides = proplists:get_value(overrides, Opts, []) ++
        proplists:get_all_values(override, Opts),
    case convert_overrides(Overrides, []) of
        {ok, Overrides2} ->
            {overrides, Overrides2};
        Error ->
            throw(Error)
    end;
create(overrides, Opts) ->
    Overrides = proplists:get_value(overrides, Opts, []) ++
        proplists:get_all_values(override, Opts),
    case convert_overrides(Overrides, []) of
        {ok, Overrides2} ->
            {overrides, Overrides2};
        Error ->
            throw(Error)
    end;
create(output_dir, Opts) ->
    OutputDir = proplists:get_value(output_dir, Opts, "./_rel"),
    {output_dir, filename:absname(OutputDir)};
create(lib_dir, Opts) ->
    Dirs = proplists:get_all_values(lib_dir, Opts) ++
        proplists:get_value(lib_dirs, Opts, []),
    ExpDirs = rlx_util:wildcard_paths(Dirs),
    case check_lib_dirs(ExpDirs) of
        Error = {error, _} ->
            throw(Error);
        ok ->
            LibDirs = [rlx_util:to_binary(Dir) || Dir <- ExpDirs],
            {lib_dirs, LibDirs}
    end;
create(lib_dirs, Opts) ->
    Dirs = proplists:get_all_values(lib_dir, Opts) ++
        proplists:get_value(lib_dirs, Opts, []),
    ExpDirs = rlx_util:wildcard_paths(Dirs),
    case check_lib_dirs(ExpDirs) of
        Error = {error, _} ->
            throw(Error);
        ok ->
            LibDirs = [rlx_util:to_binary(Dir) || Dir <- ExpDirs],
            {lib_dirs, LibDirs}
    end;
create(root_dir, Opts) ->
    Dir = proplists:get_value(root_dir, Opts, undefined),
    case Dir of
        undefined ->
            {ok, Cwd} = file:get_cwd(),
            {root_dir, Cwd};
        _ ->
            {root_dir, filename:absname(Dir)}
    end;
create(default_libs, Opts) ->
    Def = proplists:get_value(default_libs, Opts, true),
    {default_libs, Def};
create(overlay_vars, Opts)->
    OverlayVars = proplists:get_all_values(overlay_vars, Opts),
    {overlay_vars, OverlayVars};
create(sys_config, Opts) ->
    SysConfig = proplists:get_value(sys_config, Opts, undefined),
    {sys_config, SysConfig};
create(system_libs, Opts) ->
    SystemLibs = proplists:get_value(system_libs, Opts, undefined),
    {system_libs, SystemLibs};
create(upfrom, Opts) ->
    case proplists:get_value(upfrom, Opts, undefined) of
        undefined ->
            [];
        UpFrom ->
            {upfrom, UpFrom}
    end;
create(caller, Opts) ->
    case proplists:get_value(caller, Opts, api) of
        "command_line" ->
            {caller, command_line};
        "commandline" ->
            {caller, command_line};
        "api" ->
            {caller, api};
        api ->
            {caller, api};
        commandline ->
            {caller, command_line};
        command_line ->
            {caller, command_line};
        Caller ->
            ?RLX_ERROR({invalid_caller, Caller})
    end;
create(paths, Opts) ->
    Dirs = proplists:get_all_values(path, Opts) ++
        proplists:get_value(paths, Opts, []),
    case check_lib_dirs(Dirs) of
        Error = {error, _} ->
            throw(Error);
        ok ->
            code:add_pathsa([filename:absname(Path) || Path <- Dirs]),
            []
    end;
create(dev_mode, Opts) ->
    DevMode = proplists:get_value(dev_mode, Opts, false),
    {dev_mode, DevMode};
create(include_erts, Opts) ->
    case proplists:get_value(include_erts, Opts, true) of
        IncludeErts when IncludeErts =:= true
                       ; IncludeErts =:= "true" ->
            {include_erts, true};
        IncludeErts when IncludeErts =:= false
                       ; IncludeErts =:= "false" ->
            {include_erts, false};
        Erts when is_list(Erts) ->
            {include_erts, Erts}
    end;
create(_, _) ->
    [].

-spec convert_overrides([{atom(), string() | binary()} |
                         string() | binary()], [{atom(), string() | binary()}]) ->
                               {ok, [string() | binary()]} | relx:error().
convert_overrides([], Acc) ->
    {ok, Acc};
convert_overrides([QA = {OverrideApp, _} | Rest], Acc)
  when erlang:is_atom(OverrideApp) ->
    convert_overrides(Rest, [QA | Acc]);
convert_overrides([Override | Rest], Acc)
  when erlang:is_list(Override); erlang:is_binary(Override) ->
    case re:split(Override, ":") of
        [AppName, AppDir] ->
            convert_overrides(Rest, [{rlx_util:to_atom(AppName), AppDir} | Acc]);
        _ ->
             ?RLX_ERROR({failed_to_parse_override, Override})
    end;
convert_overrides([QA | _], _) ->
    ?RLX_ERROR({failed_to_parse_override, QA}).

-spec convert_goals([string()], [rlx_depsolver:constraint()]) ->
                           {ok,[rlx_depsolver:constraint()]} |
                           relx:error().
convert_goals([], Specs) ->
    %% Reverse the specs because order matters to rlx_depsolver
    {ok, lists:reverse(Specs)};
convert_goals([RawSpec | Rest], Acc) ->
    parse_goal(RawSpec, Rest, Acc).

-spec parse_goal(string() | binary() | rlx_depsolver:constraint(),
                  [string() | binary() | rlx_depsolver:constraint()],
                  rlx_depsolver:constraints()) ->
                 {ok, rlx_depsolver:constraints()} | relx:error().
parse_goal(Spec, Rest, Acc)
  when erlang:is_atom(Spec) ->
    convert_goals(Rest, [Spec | Acc]);
parse_goal(Spec, Rest, Acc)
  when erlang:is_tuple(Spec) ->
    convert_goals(Rest, [Spec | Acc]);
parse_goal(RawSpec, Rest, Acc) ->
    case rlx_goal:parse(RawSpec) of
        {ok, Spec} ->
            convert_goals(Rest, [Spec | Acc]);
        {fail, _} ->
            ?RLX_ERROR({failed_to_parse, RawSpec})
    end.

-spec check_lib_dirs([string()]) -> ok | relx:error().
check_lib_dirs([]) ->
    ok;
check_lib_dirs([Dir | Rest]) ->
    case filelib:is_dir(Dir) of
        false ->
            ?RLX_ERROR({not_directory, Dir});
        true ->
            check_lib_dirs(Rest)
    end.
