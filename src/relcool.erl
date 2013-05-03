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
%%% @doc
-module(relcool).

-export([main/1,
         do/2,
         do/7,
         do/8,
         do/9,
         format_error/1,
         opt_spec_list/0]).

-export_type([error/0]).

-include_lib("relcool/include/relcool.hrl").

%%============================================================================
%% types
%%============================================================================

-type error() :: {error, {Module::module(), Reason::term()}}.
-type goal() :: string() | binary() | rcl_depsolver:constraint().

%%============================================================================
%% API
%%============================================================================
-spec main([string()]) -> ok | error() | {ok, rcl_state:t()}.
main(Args) ->
    OptSpecList = opt_spec_list(),
    Result = case getopt:parse(OptSpecList, Args) of
                 {ok, {Options, NonOptions}} ->
                     do([{caller, command_line} | Options], NonOptions);
                 {error, Detail} ->
                     ?RCL_ERROR({opt_parse, Detail})
             end,
    case Result of
        {error, _} ->
            report_error(rcl_state:caller(rcl_state:new([], undefined),
                                          command_line),
                         Result);
        _ ->
            Result
    end.

%% @doc provides an API to run the Relcool process from erlang applications
%%
%% @param RelName - The release name to build (maybe `undefined`)
%% @param RelVsn - The release version to build (maybe `undefined`)
%% @param Goals - The release goals for the system in depsolver or Relcool goal
%% format
%% @param LibDirs - The library dirs that should be used for the system
%% @param OutputDir - The directory where the release should be built to
%% @param Configs - The list of config files for the system
-spec do(atom(), string(), [goal()], [file:name()], rcl_log:log_level(),
         [file:name()], file:name() | undefined) ->
                  ok | error() | {ok, rcl_state:t()}.
do(RelName, RelVsn, Goals, LibDirs, LogLevel, OutputDir, Config) ->
    {ok, Cwd} = file:get_cwd(),
    do(Cwd, RelName, RelVsn, Goals, LibDirs, LogLevel, OutputDir, [], Config).

%% @doc provides an API to run the Relcool process from erlang applications
%%
%% @param RootDir - The root directory for the project
%% @param RelName - The release name to build (maybe `undefined`)
%% @param RelVsn - The release version to build (maybe `undefined`)
%% @param Goals - The release goals for the system in depsolver or Relcool goal
%% format
%% @param LibDirs - The library dirs that should be used for the system
%% @param OutputDir - The directory where the release should be built to
%% @param Configs - The list of config files for the system
-spec do(file:name(), atom(), string(), [goal()], [file:name()],
           rcl_log:log_level(), [file:name()], file:name() | undefined) ->
                  ok | error() | {ok, rcl_state:t()}.
do(RootDir, RelName, RelVsn, Goals, LibDirs, LogLevel, OutputDir, Configs) ->
    do(RootDir, RelName, RelVsn, Goals, LibDirs, LogLevel, OutputDir, [], Configs).

%% @doc provides an API to run the Relcool process from erlang applications
%%
%% @param RootDir - The root directory for the system
%% @param RelName - The release name to build (maybe `undefined`)
%% @param RelVsn - The release version to build (maybe `undefined`)
%% @param Goals - The release goals for the system in depsolver or Relcool goal
%% format
%% @param LibDirs - The library dirs that should be used for the system
%% @param OutputDir - The directory where the release should be built to
%% @param Overrides - A list of overrides for the system
%% @param Configs - The list of config files for the system
-spec do(file:name(), atom(), string(), [goal()], [file:name()],
           rcl_log:log_level(), [file:name()], [{atom(), file:name()}], file:name() | undefined) ->
                  ok | error() | {ok, rcl_state:t()}.
do(RootDir, RelName, RelVsn, Goals, LibDirs, LogLevel, OutputDir, Overrides, Config) ->
    do([{relname, RelName},
        {relvsn, RelVsn},
        {goals, Goals},
        {overrides, Overrides},
        {output_dir, OutputDir},
        {lib_dirs, LibDirs},
        {root_dir, RootDir},
        {log_level, LogLevel},
        {config, Config}],
       ["release"]).

%% @doc provides an API to run the Relcool process from erlang applications
%%
%% @param Opts - A proplist of options. There are good defaults for each of
%% these entries, so any or all may be omitted. Individual options may be:
%%
%%  <dl>
%%   <dt><pre>{relname, RelName}</pre></dt>
%%   <dd>The release name to build </dd>
%%   <dt><pre>{relvsn, RelVsn}</pre></dt>
%%   <dd>The release version to build</dd>
%%   <dt><pre>{goals, Goals}</pre></dt>
%%   <dd>The release goals for the system in depsolver or Relcool goal
%%       format (@see goals())</dd>
%%   <dt><pre>{lib_dirs, LibDirs}</pre></dt>
%%   <dd>A list of library dirs that should be used for the system</dd>
%%   <dt><pre>{lib_dir, LibDir}</pre></dt>
%%   <dd>A single lib dir that should be used for the system, may appear any
%%   number of times and may be used in conjunction with lib_dirs</dd>
%%   <dt><pre>{output_dir, OutputDir}</pre></dt>
%%   <dd>The directory where the release should be built to</dd>
%%   <dt><pre>{root_dir, RootDir}</pre></dt>
%%   <dd>The base directory for this run of relcool. </dd>
%%   <dt><pre>{config, Config}</pre></dt>
%%   <dd>The path to a relcool config file</dd>
%%   <dt><pre>{log_level, LogLevel}</pre></dt>
%%   <dd>Defines the verbosity of output. Maybe a number between 0 and 2, with
%%   with higher values being more verbose</dd>
%%   <dt><pre>{overrides, Overrides}</pre></dt>
%%   <dd>A list of app overrides for the system in the form of [{AppName:atom(),
%%   Dir:string() | binary()} | string() | binary()] where the string or binary
%%   is in the form "AppName:AppDir"</dd>
%%   <dt><pre>{override, Override}</pre></dt>
%%   <dd>A single of app override for the system in the same form as entries for
%%   Overrides</dd>
%% </dl>
-spec do(proplists:proplist(), [string()]) ->
                  ok | error() | {ok, rcl_state:t()}.
do(Opts, NonOpts) ->
        case rcl_cmd_args:args2state(Opts, NonOpts) of
            {ok, State} ->
                run_relcool_process(State);
            Error={error, _} ->
                Error
        end.

-spec opt_spec_list() -> [getopt:option_spec()].
opt_spec_list() ->
    [{relname,  $n, "relname",  string,
      "Specify the name for the release that will be generated"},
     {relvsn, $v, "relvsn", string, "Specify the version for the release"},
     {goal, $g, "goal", string,
      "Specify a target constraint on the system. These are usually the OTP"},
     {upfrom, $u, "upfrom", string,
      "Only valid with relup target, specify the release to upgrade from"},
     {output_dir, $o, "output-dir", string,
      "The output directory for the release. This is `./` by default."},
     {lib_dir, $l, "lib-dir", string,
      "Additional dirs that should be searched for OTP Apps"},
     {disable_default_libs, undefined, "disable-default-libs",
      {boolean, false},
      "Disable the default system added lib dirs (means you must add them all manually"},
     {log_level, $V, "verbose", {integer, 1},
      "Verbosity level, maybe between 0 and 2"},
     {override_app, $a, "override_app", string,
      "Provide an app name and a directory to override in the form <appname>:<app directory>"},
     {config, $c, "config", {string, ""}, "The path to a config file"},
     {root_dir, $r, "root", string, "The project root directory"}].

-spec format_error(Reason::term()) -> iolist().
format_error({invalid_return_value, Provider, Value}) ->
    [rcl_provider:format(Provider), " returned an invalid value ",
     io_lib:format("~p", [Value])];
format_error({opt_parse, {invalid_option, Opt}}) ->
    io_lib:format("invalid option ~s~n", [Opt]);
format_error({opt_parse, Arg}) ->
    io_lib:format("~p~n", [Arg]);
format_error({error, {Module, Reason}}) ->
    io_lib:format("~s~n", [Module:format_error(Reason)]).

%%============================================================================
%% internal api
%%============================================================================
run_relcool_process(State) ->
    rcl_log:info(rcl_state:log(State), "Starting relcool build process ..."),
    rcl_log:debug(rcl_state:log(State),
                  fun() ->
                          rcl_state:format(State)
                  end),
    run_providers(State).

%% @doc for now the 'config' provider is special in that it generates the
%% providers used by the rest of the system. We expect the config provider to be
%% the first provider in the system. Once the config provider is run, we get the
%% providers again and run the rest of them (because they could have been
%% updated by the config process).
run_providers(State0) ->
    [ConfigProvider | _] = rcl_state:providers(State0),
    case run_provider(ConfigProvider, {ok, State0}) of
        Err = {error, _} ->
            Err;
        {ok, State1} ->
            RootDir = rcl_state:root_dir(State1),
            ok = file:set_cwd(RootDir),
            Providers = rcl_state:providers(State1),
            Result = run_providers(ConfigProvider, Providers, State1),
            handle_output(State1, rcl_state:caller(State1), Result)
    end.

handle_output(State, command_line, E={error, _}) ->
    report_error(State, E),
    init:stop(127);
handle_output(_State, command_line, _) ->
    init:stop(0);
handle_output(_State, api, Result) ->
    Result.

run_providers(ConfigProvider, Providers, State0) ->
    case Providers of
        [ConfigProvider | Rest] ->
            %% IF the config provider is still the first provider do not run it
            %% again just run the rest.
            lists:foldl(fun run_provider/2, {ok, State0}, Rest);
        _ ->
            lists:foldl(fun run_provider/2, {ok, State0}, Providers)
    end.

-spec run_provider(rcl_provider:t(), {ok, rcl_state:t()} | error()) ->
                          {ok, rcl_state:t()} | error().
run_provider(_Provider, Error = {error, _}) ->
    Error;
run_provider(Provider, {ok, State0}) ->
    rcl_log:debug(rcl_state:log(State0), "Running provider ~p~n",
                  [rcl_provider:impl(Provider)]),
    case rcl_provider:do(Provider, State0) of
        {ok, State1} ->
            rcl_log:debug(rcl_state:log(State0), "Provider successfully run: ~p~n",
                          [rcl_provider:impl(Provider)]),
            {ok, State1};
        E={error, _} ->
            rcl_log:debug(rcl_state:log(State0), "Provider (~p) failed with: ~p~n",
                          [rcl_provider:impl(Provider), E]),
            E
    end.

-spec usage() -> ok.
usage() ->
    getopt:usage(opt_spec_list(), "relcool", "[*release-specification-file*]").

-spec report_error(rcl_state:t(), error()) -> none() | error().
report_error(State, Error) ->
    io:format(format_error(Error)),
    usage(),
    case rcl_state:caller(State) of
        command_line ->
            erlang:halt(127);
        api ->
            Error
    end.
