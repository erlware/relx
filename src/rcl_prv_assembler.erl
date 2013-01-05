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
%%% @doc Given a complete built release this provider assembles that release
%%% into a release directory.
-module(rcl_prv_assembler).

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
    {RelName, RelVsn} = rcl_state:default_release(State),
    Release = rcl_state:get_release(State, RelName, RelVsn),
    OutputDir = rcl_state:output_dir(State),
    case create_output_dir(OutputDir) of
        ok ->
            case rcl_release:realized(Release) of
                true ->
                    copy_app_directories_to_output(State, Release, OutputDir);
                false ->
                    ?RCL_ERROR({unresolved_release, RelName, RelVsn})
            end;
        Error ->
            Error
    end.

-spec format_error(ErrorDetail::term()) -> iolist().
format_error({unresolved_release, RelName, RelVsn}) ->
    io_lib:format("The release has not been resolved ~p-~s", [RelName, RelVsn]);
format_error({ec_file_error, AppDir, TargetDir, E}) ->
    io_lib:format("Unable to copy OTP App from ~s to ~s due to ~p",
                  [AppDir, TargetDir, E]);
format_error({config_does_not_exist, Path}) ->
    io_lib:format("The config file specified for this release (~s) does not exist!",
                  [Path]);
format_error({specified_erts_does_not_exist, ErtsVersion}) ->
    io_lib:format("Specified version of erts (~s) does not exist",
                  [ErtsVersion]);
format_error({release_script_generation_error, RelFile}) ->
    io_lib:format("Unknown internal release error generating the release file to ~s",
                  [RelFile]);
format_error({release_script_generation_warning, Module, Warnings}) ->
    ["Warnings generating release \s",
     rcl_util:indent(1), Module:format_warning(Warnings)];
format_error({unable_to_create_output_dir, OutputDir}) ->
    io_lib:format("Unable to create output directory (possible permissions issue): ~s",
                  [OutputDir]);
format_error({release_script_generation_error, Module, Errors}) ->
    ["Errors generating release \n",
     rcl_util:indent(1), Module:format_error(Errors)];
format_error({unable_to_make_symlink, AppDir, TargetDir, Reason}) ->
    io_lib:format("Unable to symlink directory ~s to ~s because \n~s~s",
                  [AppDir, TargetDir, rcl_util:indent(1),
                   file:format_error(Reason)]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec create_output_dir(file:name()) ->
                               ok | {error, Reason::term()}.
create_output_dir(OutputDir) ->
    case filelib:is_dir(OutputDir) of
        false ->
            case rcl_util:mkdir_p(OutputDir) of
                ok ->
                    ok;
                {error, _} ->
                    ?RCL_ERROR({unable_to_create_output_dir, OutputDir})
            end;
        true ->
            ok
    end.

copy_app_directories_to_output(State, Release, OutputDir) ->
    LibDir = filename:join([OutputDir, "lib"]),
    ok = ec_file:mkdir_p(LibDir),
    Apps = rcl_release:application_details(Release),
    Result = lists:filter(fun({error, _}) ->
                                   true;
                              (_) ->
                                   false
                           end,
                          lists:flatten(ec_plists:map(fun(App) ->
                                                              copy_app(LibDir, App)
                                                      end, Apps))),
    case Result of
        [E | _] ->
            E;
        [] ->
            create_release_info(State, Release, OutputDir)
    end.

copy_app(LibDir, App) ->
    AppName = erlang:atom_to_list(rcl_app_info:name(App)),
    AppVsn = rcl_app_info:vsn_as_string(App),
    AppDir = rcl_app_info:dir(App),
    TargetDir = filename:join([LibDir, AppName ++ "-" ++ AppVsn]),
    remove_symlink_or_directory(TargetDir),
    case rcl_app_info:link(App) of
        true ->
            link_directory(AppDir, TargetDir);
        false ->
            copy_directory(AppDir, TargetDir)
    end.

remove_symlink_or_directory(TargetDir) ->
    case ec_file:is_symlink(TargetDir) of
        true ->
            ec_file:remove(TargetDir);
        false ->
            case filelib:is_dir(TargetDir) of
                true ->
                    ok = ec_file:remove(TargetDir, [recursive]);
                false ->
                    ok
            end
    end.

link_directory(AppDir, TargetDir) ->
    case file:make_symlink(AppDir, TargetDir) of
        {error, Reason} ->
            ?RCL_ERROR({unable_to_make_symlink, AppDir, TargetDir, Reason});
        ok ->
            ok
    end.

copy_directory(AppDir, TargetDir) ->
    ec_plists:map(fun(SubDir) ->
                          copy_dir(AppDir, TargetDir, SubDir)
                  end, ["ebin",
                        "include",
                        "priv",
                        "src",
                        "c_src",
                        "README",
                        "LICENSE"]).

copy_dir(AppDir, TargetDir, SubDir) ->
    SubSource = filename:join(AppDir, SubDir),
    SubTarget = filename:join(TargetDir, SubDir),
    case filelib:is_dir(SubSource) of
        true ->
            ok = rcl_util:mkdir_p(SubTarget),
            case ec_file:copy(SubSource, SubTarget, [recursive]) of
                {error, E} ->
                    ?RCL_ERROR({ec_file_error, AppDir, SubTarget, E});
                ok ->
                    ok
            end;
        false ->
            ok
    end.

create_release_info(State, Release, OutputDir) ->
    RelName = erlang:atom_to_list(rcl_release:name(Release)),
    ReleaseDir = filename:join([OutputDir,
                                "releases",
                                RelName ++ "-" ++
                                    rcl_release:vsn(Release)]),
    ReleaseFile = filename:join([ReleaseDir, RelName ++ ".rel"]),
    ok = ec_file:mkdir_p(ReleaseDir),
    case rcl_release:metadata(Release) of
        {ok, Meta} ->
                ok = ec_file:write_term(ReleaseFile, Meta),
                write_bin_file(State, Release, OutputDir, ReleaseDir);
        E ->
            E
    end.


write_bin_file(State, Release, OutputDir, RelDir) ->
    RelName = erlang:atom_to_list(rcl_release:name(Release)),
    RelVsn = rcl_release:vsn(Release),
    BinDir = filename:join([OutputDir, "bin"]),
    ok = ec_file:mkdir_p(BinDir),
    VsnRel = filename:join(BinDir, RelName ++ "-" ++ RelVsn),
    BareRel = filename:join(BinDir, RelName),
    ErlOpts = rcl_state:get(State, erl_opts, ""),
    StartFile = bin_file_contents(RelName, RelVsn,
                                  rcl_release:erts(Release),
                                  ErlOpts),
    %% We generate the start script by default, unless the user
    %% tells us not too
    case rcl_state:get(State, generate_start_script, true) of
        false ->
            ok;
        _ ->
            ok = file:write_file(VsnRel, StartFile),
            ok = file:change_mode(VsnRel, 8#777),
            ok = file:write_file(BareRel, StartFile),
            ok = file:change_mode(BareRel, 8#777)
    end,
    copy_or_generate_sys_config_file(State, Release, OutputDir, RelDir).

%% @doc copy config/sys.config or generate one to releases/VSN/sys.config
-spec copy_or_generate_sys_config_file(rcl_state:t(), rcl_release:t(),
                                       file:name(), file:name()) ->
                                              {ok, rcl_state:t()} | relcool:error().
copy_or_generate_sys_config_file(State, Release, OutputDir, RelDir) ->
    RelSysConfPath = filename:join([RelDir, "sys.config"]),
    case rcl_state:sys_config(State) of
        undefined ->
            ok = generate_sys_config_file(RelSysConfPath),
            include_erts(State, Release, OutputDir, RelDir);
        ConfigPath ->
            case filelib:is_regular(ConfigPath) of
                false ->
                    ?RCL_ERROR({config_does_not_exist, ConfigPath});
                true ->
                    ok = ec_file:copy(ConfigPath, RelSysConfPath),
                    include_erts(State, Release, OutputDir, RelDir)
            end
    end.

%% @doc write a generic sys.config to the path RelSysConfPath
-spec generate_sys_config_file(string()) -> ok.
generate_sys_config_file(RelSysConfPath) ->
    {ok, Fd} = file:open(RelSysConfPath, [write]),
    io:format(Fd,
              "%% Thanks to Ulf Wiger at Ericcson for these comments:~n"
              "%%~n"
              "%% This file is identified via the erl command line option -config File.~n"
              "%% Note that File should have no extension, e.g.~n"
              "%% erl -config .../sys (if this file is called sys.config)~n"
              "%%~n"
              "%% In this file, you can redefine application environment variables.~n"
              "%% This way, you don't have to modify the .app files of e.g. OTP applications.~n"
              "[].~n", []),
    file:close(Fd).

%% @doc Optionally add erts directory to release, if defined.
-spec include_erts(rcl_state:t(), rcl_release:t(),  file:name(), file:name()) -> {ok, rcl_state:t()} | relcool:error().
include_erts(State, Release, OutputDir, RelDir) ->
    case rcl_state:get(State, include_erts, true) of
        true ->
            Prefix = code:root_dir(),
            ErtsVersion = rcl_release:erts(Release),
            ErtsDir = filename:join([Prefix, "erts-" ++ ErtsVersion]),
            LocalErts = filename:join([OutputDir, "erts-" ++ ErtsVersion]),
            case filelib:is_dir(ErtsDir) of
                false ->
                    ?RCL_ERROR({specified_erts_does_not_exist, ErtsVersion});
                true ->
                    ok = ec_file:mkdir_p(LocalErts),
                    ok = ec_file:copy(ErtsDir, LocalErts, [recursive]),
                    make_boot_script(State, Release, OutputDir, RelDir)
            end;
        _ ->
            make_boot_script(State, Release, OutputDir, RelDir)
    end.


-spec make_boot_script(rcl_state:t(), rcl_release:t(), file:name(), file:name()) ->
                              {ok, rcl_state:t()} | relcool:error().
make_boot_script(State, Release, OutputDir, RelDir) ->
    Options = [{path, [RelDir | get_code_paths(Release, OutputDir)]},
               {outdir, RelDir},
               no_module_tests, silent],
    Name = erlang:atom_to_list(rcl_release:name(Release)),
    ReleaseFile = filename:join([RelDir, Name ++ ".rel"]),
    rcl_log:debug(rcl_state:log(State),
                  "Creating script from release file ~s ~n with options ~p ~n",
                  [ReleaseFile, Options]),
    case make_script(Name, Options)  of
        ok ->
            rcl_log:error(rcl_state:log(State),
                          "release successfully created!"),
            {ok, State};
        error ->
            ?RCL_ERROR({release_script_generation_error, ReleaseFile});
        {ok, _, []} ->
            rcl_log:error(rcl_state:log(State),
                          "release successfully created!"),
            {ok, State};
        {ok,Module,Warnings} ->
            ?RCL_ERROR({release_script_generation_warn, Module, Warnings});
        {error,Module,Error} ->
            ?RCL_ERROR({release_script_generation_error, Module, Error})
    end.

-spec make_script(string(), [term()]) ->
                         ok |
                         error |
                         {ok, module(), [term()]} |
                         {error,module,[term()]}.
make_script(Name, Options) ->
    %% Erts 5.9 introduced a non backwards compatible option to
    %% erlang this takes that into account
    Erts = erlang:system_info(version),
    case ec_semver:gte(Erts, "5.9") of
        true ->
            systools:make_script(Name, [no_warn_sasl | Options]);
        _ ->
            systools:make_script(Name, Options)
    end.

%% @doc Generates the correct set of code paths for the system.
-spec get_code_paths(rcl_release:t(), file:name()) -> [file:name()].
get_code_paths(Release, OutDir) ->
    LibDir = filename:join(OutDir, "lib"),
    [filename:join([LibDir,
                    erlang:atom_to_list(rcl_app_info:name(App)) ++ "-" ++
                        rcl_app_info:vsn_as_string(App), "ebin"]) ||
        App <- rcl_release:application_details(Release)].

bin_file_contents(RelName, RelVsn, ErtsVsn, ErlOpts) ->
    [<<"#!/bin/sh

set -e

SCRIPT_DIR=`dirname $0`
RELEASE_ROOT_DIR=`cd $SCRIPT_DIR/.. && pwd`
REL_NAME=">>, RelName, <<"
REL_VSN=">>, RelVsn, <<"
ERTS_VSN=">>, ErtsVsn, <<"
REL_DIR=$RELEASE_ROOT_DIR/releases/$REL_NAME-$REL_VSN
ERL_OPTS=">>, ErlOpts, <<"

ERTS_DIR=
SYS_CONFIG=
ROOTDIR=

ERTS_DIR=
SYS_CONFIG=
ROOTDIR=

find_erts_dir() {
    local erts_dir=$RELEASE_ROOT_DIR/erts-$ERTS_VSN
    if [ -d \"$erts_dir\" ]; then
        ERTS_DIR=$erts_dir;
        ROOTDIR=$RELEASE_ROOT_DIR
    else
        local erl=`which erl`
        local erl_root=`$erl -noshell -eval \"io:format(\\\"~s\\\", [code:root_dir()]).\" -s init stop`
        ERTS_DIR=$erl_root/erts-$ERTS_VSN
        ROOTDIR=$erl_root
    fi

}

find_sys_config() {
    local possible_sys=$REL_DIR/sys.config
    if [ -f \"$possible_sys\" ]; then
        SYS_CONFIG=\"-config $possible_sys\"
    fi
}

find_erts_dir
find_sys_config
export ROOTDIR=$RELEASE_ROOT_DIR
export BINDIR=$ERTS_DIR/bin
export EMU=beam
export PROGNAME=erl
export LD_LIBRARY_PATH=$ERTS_DIR/lib

$BINDIR/erlexec $ERL_OPTS $SYS_CONFIG -boot $REL_DIR/$REL_NAME $@">>].
