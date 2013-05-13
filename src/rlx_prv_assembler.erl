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
%%% @doc Given a complete built release this provider assembles that release
%%% into a release directory.
-module(rlx_prv_assembler).

-behaviour(rlx_provider).

-export([init/1,
         do/1,
         format_error/1]).

-include_lib("relx/include/relx.hrl").

%%============================================================================
%% API
%%============================================================================
-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    {ok, State}.

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State) ->
    {RelName, RelVsn} = rlx_state:default_configured_release(State),
    Release = rlx_state:get_realized_release(State, RelName, RelVsn),
    OutputDir = rlx_state:output_dir(State),
    case create_output_dir(OutputDir) of
        ok ->
            case rlx_release:realized(Release) of
                true ->
                    copy_app_directories_to_output(State, Release, OutputDir);
                false ->
                    ?RLX_ERROR({unresolved_release, RelName, RelVsn})
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
     rlx_util:indent(1), Module:format_warning(Warnings)];
format_error({unable_to_create_output_dir, OutputDir}) ->
    io_lib:format("Unable to create output directory (possible permissions issue): ~s",
                  [OutputDir]);
format_error({release_script_generation_error, Module, Errors}) ->
    ["Errors generating release \n",
     rlx_util:indent(1), Module:format_error(Errors)];
format_error({relup_generation_error, CurrentName, UpFromName}) ->
    io_lib:format("Unknown internal release error generating the relup from ~s to ~s",
                  [UpFromName, CurrentName]);
format_error({relup_generation_warning, Module, Warnings}) ->
    ["Warnings generating relup \s",
     rlx_util:indent(1), Module:format_warning(Warnings)];
format_error({relup_script_generation_error,
              {relupcript_generation_error, systools_relup,
               {missing_sasl, _}}}) ->
    "Unfortunately, due to requirements in systools, you need to have the sasl application "
        "in both the current release and the release to upgrade from.";
format_error({relup_script_generation_error, Module, Errors}) ->
    ["Errors generating relup \n",
     rlx_util:indent(1), Module:format_error(Errors)];
format_error({unable_to_make_symlink, AppDir, TargetDir, Reason}) ->
    io_lib:format("Unable to symlink directory ~s to ~s because \n~s~s",
                  [AppDir, TargetDir, rlx_util:indent(1),
                   file:format_error(Reason)]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec create_output_dir(file:name()) ->
                               ok | {error, Reason::term()}.
create_output_dir(OutputDir) ->
    case filelib:is_dir(OutputDir) of
        false ->
            case rlx_util:mkdir_p(OutputDir) of
                ok ->
                    ok;
                {error, _} ->
                    ?RLX_ERROR({unable_to_create_output_dir, OutputDir})
            end;
        true ->
            ok
    end.

copy_app_directories_to_output(State, Release, OutputDir) ->
    LibDir = filename:join([OutputDir, "lib"]),
    ok = ec_file:mkdir_p(LibDir),
    Apps = rlx_release:application_details(Release),
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
    AppName = erlang:atom_to_list(rlx_app_info:name(App)),
    AppVsn = rlx_app_info:vsn_as_string(App),
    AppDir = rlx_app_info:dir(App),
    TargetDir = filename:join([LibDir, AppName ++ "-" ++ AppVsn]),
    if
        AppDir == TargetDir ->
            %% No need to do anything here, discover found something already in
            %% a release dir
            ok;
        true ->
            copy_app(App, AppDir, TargetDir)
    end.

copy_app(App, AppDir, TargetDir) ->
    remove_symlink_or_directory(TargetDir),
    case rlx_app_info:link(App) of
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
            ?RLX_ERROR({unable_to_make_symlink, AppDir, TargetDir, Reason});
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
            ok = rlx_util:mkdir_p(SubTarget),
            case ec_file:copy(SubSource, SubTarget, [recursive]) of
                {error, E} ->
                    ?RLX_ERROR({ec_file_error, AppDir, SubTarget, E});
                ok ->
                    ok
            end;
        false ->
            ok
    end.

create_release_info(State0, Release0, OutputDir) ->
    RelName = erlang:atom_to_list(rlx_release:name(Release0)),
    ReleaseDir = release_output_dir(State0, Release0),
    ReleaseFile = filename:join([ReleaseDir, RelName ++ ".rel"]),
    ok = ec_file:mkdir_p(ReleaseDir),
    Release1 = rlx_release:relfile(Release0, ReleaseFile),
    State1 = rlx_state:update_realized_release(State0, Release1),
    case rlx_release:metadata(Release1) of
        {ok, Meta} ->
                ok = ec_file:write_term(ReleaseFile, Meta),
                write_bin_file(State1, Release1, OutputDir, ReleaseDir);
        E ->
            E
    end.


write_bin_file(State, Release, OutputDir, RelDir) ->
    RelName = erlang:atom_to_list(rlx_release:name(Release)),
    RelVsn = rlx_release:vsn(Release),
    BinDir = filename:join([OutputDir, "bin"]),
    ok = ec_file:mkdir_p(BinDir),
    VsnRel = filename:join(BinDir, rlx_release:canonical_name(Release)),
    BareRel = filename:join(BinDir, RelName),
    ErlOpts = rlx_state:get(State, erl_opts, ""),
    StartFile = case rlx_state:get(State, extended_start_script, false) of
                    false ->
                        bin_file_contents(RelName, RelVsn,
                                          rlx_release:erts(Release),
                                          ErlOpts);
                    true ->
                        extended_bin_file_contents(RelName, RelVsn, rlx_release:erts(Release), ErlOpts)
                end,
    %% We generate the start script by default, unless the user
    %% tells us not too
    case rlx_state:get(State, generate_start_script, true) of
        false ->
            ok;
        _ ->
            ok = file:write_file(VsnRel, StartFile),
            ok = file:change_mode(VsnRel, 8#777),
            ok = file:write_file(BareRel, StartFile),
            ok = file:change_mode(BareRel, 8#777)
    end,
    ok = file:write_file(filename:join(RelDir, "vm.args"), vm_args_file(RelName)),
    copy_or_generate_sys_config_file(State, Release, OutputDir, RelDir).

%% @doc copy config/sys.config or generate one to releases/VSN/sys.config
-spec copy_or_generate_sys_config_file(rlx_state:t(), rlx_release:t(),
                                       file:name(), file:name()) ->
                                              {ok, rlx_state:t()} | relx:error().
copy_or_generate_sys_config_file(State, Release, OutputDir, RelDir) ->
    RelSysConfPath = filename:join([RelDir, "sys.config"]),
    case rlx_state:sys_config(State) of
        undefined ->
            ok = generate_sys_config_file(RelSysConfPath),
            include_erts(State, Release, OutputDir, RelDir);
        ConfigPath ->
            case filelib:is_regular(ConfigPath) of
                false ->
                    ?RLX_ERROR({config_does_not_exist, ConfigPath});
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
-spec include_erts(rlx_state:t(), rlx_release:t(),  file:name(), file:name()) -> {ok, rlx_state:t()} | relx:error().
include_erts(State, Release, OutputDir, RelDir) ->
    case rlx_state:get(State, include_erts, true) of
        true ->
            Prefix = code:root_dir(),
            ErtsVersion = rlx_release:erts(Release),
            ErtsDir = filename:join([Prefix, "erts-" ++ ErtsVersion]),
            LocalErts = filename:join([OutputDir, "erts-" ++ ErtsVersion]),
            case filelib:is_dir(ErtsDir) of
                false ->
                    ?RLX_ERROR({specified_erts_does_not_exist, ErtsVersion});
                true ->
                    ok = ec_file:mkdir_p(LocalErts),
                    ok = ec_file:copy(ErtsDir, LocalErts, [recursive]),
                    ok = ec_file:remove(filename:join([LocalErts, "bin", "erl"])),
                    ok = file:write_file(filename:join([LocalErts, "bin", "erl"]), erl_script(ErtsVersion)),
                    case rlx_state:get(State, extended_start_script, false) of
                        true ->
                            ok = ec_file:copy(filename:join([Prefix, "bin", "start_clean.boot"]),
                                              filename:join([OutputDir, "bin", "start_clean.boot"])),
                            NodeToolFile = nodetool_contents(),
                            NodeTool = filename:join([LocalErts, "bin", "nodetool"]),
                            ok = file:write_file(NodeTool, NodeToolFile),
                            ok = file:change_mode(NodeTool, 8#755);
                        false ->
                            ok
                    end,
                    make_boot_script(State, Release, OutputDir, RelDir)
            end;
        _ ->
            make_boot_script(State, Release, OutputDir, RelDir)
    end.


-spec make_boot_script(rlx_state:t(), rlx_release:t(), file:name(), file:name()) ->
                              {ok, rlx_state:t()} | relx:error().
make_boot_script(State, Release, OutputDir, RelDir) ->
    Options = [{path, [RelDir | get_code_paths(Release, OutputDir)]},
               {outdir, RelDir},
               no_module_tests, silent],
    Name = erlang:atom_to_list(rlx_release:name(Release)),
    ReleaseFile = filename:join([RelDir, Name ++ ".rel"]),
    case make_script(Options,
                    fun(CorrectedOptions) ->
                            systools:make_script(Name, CorrectedOptions)
                    end) of
        ok ->
            rlx_log:error(rlx_state:log(State),
                          "release successfully created!"),
            make_relup(State, Release);
        error ->
            ?RLX_ERROR({release_script_generation_error, ReleaseFile});
        {ok, _, []} ->
            rlx_log:error(rlx_state:log(State),
                          "release successfully created!"),
            make_relup(State, Release);
        {ok,Module,Warnings} ->
            ?RLX_ERROR({release_script_generation_warn, Module, Warnings});
        {error,Module,Error} ->
            ?RLX_ERROR({release_script_generation_error, Module, Error})
    end.

-spec make_script([term()],
                  fun(([term()]) -> Res)) -> Res.
make_script(Options, RunFun) ->
    %% Erts 5.9 introduced a non backwards compatible option to
    %% erlang this takes that into account
    Erts = erlang:system_info(version),
    case ec_semver:gte(Erts, "5.9") of
        true ->
            RunFun([no_warn_sasl | Options]);
        _ ->
            RunFun(Options)
    end.

make_relup(State, Release) ->
    case rlx_state:action(State) of
        relup ->
            UpFrom =
                case rlx_state:upfrom(State) of
                    undefined ->
                        get_last_release(State, Release);
                    Vsn ->
                        get_up_release(State, Release, Vsn)
                end,
            case UpFrom of
                undefined ->
                    ?RLX_ERROR(no_upfrom_release_found);
                _ ->
                    make_upfrom_script(State, Release, UpFrom)
            end;
        _ ->
            {ok, State}
    end.

make_upfrom_script(State, Release, UpFrom) ->
    OutputDir = rlx_state:output_dir(State),
    Options = [{outdir, OutputDir},
               {path, get_code_paths(Release, OutputDir) ++
                    get_code_paths(UpFrom, OutputDir)},
               silent],
    CurrentRel = strip_rel(rlx_release:relfile(Release)),
    UpFromRel = strip_rel(rlx_release:relfile(UpFrom)),
    rlx_log:debug(rlx_state:log(State),
                  "systools:make_relup(~p, ~p, ~p, ~p)",
                  [CurrentRel, UpFromRel, UpFromRel, Options]),
    case make_script(Options,
                     fun(CorrectOptions) ->
                             systools:make_relup(CurrentRel, [UpFromRel], [UpFromRel], CorrectOptions)
                     end)  of
        ok ->
            rlx_log:error(rlx_state:log(State),
                          "relup from ~s to ~s successfully created!",
                          [UpFromRel, CurrentRel]),
            {ok, State};
        error ->
            ?RLX_ERROR({relup_script_generation_error, CurrentRel, UpFromRel});
        {ok, RelUp, _, []} ->
            rlx_log:error(rlx_state:log(State),
                          "relup successfully created!"),
            write_relup_file(State, Release, RelUp),
            {ok, State};
        {ok,_, Module,Warnings} ->
            ?RLX_ERROR({relup_script_generation_warn, Module, Warnings});
        {error,Module,Errors} ->
            ?RLX_ERROR({relupcript_generation_error, Module, Errors})
    end.

write_relup_file(State, Release, Relup) ->
    OutDir = release_output_dir(State, Release),
    RelName = rlx_util:to_string(rlx_release:name(Release)),
    RelupFile = filename:join(OutDir, RelName ++ ".relup"),
    ok = ec_file:write_term(RelupFile, Relup).

strip_rel(Name) ->
    rlx_util:to_string(filename:join(filename:dirname(Name),
                                     filename:basename(Name, ".rel"))).


get_up_release(State, Release, Vsn) ->
    Name = rlx_release:name(Release),
    try
        ec_dictionary:get({Name, Vsn}, rlx_state:realized_releases(State))
    catch
        throw:notfound ->
            undefined
    end.

get_last_release(State, Release) ->
    Releases0 = [Rel || {{_, _}, Rel} <- ec_dictionary:to_list(rlx_state:realized_releases(State))],
    Releases1 = lists:sort(fun(R1, R2) ->
                                  ec_semver:lte(rlx_release:vsn(R1),
                                                rlx_release:vsn(R2))
                          end, Releases0),
    Res = lists:foldl(fun(_Rel, R = {found, _}) ->
                              R;
                         (Rel, Prev) ->
                              case rlx_release:vsn(Rel) == rlx_release:vsn(Release)  of
                                  true ->
                                      {found, Prev};
                                  false ->
                                      Rel
                              end
                      end, undefined, Releases1),
    case Res of
        {found, R} ->
            R;
        Else ->
            Else
    end.

-spec release_output_dir(rlx_state:t(), rlx_release:t()) -> string().
release_output_dir(State, Release) ->
    OutputDir = rlx_state:output_dir(State),
    filename:join([OutputDir,
                   "releases",
                   rlx_release:canonical_name(Release)]).

%% @doc Generates the correct set of code paths for the system.
-spec get_code_paths(rlx_release:t(), file:name()) -> [file:name()].
get_code_paths(Release, OutDir) ->
    LibDir = filename:join(OutDir, "lib"),
    [filename:join([LibDir,
                    erlang:atom_to_list(rlx_app_info:name(App)) ++ "-" ++
                        rlx_app_info:vsn_as_string(App), "ebin"]) ||
        App <- rlx_release:application_details(Release)].

erl_script(ErtsVsn) ->
    [<<"#!/bin/sh
set -e

SCRIPT_DIR=`dirname $0`
ROOTDIR=`cd $SCRIPT_DIR/../../ && pwd`
BINDIR=$ROOTDIR/erts-">>, ErtsVsn, <<"/bin
EMU=beam
PROGNAME=`echo $0 | sed 's/.*\\///'`
export EMU
export ROOTDIR
export BINDIR
export PROGNAME
exec \"$BINDIR/erlexec\" ${1+\"$@\"}
">>].

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

cd $ROOTDIR

$BINDIR/erlexec $ERL_OPTS $SYS_CONFIG -boot $REL_DIR/$REL_NAME $@">>].

extended_bin_file_contents(RelName, RelVsn, ErtsVsn, ErlOpts) ->
    [<<"#!/bin/sh

set -e

SCRIPT_DIR=`dirname $0`
RELEASE_ROOT_DIR=`cd $SCRIPT_DIR/.. && pwd`
REL_NAME=">>, RelName, <<"
REL_VSN=">>, RelVsn, <<"
ERTS_VSN=">>, ErtsVsn, <<"
REL_DIR=$RELEASE_ROOT_DIR/releases/$REL_NAME-$REL_VSN
ERL_OPTS=">>, ErlOpts, <<"
PIPE_DIR=/tmp/$REL_DIR/

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

# Use $CWD/vm.args if exists, otherwise releases/APP_VSN/vm.args, or else etc/vm.args
if [ -e \"$CALLER_DIR/vm.args\" ]; then
    VMARGS_PATH=$CALLER_DIR/vm.args
    USE_DIR=$CALLER_DIR
else
    USE_DIR=$REL_DIR
    if [ -e \"$REL_DIR/vm.args\" ]; then
        VMARGS_PATH=\"$REL_DIR/vm.args\"
    else
        VMARGS_PATH=\"$REL_DIR/vm.args\"
    fi
fi

RUNNER_LOG_DIR=$USE_DIR/log
# Make sure log directory exists
mkdir -p $RUNNER_LOG_DIR

# Use releases/VSN/sys.config if it exists otherwise use etc/app.config
if [ -e \"$USE_DIR/sys.config\" ]; then
    CONFIG_PATH=\"$USE_DIR/sys.config\"
else
    if [ -e \"$REL_DIR/sys.config\" ]; then
        CONFIG_PATH=\"$REL_DIR/sys.config\"
    else
        CONFIG_PATH=\"$REL_DIR/app.config\"
    fi
fi

# Extract the target node name from node.args
NAME_ARG=`egrep '^-s?name' $VMARGS_PATH`
if [ -z \"$NAME_ARG\" ]; then
    echo \"vm.args needs to have either -name or -sname parameter.\"
    exit 1
fi

# Extract the name type and name from the NAME_ARG for REMSH
REMSH_TYPE=`echo $NAME_ARG | awk '{print $1}'`
REMSH_NAME=`echo $NAME_ARG | awk '{print $2}'`

# Note the `date +%s`, used to allow multiple remsh to the same node transparently
REMSH_NAME_ARG=\"$REMSH_TYPE remsh`date +%s`@`echo $REMSH_NAME | awk -F@ '{print $2}'`\"
REMSH_REMSH_ARG=\"-remsh $REMSH_NAME\"

# Extract the target cookie
COOKIE_ARG=`grep '^-setcookie' $VMARGS_PATH`
if [ -z \"$COOKIE_ARG\" ]; then
    echo \"vm.args needs to have a -setcookie parameter.\"
    exit 1
fi

# Setup remote shell command to control node
REMSH=\"$ERTS_PATH/erl $REMSH_NAME_ARG $REMSH_REMSH_ARG $COOKIE_ARG\"

find_erts_dir
find_sys_config
export ROOTDIR=$RELEASE_ROOT_DIR
export BINDIR=$ERTS_DIR/bin
export EMU=beam
export PROGNAME=erl
export LD_LIBRARY_PATH=$ERTS_DIR/lib

cd $ROOTDIR

# Setup command to control the node
NODETOOL=\"$ERTS_DIR/bin/escript $ERTS_DIR/bin/nodetool $NAME_ARG $COOKIE_ARG\"

# Check the first argument for instructions
case \"$1\" in
    start|start_boot)

        # Make sure there is not already a node running
        #RES=`$NODETOOL ping`
        #if [ \"$RES\" = \"pong\" ]; then
        #    echo \"Node is already running!\"
        #    exit 1
        #fi
        case \"$1\" in
            start)
                shift
                START_OPTION=\"console\"
                HEART_OPTION=\"start\"
                ;;
            start_boot)
                shift
                START_OPTION=\"console_boot\"
                HEART_OPTION=\"start_boot\"
                ;;
        esac
        RUN_PARAM=$(printf \"'%s' \" \"$@\")
        HEART_COMMAND=\"$SCRIPT_DIR/bin/$REL_NAME $HEART_OPTION $RUN_PARAM\"
        export HEART_COMMAND
        mkdir -p $PIPE_DIR
        $ERTS_DIR/bin/run_erl -daemon $PIPE_DIR $RUNNER_LOG_DIR \"exec $RELEASE_ROOT_DIR/bin/$REL_NAME $START_OPTION $RUN_PARAM\" 2>&1
        ;;

    stop)
        # Wait for the node to completely stop...
        case `uname -s` in
            Linux|Darwin|FreeBSD|DragonFly|NetBSD|OpenBSD)
                # PID COMMAND
                PID=`ps ax -o pid= -o command=|
                    grep \"$SCRIPT_DIR/.*/[b]eam\"|awk '{print $1}'`
                ;;
            SunOS)
                # PID COMMAND
                PID=`ps -ef -o pid= -o args=|
                    grep \"$SCRIPT_DIR/.*/[b]eam\"|awk '{print $1}'`
                ;;
            CYGWIN*)
                # UID PID PPID TTY STIME COMMAND
                PID=`ps -efW|grep \"$SCRIPT_DIR/.*/[b]eam\"|awk '{print $2}'`
                ;;
        esac
        $NODETOOL stop
        ES=$?
        if [ \"$ES\" -ne 0 ]; then
            exit $ES
        fi
        while `kill -0 $PID 2>/dev/null`;
        do
            sleep 1
        done
        ;;

    restart)
        ## Restart the VM without exiting the process
        $NODETOOL restart
        ES=$?
        if [ \"$ES\" -ne 0 ]; then
            exit $ES
        fi
        ;;

    reboot)
        ## Restart the VM completely (uses heart to restart it)
        $NODETOOL reboot
        ES=$?
        if [ \"$ES\" -ne 0 ]; then
            exit $ES
        fi
        ;;

    ping)
        ## See if the VM is alive
        $NODETOOL ping
        ES=$?
        if [ \"$ES\" -ne 0 ]; then
            exit $ES
        fi
        ;;

    attach)
        # Make sure a node IS running
        RES=`$NODETOOL ping`
        ES=$?
        if [ \"$ES\" -ne 0 ]; then
            echo \"Node is not running!\"
            exit $ES
        fi

        shift
        exec $ERTS_DIR/bin/to_erl $PIPE_DIR
        ;;

    remote_console)
        # Make sure a node IS running
        RES=`$NODETOOL ping`
        ES=$?
        if [ \"$ES\" -ne 0 ]; then
            echo \"Node is not running!\"
            exit $ES
        fi

        shift
        exec $REMSH
        ;;

    upgrade)
        if [ -z \"$2\" ]; then
            echo \"Missing upgrade package argument\"
            echo \"Usage: $REL_NAME upgrade {package base name}\"
            echo \"NOTE {package base name} MUST NOT include the .tar.gz suffix\"
            exit 1
        fi

        # Make sure a node IS running
        RES=`$NODETOOL ping`
        ES=$?
        if [ \"$ES\" -ne 0 ]; then
            echo \"Node is not running!\"
            exit $ES
        fi

        node_name=`echo $NAME_ARG | awk '{print $2}'`
        erlang_cookie=`echo $COOKIE_ARG | awk '{print $2}'`

        $ERTS_DIR/bin/escript $SCRIPT_DIR/bin/install_upgrade.escript $node_name $erlang_cookie $2
        ;;

    console|console_clean|console_boot)
        # .boot file typically just $REL_NAME (ie, the app name)
        # however, for debugging, sometimes start_clean.boot is useful.
        # For e.g. 'setup', one may even want to name another boot script.
        case \"$1\" in
            console)        BOOTFILE=$REL_NAME ;;
            console_clean)  BOOTFILE=start_clean ;;
            console_boot)
                shift
                BOOTFILE=\"$1\"
                shift
                ;;
        esac
        # Setup beam-required vars
        ROOTDIR=$RELEASE_ROOT_DIR
        BINDIR=$RELEASE_ROOT_DIR/erts-$ERTS_VSN/bin
        EMU=beam
        PROGNAME=`echo $0 | sed 's/.*\\///'`
        CMD=\"$BINDIR/erlexec -boot $REL_DIR/$BOOTFILE -mode embedded -config $CONFIG_PATH -args_file $VMARGS_PATH\"
        export EMU
        export ROOTDIR
        export BINDIR
        export PROGNAME

        # Dump environment info for logging purposes
        echo \"Exec: $CMD\" -- ${1+\"$@\"}
        echo \"Root: $ROOTDIR\"

        # Log the startup
        logger -t \"$REL_NAME[$$]\" \"Starting up\"

        # Start the VM
        exec $CMD -- ${1+\"$@\"}
        ;;

    foreground)
        # start up the release in the foreground for use by runit
        # or other supervision services

        BOOTFILE=$REL_NAME
        FOREGROUNDOPTIONS=\"-noinput +Bd\"

        # Setup beam-required vars
        ROOTDIR=$RELEASE_ROOT_DIR
        BINDIR=$RELEASE_ROOT_DIR/erts-$ERTS_VSN/bin
        EMU=beam
        PROGNAME=`echo $0 | sed 's/.*///'`
        CMD=\"$BINDIR/erlexec $FOREGROUNDOPTIONS -boot $REL_DIR/$BOOTFILE -config $CONFIG_PATH -args_file $VMARGS_PATH\"
        export EMU
        export ROOTDIR
        export BINDIR
        export PROGNAME

        # Dump environment info for logging purposes
        echo \"Exec: $CMD\" -- ${1+\"$@\"}
        echo \"Root: $ROOTDIR\"

        # Start the VM
        exec $CMD -- ${1+\"$@\"}
        ;;
    *)
        echo \"Usage: $REL_NAME {start|start_boot <file>|foreground|stop|restart|reboot|ping|console|console_clean|console_boot <file>|attach|remote_console|upgrade}\"
        exit 1
        ;;
esac

exit 0">>].

nodetool_contents() ->
    [<<"%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% nodetool: Helper Script for interacting with live nodes
%%
%% -------------------------------------------------------------------

main(Args) ->
    ok = start_epmd(),
    %% Extract the args
    {RestArgs, TargetNode} = process_args(Args, [], undefined),

    %% See if the node is currently running  -- if it's not, we'll bail
    case {net_kernel:hidden_connect_node(TargetNode), net_adm:ping(TargetNode)} of
        {true, pong} ->
            ok;
        {_, pang} ->
            io:format(\"Node ~p not responding to pings.\n\", [TargetNode]),
            halt(1)
    end,

    case RestArgs of
        [\"ping\"] ->
            %% If we got this far, the node already responsed to a ping, so just dump
            %% a \"pong\"
            io:format(\"pong\n\");
        [\"stop\"] ->
            io:format(\"~p\n\", [rpc:call(TargetNode, init, stop, [], 60000)]);
        [\"restart\"] ->
            io:format(\"~p\n\", [rpc:call(TargetNode, init, restart, [], 60000)]);
        [\"reboot\"] ->
            io:format(\"~p\n\", [rpc:call(TargetNode, init, reboot, [], 60000)]);
        [\"rpc\", Module, Function | RpcArgs] ->
            case rpc:call(TargetNode, list_to_atom(Module), list_to_atom(Function),
                          [RpcArgs], 60000) of
                ok ->
                    ok;
                {badrpc, Reason} ->
                    io:format(\"RPC to ~p failed: ~p\n\", [TargetNode, Reason]),
                    halt(1);
                _ ->
                    halt(1)
            end;
        [\"rpcterms\", Module, Function, ArgsAsString] ->
            case rpc:call(TargetNode, list_to_atom(Module), list_to_atom(Function),
                          consult(ArgsAsString), 60000) of
                {badrpc, Reason} ->
                    io:format(\"RPC to ~p failed: ~p\n\", [TargetNode, Reason]),
                    halt(1);
                Other ->
                    io:format(\"~p\n\", [Other])
            end;
        Other ->
            io:format(\"Other: ~p\n\", [Other]),
            io:format(\"Usage: nodetool {ping|stop|restart|reboot}\n\")
    end,
    net_kernel:stop().

process_args([], Acc, TargetNode) ->
    {lists:reverse(Acc), TargetNode};
process_args([\"-setcookie\", Cookie | Rest], Acc, TargetNode) ->
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    process_args(Rest, Acc, TargetNode);
process_args([\"-name\", TargetName | Rest], Acc, _) ->
    ThisNode = append_node_suffix(TargetName, \"_maint_\"),
    {ok, _} = net_kernel:start([ThisNode, longnames]),
    process_args(Rest, Acc, nodename(TargetName));
process_args([\"-sname\", TargetName | Rest], Acc, _) ->
    ThisNode = append_node_suffix(TargetName, \"_maint_\"),
    {ok, _} = net_kernel:start([ThisNode, shortnames]),
    process_args(Rest, Acc, nodename(TargetName));
process_args([Arg | Rest], Acc, Opts) ->
    process_args(Rest, [Arg | Acc], Opts).


start_epmd() ->
    [] = os:cmd(epmd_path() ++ \" -daemon\"),
    ok.

epmd_path() ->
    ErtsBinDir = filename:dirname(escript:script_name()),
    Name = \"epmd\",
    case os:find_executable(Name, ErtsBinDir) of
        false ->
            case os:find_executable(Name) of
                false ->
                    io:format(\"Could not find epmd.~n\"),
                    halt(1);
                GlobalEpmd ->
                    GlobalEpmd
            end;
        Epmd ->
            Epmd
    end.


nodename(Name) ->
    case string:tokens(Name, \"@\") of
        [_Node, _Host] ->
            list_to_atom(Name);
        [Node] ->
            [_, Host] = string:tokens(atom_to_list(node()), \"@\"),
            list_to_atom(lists:concat([Node, \"@\", Host]))
    end.

append_node_suffix(Name, Suffix) ->
    case string:tokens(Name, \"@\") of
        [Node, Host] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid(), \"@\", Host]));
        [Node] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid()]))
    end.


%%
%% Given a string or binary, parse it into a list of terms, ala file:consult/0
%%
consult(Str) when is_list(Str) ->
    consult([], Str, []);
consult(Bin) when is_binary(Bin)->
    consult([], binary_to_list(Bin), []).

consult(Cont, Str, Acc) ->
    case erl_scan:tokens(Cont, Str, 0) of
        {done, Result, Remaining} ->
            case Result of
                {ok, Tokens, _} ->
                    {ok, Term} = erl_parse:parse_term(Tokens),
                    consult([], Remaining, [Term | Acc]);
                {eof, _Other} ->
                    lists:reverse(Acc);
                {error, Info, _} ->
                    {error, Info}
            end;
        {more, Cont1} ->
            consult(Cont1, eof, Acc)
    end.">>].

vm_args_file(RelName) ->
    [<<"## Name of the node
-name ">>, RelName, <<"@127.0.0.1

## Cookie for distributed erlang
-setcookie ">>, RelName, <<"

## Heartbeat management; auto-restarts VM if it dies or becomes unresponsive
## (Disabled by default..use with caution!)
##-heart

## Enable kernel poll and a few async threads
##+K true
##+A 5

## Increase number of concurrent ports/sockets
##-env ERL_MAX_PORTS 4096

## Tweak GC to run more often
##-env ERL_FULLSWEEP_AFTER 10">>].
