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

-include("relx.hrl").

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
    print_dev_mode(State),
    {RelName, RelVsn} = rlx_state:default_configured_release(State),
    Release = rlx_state:get_realized_release(State, RelName, RelVsn),
    OutputDir = rlx_state:output_dir(State),
    case create_output_dir(OutputDir) of
        ok ->
            case rlx_release:realized(Release) of
                true ->
                    run_actions(State, Release, OutputDir);
                false ->
                    ?RLX_ERROR({unresolved_release, RelName, RelVsn})
            end;
        Error ->
            Error
    end.

do(release, State, Release, OutputDir) ->
    copy_app_directories_to_output(State, Release, OutputDir);
do(relup, State, Release, _OutputDir) ->
    RelName = rlx_release:name(Release),
    RelVsn = rlx_release:vsn(Release),
    Release0 = rlx_state:get_realized_release(State, RelName, RelVsn),
    make_relup(State, Release0);
do(tar, State, Release, OutputDir) ->
    make_tar(State, Release, OutputDir).

run_actions(State, Release, OutputDir) ->
    run_actions(State, Release, OutputDir, rlx_state:actions(State), [release, relup, tar]).

run_actions(State, _Release, _OutputDir, _Actions, []) ->
    {ok, State};
run_actions(State, Release, OutputDir, Actions, [H | T]) ->
    case lists:member(H, Actions) of
        true ->
            case do(H, State, Release, OutputDir) of
                {ok, NewState} ->
                    run_actions(NewState, Release, OutputDir, Actions, T);
                Error ->
                    Error
            end;
        false ->
            run_actions(State, Release, OutputDir, Actions, T)
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
     rlx_util:indent(2), Module:format_warning(Warnings)];
format_error({unable_to_create_output_dir, OutputDir}) ->
    io_lib:format("Unable to create output directory (possible permissions issue): ~s",
                  [OutputDir]);
format_error({release_script_generation_error, Module, Errors}) ->
    ["Errors generating release \n",
     rlx_util:indent(2), Module:format_error(Errors)];
format_error({relup_generation_error, CurrentName, UpFromName}) ->
    io_lib:format("Unknown internal release error generating the relup from ~s to ~s",
                  [UpFromName, CurrentName]);
format_error({relup_generation_warning, Module, Warnings}) ->
    ["Warnings generating relup \s",
     rlx_util:indent(2), Module:format_warning(Warnings)];
format_error({no_upfrom_release_found, undefined}) ->
    io_lib:format("No earlier release for relup found", []);
format_error({no_upfrom_release_found, Vsn}) ->
    io_lib:format("Upfrom release version (~s) for relup not found", [Vsn]);
format_error({relup_script_generation_error,
              {relup_script_generation_error, systools_relup,
               {missing_sasl, _}}}) ->
    "Unfortunately, due to requirements in systools, you need to have the sasl application "
        "in both the current release and the release to upgrade from.";
format_error({relup_script_generation_error, Module, Errors}) ->
    ["Errors generating relup \n",
     rlx_util:indent(2), Module:format_error(Errors)];
format_error({unable_to_make_symlink, AppDir, TargetDir, Reason}) ->
    io_lib:format("Unable to symlink directory ~s to ~s because \n~s~s",
                  [AppDir, TargetDir, rlx_util:indent(2),
                   file:format_error(Reason)]);
format_error({tar_unknown_generation_error, Module, Vsn}) ->
    io_lib:format("Tarball generation error of ~s ~s",
                  [Module, Vsn]);
format_error({tar_generation_warn, Module, Warnings}) ->
    io_lib:format("Tarball generation warnings for ~p : ~p",
                  [Module, Warnings]);
format_error({tar_generation_error, Module, Errors}) ->
    io_lib:format("Tarball generation error for ~p reason ~p",
                  [Module, Errors]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
print_dev_mode(State) ->
    case rlx_state:dev_mode(State) of
        true ->
            ec_cmd_log:info(rlx_state:log(State),
                            "Dev mode enabled, release will be symlinked");
        false ->
            ok
    end.

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
    IncludeSrc = rlx_state:include_src(State),
    Apps = prepare_applications(State, rlx_release:application_details(Release)),
    Result = lists:filter(fun({error, _}) ->
                                   true;
                              (_) ->
                                   false
                           end,
                          lists:flatten(ec_plists:map(fun(App) ->
                                                              copy_app(LibDir, App, IncludeSrc)
                                                      end, Apps))),
    case Result of
        [E | _] ->
            E;
        [] ->
            create_release_info(State, Release, OutputDir)
    end.

prepare_applications(State, Apps) ->
    case rlx_state:dev_mode(State) of
        true ->
            [rlx_app_info:link(App, true) || App <- Apps];
        false ->
            Apps
    end.

copy_app(LibDir, App, IncludeSrc) ->
    AppName = erlang:atom_to_list(rlx_app_info:name(App)),
    AppVsn = rlx_app_info:original_vsn(App),
    AppDir = rlx_app_info:dir(App),
    TargetDir = filename:join([LibDir, AppName ++ "-" ++ AppVsn]),
    if
        AppDir == TargetDir ->
            %% No need to do anything here, discover found something already in
            %% a release dir
            ok;
        true ->
            copy_app(App, AppDir, TargetDir, IncludeSrc)
    end.

copy_app(App, AppDir, TargetDir, IncludeSrc) ->
    remove_symlink_or_directory(TargetDir),
    case rlx_app_info:link(App) of
        true ->
            link_directory(AppDir, TargetDir);
        false ->
            copy_directory(AppDir, TargetDir, IncludeSrc)
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

copy_directory(AppDir, TargetDir, IncludeSrc) ->
    ec_plists:map(fun(SubDir) ->
                          copy_dir(AppDir, TargetDir, SubDir)
                  end, ["ebin",
                        "include",
                        "priv",
                        "README",
                        "LICENSE" |
                        case IncludeSrc of
                            true ->
                                ["src",
                                 "c_src"];
                            false ->
                                []
                        end]).

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
    RelName = atom_to_list(rlx_release:name(Release0)),
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
    {OsFamily, _OsName} = os:type(),
    StartFile = case rlx_state:get(State, extended_start_script, false) of
                    false ->
                        bin_file_contents(OsFamily, RelName, RelVsn,
                                          rlx_release:erts(Release),
                                          ErlOpts);
                    true ->
                        case rlx_state:get(State, extended_start_script, false) of
                            true ->
                                Prefix = code:root_dir(),
                                DstFile = filename:join([BinDir, "start_clean.boot"]),
                                %% Explicitly remove before cp, since it is 0444 mode
                                ec_file:remove(DstFile),
                                ok = ec_file:copy(filename:join([Prefix, "bin", "start_clean.boot"]),
                                                  DstFile),
                                NodeToolFile = nodetool_contents(),
                                InstallUpgradeFile = install_upgrade_escript_contents(),
                                NodeTool = filename:join([BinDir, "nodetool"]),
                                InstallUpgrade = filename:join([BinDir, "install_upgrade.escript"]),
                                ok = file:write_file(NodeTool, NodeToolFile),
                                ok = file:write_file(InstallUpgrade, InstallUpgradeFile);
                            false ->
                                ok
                        end,
                        extended_bin_file_contents(OsFamily, RelName, RelVsn, rlx_release:erts(Release), ErlOpts)
                end,
    %% We generate the start script by default, unless the user
    %% tells us not too
    case rlx_state:get(State, generate_start_script, true) of
        false ->
            ok;
        _ ->
            VsnRelStartFile = case OsFamily of
                unix -> VsnRel;
                win32 -> string:concat(VsnRel, ".cmd")
            end,
            ok = file:write_file(VsnRelStartFile, StartFile),
            ok = file:change_mode(VsnRelStartFile, 8#777),
            BareRelStartFile = case OsFamily of
                unix -> BareRel;
                win32 -> string:concat(BareRel, ".cmd")
            end,
            ok = file:write_file(BareRelStartFile, StartFile),
            ok = file:change_mode(BareRelStartFile, 8#777)
    end,
    ReleasesDir = filename:join(OutputDir, "releases"),
    generate_start_erl_data_file(Release, ReleasesDir),
    copy_or_generate_vmargs_file(State, Release, RelDir),
    copy_or_generate_sys_config_file(State, RelDir),
    include_erts(State, Release, OutputDir, RelDir).

%% @doc generate a start_erl.data file
-spec generate_start_erl_data_file(rlx_release:t(), file:name()) ->
                                   ok | relx:error().
generate_start_erl_data_file(Release, ReleasesDir) ->
    ErtsVersion = rlx_release:erts(Release),
    ReleaseVersion = rlx_release:vsn(Release),
    Data = ErtsVersion ++ " " ++ ReleaseVersion,
    ok = file:write_file(filename:join(ReleasesDir, "start_erl.data"), Data).

%% @doc copy vm.args or generate one to releases/VSN/vm.args
-spec copy_or_generate_vmargs_file(rlx_state:t(), rlx_release:t(), file:name()) ->
                                              {ok, rlx_state:t()} | relx:error().

copy_or_generate_vmargs_file(State, Release, RelDir) ->
    RelVmargsPath = filename:join([RelDir, "vm.args"]),
    case rlx_state:vm_args(State) of
        undefined ->
            RelName = erlang:atom_to_list(rlx_release:name(Release)),
            unless_exists_write_default(RelVmargsPath, vm_args_file(RelName));
        ArgsPath ->
            case filelib:is_regular(ArgsPath) of
                false ->
                    ?RLX_ERROR({vmargs_does_not_exist, ArgsPath});
                true ->
                    copy_or_symlink_config_file(State, ArgsPath, RelVmargsPath)
            end
    end.

%% @doc copy config/sys.config or generate one to releases/VSN/sys.config
-spec copy_or_generate_sys_config_file(rlx_state:t(), file:name()) ->
                                              {ok, rlx_state:t()} | relx:error().
copy_or_generate_sys_config_file(State, RelDir) ->
    RelSysConfPath = filename:join([RelDir, "sys.config"]),
    case rlx_state:sys_config(State) of
        undefined ->
            unless_exists_write_default(RelSysConfPath, sys_config_file());
        ConfigPath ->
            case filelib:is_regular(ConfigPath) of
                false ->
                    ?RLX_ERROR({config_does_not_exist, ConfigPath});
                true ->
                    copy_or_symlink_config_file(State, ConfigPath, RelSysConfPath)
            end
    end.

%% @doc copy config/sys.config or generate one to releases/VSN/sys.config
-spec copy_or_symlink_config_file(rlx_state:t(), file:name(), file:name()) ->
                                         ok.
copy_or_symlink_config_file(State, ConfigPath, RelConfPath) ->
    ensure_not_exist(RelConfPath),
    case rlx_state:dev_mode(State) of
        true ->
            ok = file:make_symlink(ConfigPath, RelConfPath);
        _ ->
            ok = ec_file:copy(ConfigPath, RelConfPath)
    end.

%% @doc Optionally add erts directory to release, if defined.
-spec include_erts(rlx_state:t(), rlx_release:t(),  file:name(), file:name()) ->
                          {ok, rlx_state:t()} | relx:error().
include_erts(State, Release, OutputDir, RelDir) ->
    Prefix = case rlx_state:get(State, include_erts, true) of
                 false ->
                     false;
                 true ->
                     code:root_dir();
                 P ->
                     filename:absname(P)
    end,

    case Prefix of
        false ->
            make_boot_script(State, Release, OutputDir, RelDir);
        _ ->
            ec_cmd_log:info(rlx_state:log(State),
                            "Including Erts from ~s~n", [Prefix]),
            ErtsVersion = rlx_release:erts(Release),
            ErtsDir = filename:join([Prefix, "erts-" ++ ErtsVersion]),
            LocalErts = filename:join([OutputDir, "erts-" ++ ErtsVersion]),
            {OsFamily, _OsName} = os:type(),
            case filelib:is_dir(ErtsDir) of
                false ->
                    ?RLX_ERROR({specified_erts_does_not_exist, ErtsVersion});
                true ->
                    ok = ec_file:mkdir_p(LocalErts),
                    ok = ec_file:copy(ErtsDir, LocalErts, [recursive]),
                    case OsFamily of
                        unix ->
                            Erl = filename:join([LocalErts, "bin", "erl"]),
                            ok = ec_file:remove(Erl),
                            ok = file:write_file(Erl, erl_script(ErtsVersion)),
                            ok = file:change_mode(Erl, 8#755);
                        win32 ->
                            ErlIni = filename:join([LocalErts, "bin", "erl.ini"]),
                            ok = ec_file:remove(ErlIni),
                            ok = file:write_file(ErlIni, erl_ini(OutputDir, ErtsVersion))
                    end,
                    case rlx_state:get(State, extended_start_script, false) of
                        true ->
                            ok = ec_file:remove(filename:join([OutputDir, "bin", "start_clean.boot"])),
                            ok = ec_file:copy(filename:join([Prefix, "bin", "start_clean.boot"]),
                                              filename:join([OutputDir, "bin", "start_clean.boot"])),
                            NodeToolFile = nodetool_contents(),
                            InstallUpgradeFile = install_upgrade_escript_contents(),
                            NodeTool = filename:join([LocalErts, "bin", "nodetool"]),
                            InstallUpgrade = filename:join([LocalErts, "bin", "install_upgrade.escript"]),
                            ok = file:write_file(NodeTool, NodeToolFile),
                            ok = file:write_file(InstallUpgrade, InstallUpgradeFile),
                            ok = file:change_mode(NodeTool, 8#755),
                            ok = file:change_mode(InstallUpgrade, 8#755);
                        false ->
                            ok
                    end,
                    make_boot_script(State, Release, OutputDir, RelDir)
            end
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
            ec_cmd_log:info(rlx_state:log(State),
                             "release successfully created!"),
            create_RELEASES(OutputDir, ReleaseFile),
            {ok, State};
        error ->
            ?RLX_ERROR({release_script_generation_error, ReleaseFile});
        {ok, _, []} ->
            ec_cmd_log:info(rlx_state:log(State),
                          "release successfully created!"),
            create_RELEASES(OutputDir, ReleaseFile),
            {ok, State};
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
    Vsn = rlx_state:upfrom(State),
    UpFrom =
        case Vsn of
            undefined ->
                get_last_release(State, Release);
            Vsn ->
                get_up_release(State, Release, Vsn)
        end,
    case UpFrom of
        undefined ->
            ?RLX_ERROR({no_upfrom_release_found, Vsn});
        _ ->
            make_upfrom_script(State, Release, UpFrom)
    end.

make_tar(State, Release, OutputDir) ->
    Name = atom_to_list(rlx_release:name(Release)),
    Vsn = rlx_release:vsn(Release),
    ErtsVersion = rlx_release:erts(Release),
    Opts = [{path, [filename:join([OutputDir, "lib", "*", "ebin"])]},
           {outdir, OutputDir} |
            case rlx_state:get(State, include_erts, true) of
                true ->
                    Prefix = code:root_dir(),
                    ErtsDir = filename:join([Prefix]),
                    [{erts, ErtsDir}];
                false ->
                    [];
                Prefix ->
                    ErtsDir = filename:join([Prefix]),
                    [{erts, ErtsDir}]
            end],
    case systools:make_tar(filename:join([OutputDir, "releases", Vsn, Name]),
                           Opts) of
        ok ->
            TempDir = ec_file:insecure_mkdtemp(),
            try
                update_tar(State, TempDir, OutputDir, Name, Vsn, ErtsVersion)
            catch
                E:R ->
                    ec_file:remove(TempDir, [recursive]),
                    ?RLX_ERROR({tar_generation_error, E, R})
            end;
        {ok, Module, Warnings} ->
            ?RLX_ERROR({tar_generation_warn, Module, Warnings});
        error ->
            ?RLX_ERROR({tar_unknown_generation_error, Name, Vsn});
        {error, Module, Errors} ->
            ?RLX_ERROR({tar_generation_error, Module, Errors})
    end.

update_tar(State, TempDir, OutputDir, Name, Vsn, ErtsVersion) ->
    TarFile = filename:join(OutputDir, Name++"-"++Vsn++".tar.gz"),
    file:rename(filename:join(OutputDir, Name++".tar.gz"), TarFile),
    erl_tar:extract(TarFile, [{cwd, TempDir}, compressed]),
    ok =
        erl_tar:create(TarFile,
                       [{"lib", filename:join(TempDir, "lib")},
                        {"releases", filename:join(TempDir, "releases")},
                        {filename:join(["releases", "RELEASES"]),
                         filename:join([OutputDir, "releases", "RELEASES"])},
                        {filename:join(["releases", Vsn, "vm.args"]),
                         filename:join([OutputDir, "releases", Vsn, "vm.args"])},
                        {"bin", filename:join([OutputDir, "bin"])} |
                        case rlx_state:get(State, include_erts, true) of
                            false ->
                                [];
                            _ ->
                                [{"erts-"++ErtsVersion, filename:join(OutputDir, "erts-"++ErtsVersion)}]
                        end], [compressed]),
    ec_cmd_log:info(rlx_state:log(State),
                 "tarball ~s successfully created!~n", [TarFile]),
    ec_file:remove(TempDir, [recursive]),
    {ok, State}.

create_RELEASES(OutputDir, ReleaseFile) ->
    {ok, OldCWD} = file:get_cwd(),
    file:set_cwd(OutputDir),
    release_handler:create_RELEASES("./",
                                    "releases",
                                    ReleaseFile,
                                    []),
    file:set_cwd(OldCWD).

make_upfrom_script(State, Release, UpFrom) ->
    OutputDir = rlx_state:output_dir(State),
    Options = [{outdir, OutputDir},
               {path, get_code_paths(Release, OutputDir) ++
                   get_code_paths(UpFrom, OutputDir)},
               silent],
    CurrentRel = strip_rel(rlx_release:relfile(Release)),
    UpFromRel = strip_rel(rlx_release:relfile(UpFrom)),
    ec_cmd_log:debug(rlx_state:log(State),
                  "systools:make_relup(~p, ~p, ~p, ~p)",
                  [CurrentRel, UpFromRel, UpFromRel, Options]),
    case make_script(Options,
                     fun(CorrectOptions) ->
                             systools:make_relup(CurrentRel, [UpFromRel], [UpFromRel], CorrectOptions)
                     end)  of
        ok ->
            ec_cmd_log:error(rlx_state:log(State),
                          "relup from ~s to ~s successfully created!",
                          [UpFromRel, CurrentRel]),
            {ok, State};
        error ->
            ?RLX_ERROR({relup_script_generation_error, CurrentRel, UpFromRel});
        {ok, RelUp, _, []} ->
            ec_cmd_log:error(rlx_state:log(State),
                          "relup successfully created!"),
            write_relup_file(State, Release, RelUp),
            {ok, State};
        {ok,_, Module,Warnings} ->
            ?RLX_ERROR({relup_script_generation_warn, Module, Warnings});
        {error,Module,Errors} ->
            ?RLX_ERROR({relup_script_generation_error, Module, Errors})
    end.

write_relup_file(State, Release, Relup) ->
    OutDir = release_output_dir(State, Release),
    RelupFile = filename:join(OutDir, "relup"),
    ok = ec_file:write_term(RelupFile, Relup).

strip_rel(Name) ->
    rlx_util:to_string(filename:join(filename:dirname(Name),
                                     filename:basename(Name, ".rel"))).

get_up_release(State, Release, Vsn) ->
    Name = rlx_release:name(Release),
    try
        ec_dictionary:get({Name, Vsn}, rlx_state:realized_releases(State))
    catch
        throw:not_found ->
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
                   rlx_release:vsn(Release)]).

%% @doc Generates the correct set of code paths for the system.
-spec get_code_paths(rlx_release:t(), file:name()) -> [file:name()].
get_code_paths(Release, OutDir) ->
    LibDir = filename:join(OutDir, "lib"),
    [filename:join([LibDir,
                    erlang:atom_to_list(rlx_app_info:name(App)) ++ "-" ++
                        rlx_app_info:original_vsn(App), "ebin"]) ||
        App <- rlx_release:application_details(Release)].

unless_exists_write_default(Path, File) ->
    case ec_file:exists(Path) of
        true ->
            ok;
        false ->
            ok = file:write_file(Path, File)
    end.

-spec ensure_not_exist(file:name()) -> ok.
ensure_not_exist(RelConfPath) ->
    case ec_file:exists(RelConfPath) of
        false ->
            ok;
        _ ->
            ec_file:remove(RelConfPath)
    end.

erl_script(ErtsVsn) ->
    render(erl_script_dtl, [{erts_vsn, ErtsVsn}]).

bin_file_contents(OsFamily, RelName, RelVsn, ErtsVsn, ErlOpts) ->
    Template = case OsFamily of
        unix -> bin_dtl;
        win32 -> bin_windows_dtl
    end,
    render(Template, [{rel_name, RelName}, {rel_vsn, RelVsn},
                      {erts_vsn, ErtsVsn}, {erl_opts, ErlOpts}]).

extended_bin_file_contents(OsFamily, RelName, RelVsn, ErtsVsn, ErlOpts) ->
    Template = case OsFamily of
        unix -> extended_bin_dtl;
        win32 -> extended_bin_windows_dtl
    end,
    render(Template, [{rel_name, RelName}, {rel_vsn, RelVsn},
                      {erts_vsn, ErtsVsn}, {erl_opts, ErlOpts}]).

erl_ini(OutputDir, ErtsVsn) ->
    ErtsDirName = string:concat("erts-", ErtsVsn),
    BinDir = filename:join([OutputDir, ErtsDirName, bin]),
    render(erl_ini_dtl, [{bin_dir, BinDir}, {output_dir, OutputDir}]).

install_upgrade_escript_contents() ->
    render(install_upgrade_escript_dtl).

nodetool_contents() ->
    render(nodetool_dtl).

sys_config_file() ->
    render(sys_config_dtl).

vm_args_file(RelName) ->
    render(vm_args_dtl, [{rel_name, RelName}]).

render(Template) ->
    render(Template, []).

render(Template, Data) ->
    {ok, Rendered} = Template:render(Data),
    Rendered.
