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

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/2]).

-include("relx.hrl").

-define(PROVIDER, release).
-define(DEPS, [overlay]).

%%============================================================================
%% API
%%============================================================================
-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    State1 = rlx_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {bare, false},
                                                             {deps, ?DEPS},
                                                             {example, "release"},
                                                             {short_desc, ""},
                                                             {desc, ""},
                                                             {opts, []}])),
    {ok, State1}.

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
                    case copy_app_directories_to_output(State, Release, OutputDir) of
                        {ok, State1} ->
                            case rlx_state:debug_info(State1) =:= strip
                                andalso rlx_state:dev_mode(State1) =:= false of
                                true ->
                                    case beam_lib:strip_release(OutputDir) of
                                        {ok, _} ->
                                            {ok, State1};
                                        {error, _, Reason} ->
                                            ?RLX_ERROR({strip_release, Reason})
                                    end;
                                false ->
                                    {ok, State1}
                            end;
                        E ->
                            E
                    end;
                false ->
                    ?RLX_ERROR({unresolved_release, RelName, RelVsn})
            end;
        Error ->
            Error
    end.

-spec format_error(ErrorDetail::term(), rlx_state:t()) -> iolist().
format_error({unresolved_release, RelName, RelVsn}, _) ->
    io_lib:format("The release has not been resolved ~p-~s", [RelName, RelVsn]);
format_error({ec_file_error, AppDir, TargetDir, E}, _) ->
    io_lib:format("Unable to copy OTP App from ~s to ~s due to ~p",
                  [AppDir, TargetDir, E]);
format_error({config_does_not_exist, Path}, _) ->
    io_lib:format("The config file specified for this release (~s) does not exist!",
                  [Path]);
format_error({specified_erts_does_not_exist, ErtsVersion}, _) ->
    io_lib:format("Specified version of erts (~s) does not exist",
                  [ErtsVersion]);
format_error({release_script_generation_error, RelFile}, _) ->
    io_lib:format("Unknown internal release error generating the release file to ~s",
                  [RelFile]);
format_error({release_script_generation_warning, Module, Warnings}, _) ->
    ["Warnings generating release \s",
     rlx_util:indent(2), Module:format_warning(Warnings)];
format_error({unable_to_create_output_dir, OutputDir}, _) ->
    io_lib:format("Unable to create output directory (possible permissions issue): ~s",
                  [OutputDir]);
format_error({release_script_generation_error, Module, Errors}, State) ->
    ["Errors generating release \n",
     rlx_util:indent(2), Module:format_error(Errors, State)];
format_error({unable_to_make_symlink, AppDir, TargetDir, Reason}, _) ->
    io_lib:format("Unable to symlink directory ~s to ~s because \n~s~s",
                  [AppDir, TargetDir, rlx_util:indent(2),
                   file:format_error(Reason)]);
format_error({strip_release, Reason}, _) ->
    io_lib:format("Stripping debug info from release beam files failed becuase ~s",
                  [beam_lib:format_error(Reason)]).

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
    case ec_file:is_dir(OutputDir) of
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
            case ec_file:is_dir(TargetDir) of
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
    case ec_file:is_dir(SubSource) of
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
    ReleaseDir = rlx_util:release_output_dir(State0, Release0),
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
            case ec_file:is_dir(ErtsDir) of
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
    Options = [{path, [RelDir | rlx_util:get_code_paths(Release, OutputDir)]},
               {outdir, RelDir},
               no_module_tests, silent],
    Name = erlang:atom_to_list(rlx_release:name(Release)),
    ReleaseFile = filename:join([RelDir, Name ++ ".rel"]),
    case rlx_util:make_script(Options,
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

create_RELEASES(OutputDir, ReleaseFile) ->
    {ok, OldCWD} = file:get_cwd(),
    file:set_cwd(OutputDir),
    release_handler:create_RELEASES("./",
                                    "releases",
                                    ReleaseFile,
                                    []),
    file:set_cwd(OldCWD).

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
