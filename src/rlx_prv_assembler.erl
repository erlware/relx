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
         format_error/1]).

-include("relx.hrl").

-define(PROVIDER, release).
-define(DEPS, [resolve_release]).

%%============================================================================
%% API
%%============================================================================
-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    State1 = rlx_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {deps, ?DEPS},
                                                             {hooks, {[], [overlay]}}])),
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

-spec format_error(ErrorDetail::term()) -> iolist().
format_error({unresolved_release, RelName, RelVsn}) ->
    io_lib:format("The release has not been resolved ~p-~s", [RelName, RelVsn]);
format_error({ec_file_error, AppDir, TargetDir, E}) ->
    io_lib:format("Unable to copy OTP App from ~s to ~s due to ~p",
                  [AppDir, TargetDir, E]);
format_error({config_does_not_exist, Path}) ->
    io_lib:format("The config file specified for this release (~s) does not exist!",
                  [Path]);
format_error({sys_config_parse_error, ConfigPath, Reason}) ->
    io_lib:format("The config file (~s) specified for this release could not be opened or parsed: ~s",
                  [ConfigPath, file:format_error(Reason)]);
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
format_error({unable_to_make_symlink, AppDir, TargetDir, Reason}) ->
    io_lib:format("Unable to symlink directory ~s to ~s because \n~s~s",
                  [AppDir, TargetDir, rlx_util:indent(2),
                   file:format_error(Reason)]);
format_error(start_clean_script_generation_error) ->
    "Unknown internal release error generating start_clean.boot";
format_error({start_clean_script_generation_warning, Module, Warnings}) ->
    ["Warnings generating start_clean.boot \s",
     rlx_util:indent(2), Module:format_warning(Warnings)];
format_error({start_clean_script_generation_error, Module, Errors}) ->
    ["Errors generating start_clean.boot \n",
     rlx_util:indent(2), Module:format_error(Errors)];
format_error({strip_release, Reason}) ->
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
    IncludeErts = rlx_state:get(State, include_erts, true),
    Apps = prepare_applications(State, rlx_release:application_details(Release)),
    Result = lists:filter(fun({error, _}) ->
                                  true;
                             (_) ->
                                  false
                          end,
                         lists:flatten(ec_plists:map(fun(App) ->
                                                             copy_app(LibDir, App, IncludeSrc, IncludeErts)
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

copy_app(LibDir, App, IncludeSrc, IncludeErts) ->
    AppName = erlang:atom_to_list(rlx_app_info:name(App)),
    AppVsn = rlx_app_info:original_vsn(App),
    AppDir = rlx_app_info:dir(App),
    TargetDir = filename:join([LibDir, AppName ++ "-" ++ AppVsn]),
    case AppDir == ec_cnv:to_binary(TargetDir) of
        true ->
            %% No need to do anything here, discover found something already in
            %% a release dir
            ok;
        false ->
            case IncludeErts of
                false ->
                    case is_erts_lib(AppDir) of
                        true ->
                            [];
                        false ->
                            copy_app_(App, AppDir, TargetDir, IncludeSrc)
                    end;
                _ ->
                    copy_app_(App, AppDir, TargetDir, IncludeSrc)
            end
    end.

is_erts_lib(Dir) ->
    lists:prefix(filename:split(list_to_binary(code:lib_dir())), filename:split(Dir)).

copy_app_(App, AppDir, TargetDir, IncludeSrc) ->
    remove_symlink_or_directory(TargetDir),
    case rlx_app_info:link(App) of
        true ->
            link_directory(AppDir, TargetDir),
            rewrite_app_file(App, AppDir);
        false ->
            copy_directory(AppDir, TargetDir, IncludeSrc),
            rewrite_app_file(App, TargetDir)
    end.

%% If excluded apps exist in this App's applications list we must write a new .app
rewrite_app_file(App, TargetDir) ->
    Name = rlx_app_info:name(App),
    ActiveDeps = rlx_app_info:active_deps(App),
    IncludedDeps = rlx_app_info:library_deps(App),
    AppFile = filename:join([TargetDir, "ebin", ec_cnv:to_list(Name) ++ ".app"]),
    {ok, [{application, AppName, AppData}]} = file:consult(AppFile),
    OldActiveDeps = proplists:get_value(applications, AppData, []),
    OldIncludedDeps = proplists:get_value(included_applications, AppData, []),

    case {OldActiveDeps, OldIncludedDeps} of
        {ActiveDeps, IncludedDeps} ->
            ok;
        _ ->
            AppData1 = lists:keyreplace(applications
                                       ,1
                                       ,AppData
                                       ,{applications, ActiveDeps}),
            AppData2 = lists:keyreplace(included_applications
                                       ,1
                                       ,AppData1
                                       ,{included_applications, IncludedDeps}),
            Spec = io_lib:format("~p.\n", [{application, AppName, AppData2}]),
            write_file_if_contents_differ(AppFile, Spec)
    end.

write_file_if_contents_differ(Filename, Bytes) ->
    ToWrite = iolist_to_binary(Bytes),
    case file:read_file(Filename) of
        {ok, ToWrite} ->
            ok;
        {ok,  _} ->
            file:write_file(Filename, ToWrite);
        {error,  _} ->
            file:write_file(Filename, ToWrite)
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
    case rlx_util:symlink_or_copy(AppDir, TargetDir) of
        {error, Reason} ->
            ?RLX_ERROR({unable_to_make_symlink, AppDir, TargetDir, Reason});
        ok ->
            ok
    end.

copy_directory(AppDir, TargetDir, IncludeSrc) ->
    [copy_dir(AppDir, TargetDir, SubDir)
    || SubDir <- ["ebin",
                  "include",
                  "priv",
                  "lib" |
                  case IncludeSrc of
                      true ->
                          ["src",
                           "c_src"];
                      false ->
                          []
                  end]].

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
    StartCleanFile = filename:join([ReleaseDir, "start_clean.rel"]),
    ok = ec_file:mkdir_p(ReleaseDir),
    Release1 = rlx_release:relfile(Release0, ReleaseFile),
    State1 = rlx_state:update_realized_release(State0, Release1),
    case rlx_release:metadata(Release1) of
        {ok, Meta} ->
            case rlx_release:start_clean_metadata(Release1) of
                {ok, StartCleanMeta} ->
                    ok = ec_file:write_term(ReleaseFile, Meta),
                    ok = ec_file:write_term(StartCleanFile, StartCleanMeta),
                    write_bin_file(State1, Release1, OutputDir, ReleaseDir);
                E ->
                    E
            end;
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
                        case rlx_state:get(State, include_nodetool, false) of
                            true ->
                                include_nodetool(BinDir);
                            false ->
                                ok
                        end,
                        bin_file_contents(OsFamily, RelName, RelVsn,
                                          rlx_release:erts(Release),
                                          ErlOpts);
                    true ->
                        %% extended start script needs nodetool so it's
                        %% always included
                        include_nodetool(BinDir),
                        Hooks = expand_hooks(BinDir,
                                             rlx_state:get(State,
                                                           extended_start_script_hooks, [])),
                        extended_bin_file_contents(OsFamily, RelName, RelVsn,
                                                   rlx_release:erts(Release), ErlOpts,
                                                   Hooks)
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
    case copy_or_generate_sys_config_file(State, RelDir) of
        ok ->
            include_erts(State, Release, OutputDir, RelDir);
        E ->
            E
    end.

expand_hooks(_Bindir, []) -> [];
expand_hooks(BinDir, Hooks) ->
    expand_hooks(BinDir, Hooks, []).

expand_hooks(_BinDir, [], Acc) -> Acc;
expand_hooks(BinDir, [{Phase, Hooks0} | Rest], Acc) ->
    %% filter and expand hooks to their respective shell scripts
    Hooks =
        lists:foldl(
            fun(Hook, Acc0) ->
                case validate_hook(Phase, Hook) of
                    true ->
                        %% all hooks are relative to the bin dir
                        HookScriptFilename = filename:join([BinDir,
                                                            hook_filename(Hook)]),
                        %% write the hook script file to it's proper location
                        ok = render_hook(hook_template(Hook), HookScriptFilename),
                        %% and return the invocation that's to be templated in the
                        %% extended script
                        Acc0 ++ [hook_invocation(Hook)];
                    false ->
                        rebar_api:info("~p hook is not allowed in the ~p phase, ignoring it",
                            [Hook, Phase]),
                        Acc0
                end
            end, [], Hooks0),
    expand_hooks(BinDir, Rest, Acc ++ [{Phase, Hooks}]).

%% the pid script hook is only allowed in the
%% post_start phase
%% with args
validate_hook(post_start, {pid, _}) -> true;
%% and without args
validate_hook(post_start, pid) -> true;
%% same for wait_for_vm_start, wait_for_process script
validate_hook(post_start, wait_for_vm_start) -> true;
validate_hook(post_start, {wait_for_process, _}) -> true;
%% custom hooks are allowed in all phases
validate_hook(_Phase, {custom, _}) -> true;
%% deny all others
validate_hook(_, _) -> false.

hook_filename({custom, CustomScript}) -> CustomScript;
hook_filename(pid) -> "hooks/builtin/pid";
hook_filename({pid, _}) -> "hooks/builtin/pid";
hook_filename(wait_for_vm_start) -> "hooks/builtin/wait_for_vm_start";
hook_filename({wait_for_process, _}) -> "hooks/builtin/wait_for_process".

hook_invocation({custom, CustomScript}) -> CustomScript;
%% the pid builtin hook with no arguments writes to pid file
%% at /var/run/{{ rel_name }}.pid
hook_invocation(pid) -> string:join(["hooks/builtin/pid",
                                     "/var/run/$REL_NAME.pid"], "|");
hook_invocation({pid, PidFile}) -> string:join(["hooks/builtin/pid",
                                                PidFile], "|");
hook_invocation(wait_for_vm_start) -> "hooks/builtin/wait_for_vm_start";
hook_invocation({wait_for_process, Name}) ->
    %% wait_for_process takes an atom as argument
    %% which is the process name to wait for
    string:join(["hooks/builtin/wait_for_process",
                 atom_to_list(Name)], "|").

hook_template({custom, _}) -> custom;
hook_template(pid) -> builtin_hook_pid;
hook_template({pid, _}) -> builtin_hook_pid;
hook_template(wait_for_vm_start) -> builtin_hook_wait_for_vm_start;
hook_template({wait_for_process, _}) -> builtin_hook_wait_for_process.

%% custom hooks are not rendered, they should
%% be copied by the release overlays
render_hook(custom, _) -> ok;
render_hook(TemplateName, Script) ->
    rebar_api:debug("rendering ~p hook to ~p",
        [TemplateName, Script]),
    Template = render(TemplateName),
    ok = filelib:ensure_dir(Script),
    _ = ec_file:remove(Script),
    ok = file:write_file(Script, Template),
    ok = file:change_mode(Script, 8#755).

include_nodetool(BinDir) ->
    NodeToolFile = nodetool_contents(),
    InstallUpgradeFile = install_upgrade_escript_contents(),
    NodeTool = filename:join([BinDir, "nodetool"]),
    InstallUpgrade = filename:join([BinDir, "install_upgrade.escript"]),
    ok = file:write_file(NodeTool, NodeToolFile),
    ok = file:write_file(InstallUpgrade, InstallUpgradeFile).

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
        false ->
            ok;
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
        false ->
            ok;
        undefined ->
            unless_exists_write_default(RelSysConfPath, sys_config_file());
        ConfigPath ->
            case filelib:is_regular(ConfigPath) of
                false ->
                    ?RLX_ERROR({config_does_not_exist, ConfigPath});
                true ->
                    %% validate sys.config is valid Erlang terms
                    case file:consult(ConfigPath) of
                        {ok, _} ->
                            copy_or_symlink_config_file(State, ConfigPath, RelSysConfPath);
                        {error, Reason} ->
                            ?RLX_ERROR({sys_config_parse_error, ConfigPath, Reason})
                    end
            end
    end.

%% @doc copy config/sys.config or generate one to releases/VSN/sys.config
-spec copy_or_symlink_config_file(rlx_state:t(), file:name(), file:name()) ->
                                         ok.
copy_or_symlink_config_file(State, ConfigPath, RelConfPath) ->
    ensure_not_exist(RelConfPath),
    case rlx_state:dev_mode(State) of
        true ->
            ok = rlx_util:symlink_or_copy(ConfigPath, RelConfPath);
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

                    %% delete erts src if the user requested it not be included
                    case rlx_state:include_src(State) of
                        true -> ok;
                        false ->
                            SrcDir = filename:join([LocalErts, "src"]),
                            %% ensure the src folder exists before deletion
                            case ec_file:exists(SrcDir) of
                              true -> ok = ec_file:remove(SrcDir, [recursive]);
                              false -> ok
                            end
                    end,

                    case rlx_state:get(State, extended_start_script, false) of
                        true ->

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
               {variables, make_boot_script_variables(State)},
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
            create_start_clean(RelDir, OutputDir, Options, State);
        error ->
            ?RLX_ERROR({release_script_generation_error, ReleaseFile});
        {ok, _, []} ->
            ec_cmd_log:info(rlx_state:log(State),
                          "release successfully created!"),
            create_RELEASES(OutputDir, ReleaseFile),
            create_start_clean(RelDir, OutputDir, Options, State);
        {ok,Module,Warnings} ->
            ?RLX_ERROR({release_script_generation_warn, Module, Warnings});
        {error,Module,Error} ->
            ?RLX_ERROR({release_script_generation_error, Module, Error})
    end.

make_boot_script_variables(State) ->
    % A boot variable is needed when {include_erts, false} and the application
    % directories are split between the release/lib directory and the erts/lib
    % directory.
    % The built-in $ROOT variable points to the erts directory on Windows
    % (dictated by erl.ini [erlang] Rootdir=) and so a boot variable is made
    % pointing to the release directory
    % On non-Windows, $ROOT is set by the ROOTDIR environment variable as the
    % release directory, so a boot variable is made pointing to the erts
    % directory.
    % NOTE the boot variable can point to either the release/erts root directory
    % or the release/erts lib directory, as long as the usage here matches the
    % usage used in the start up scripts
    case {os:type(), rlx_state:get(State, include_erts, true)} of
        {{win32, _}, false} ->
            [{"RELEASE_DIR", rlx_state:output_dir(State)}];
        {{win32, _}, true} ->
            [];
        _ ->
            [{"ERTS_LIB_DIR", code:lib_dir()}]
    end.

create_start_clean(RelDir, OutputDir, Options, State) ->
    case rlx_util:make_script(Options,
                         fun(CorrectedOptions) ->
                                 systools:make_script("start_clean", CorrectedOptions)
                         end) of
        ok ->
            ok = ec_file:copy(filename:join([RelDir, "start_clean.boot"]),
                              filename:join([OutputDir, "bin", "start_clean.boot"])),
            ec_file:remove(filename:join([RelDir, "start_clean.rel"])),
            ec_file:remove(filename:join([RelDir, "start_clean.script"])),
            {ok, State};
        error ->
            ?RLX_ERROR(start_clean_script_generation_error);
        {ok, _, []} ->
            ok = ec_file:copy(filename:join([RelDir, "start_clean.boot"]),
                              filename:join([OutputDir, "bin", "start_clean.boot"])),
            ec_file:remove(filename:join([RelDir, "start_clean.rel"])),
            ec_file:remove(filename:join([RelDir, "start_clean.script"])),
            {ok, State};
        {ok,Module,Warnings} ->
            ?RLX_ERROR({start_clean_script_generation_warn, Module, Warnings});
        {error,Module,Error} ->
            ?RLX_ERROR({start_clean_script_generation_error, Module, Error})
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
    render(erl_script, [{erts_vsn, ErtsVsn}]).

bin_file_contents(OsFamily, RelName, RelVsn, ErtsVsn, ErlOpts) ->
    Template = case OsFamily of
        unix -> bin;
        win32 -> bin_windows
    end,
    render(Template, [{rel_name, RelName}, {rel_vsn, RelVsn},
                      {erts_vsn, ErtsVsn}, {erl_opts, ErlOpts}]).

extended_bin_file_contents(OsFamily, RelName, RelVsn, ErtsVsn, ErlOpts, Hooks) ->
    Template = case OsFamily of
        unix -> extended_bin;
        win32 -> extended_bin_windows
    end,
    %% turn all the hook lists into space separated strings
    PreStartHooks = string:join(proplists:get_value(pre_start, Hooks, []), " "),
    PostStartHooks = string:join(proplists:get_value(post_start, Hooks, []), " "),
    PreStopHooks = string:join(proplists:get_value(pre_stop, Hooks, []), " "),
    PostStopHooks = string:join(proplists:get_value(post_stop, Hooks, []), " "),
    PreInstallUpgradeHooks = string:join(proplists:get_value(pre_install_upgrade,
                                                Hooks, []), " "),
    PostInstallUpgradeHooks = string:join(proplists:get_value(post_install_upgrade,
                                                 Hooks, []), " "),
    render(Template, [{rel_name, RelName}, {rel_vsn, RelVsn},
                      {erts_vsn, ErtsVsn}, {erl_opts, ErlOpts},
                      {pre_start_hooks, PreStartHooks},
                      {post_start_hooks, PostStartHooks},
                      {pre_stop_hooks, PreStopHooks},
                      {post_stop_hooks, PostStopHooks},
                      {pre_install_upgrade_hooks, PreInstallUpgradeHooks},
                      {post_install_upgrade_hooks, PostInstallUpgradeHooks}]).

erl_ini(OutputDir, ErtsVsn) ->
    ErtsDirName = string:concat("erts-", ErtsVsn),
    BinDir = filename:join([OutputDir, ErtsDirName, bin]),
    render(erl_ini, [{bin_dir, BinDir}, {output_dir, OutputDir}]).

install_upgrade_escript_contents() ->
    render(install_upgrade_escript).

nodetool_contents() ->
    render(nodetool).

sys_config_file() ->
    render(sys_config).

vm_args_file(RelName) ->
    render(vm_args, [{rel_name, RelName}]).

render(Template) ->
    render(Template, []).

render(Template, Data) ->
    Files = rlx_util:template_files(),
    Tpl = rlx_util:load_file(Files, escript, atom_to_list(Template)),
    {ok, Content} = rlx_util:render(Tpl, Data),
    Content.
