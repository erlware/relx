-module(rlx_assemble).

-export([do/2,
         format_error/1]).

-include("relx.hrl").
-include("rlx_log.hrl").

do(Release, State) ->
    RelName = rlx_release:name(Release),
    ?log_debug("Assembling release ~p-~s", [RelName, rlx_release:vsn(Release)]),
    OutputDir = filename:join(rlx_state:base_output_dir(State), RelName),
    ok = create_output_dir(OutputDir),
    ok = copy_app_directories_to_output(Release, OutputDir, State),
    {ok, State1} = create_release_info(State, Release, OutputDir),

    %% don't strip the release in debug mode since that would strip
    %% the beams symlinked to and no one wants that
    case rlx_state:debug_info(State1) =:= strip
        andalso rlx_state:dev_mode(State1) =:= false of
        true ->
            case beam_lib:strip_release(OutputDir) of
                {ok, _} ->
                    {ok, State1};
                {error, _, Reason} ->
                    erlang:error(?RLX_ERROR({strip_release, Reason}))
            end;
        false ->
            {ok, State1}
    end.

%% internal functions

-spec create_output_dir(file:name()) -> ok.
create_output_dir(OutputDir) ->
    case rlx_file_utils:is_dir(OutputDir) of
        false ->
            case rlx_file_utils:mkdir_p(OutputDir) of
                ok ->
                    ok;
                {error, _} ->
                    erlang:error(?RLX_ERROR({unable_to_create_output_dir, OutputDir}))
            end;
        true ->
            ok
    end.

copy_app_directories_to_output(Release, OutputDir, State) ->
    LibDir = filename:join([OutputDir, "lib"]),
    ok = rlx_file_utils:mkdir_p(LibDir),
    IncludeSystemLibs = rlx_state:get(State, system_libs, rlx_state:get(State, include_erts, true)),
    Apps = prepare_applications(State, rlx_release:applications(Release)),
    [copy_app(State, LibDir, App, IncludeSystemLibs) || App <- Apps],
    ok.

prepare_applications(State, Apps) ->
    case rlx_state:dev_mode(State) of
        true ->
            [rlx_app_info:link(App, true) || App <- Apps];
        false ->
            %% TODO: since system libs are versioned, maybe we can always use symlinks for them
            %% [rlx_app_info:link(App, is_system_lib(rlx_app_info:dir(App))) || App <- Apps]
            Apps
    end.

copy_app(State, LibDir, App, IncludeSystemLibs) ->
    AppName = erlang:atom_to_list(rlx_app_info:name(App)),
    AppVsn = rlx_app_info:vsn(App),
    AppDir = rlx_app_info:dir(App),
    TargetDir = filename:join([LibDir, AppName ++ "-" ++ AppVsn]),
    case AppDir == rlx_util:to_binary(TargetDir) of
        true ->
            %% No need to do anything here, discover found something already in
            %% a release dir
            ok;
        false ->
            case IncludeSystemLibs of
                false ->
                    case is_system_lib(AppDir) of
                        true ->
                            ok;
                        false ->
                            copy_app_(State, App, AppDir, TargetDir)
                    end;
                _ ->
                    copy_app_(State, App, AppDir, TargetDir)
            end
    end.

%% TODO: support override system lib dir that isn't code:lib_dir/0
is_system_lib(Dir) ->
    lists:prefix(filename:split(code:lib_dir()), filename:split(rlx_string:to_list(Dir))).

copy_app_(State, App, AppDir, TargetDir) ->
    remove_symlink_or_directory(TargetDir),
    case rlx_app_info:link(App) of
        true ->
            link_directory(AppDir, TargetDir),
            rewrite_app_file(State, App, AppDir);
        false ->
            copy_directory(State, App, AppDir, TargetDir),
            rewrite_app_file(State, App, TargetDir)
    end.

%% If excluded apps or modules exist in this App's applications list we must write a new .app
rewrite_app_file(State, App, TargetDir) ->
    Name = rlx_app_info:name(App),
    Applications = rlx_app_info:applications(App),
    IncludedApplications = rlx_app_info:included_applications(App),

    %% TODO: should really read this in when creating rlx_app:t() and keep it
    AppFile = filename:join([TargetDir, "ebin", [Name, ".app"]]),
    {ok, [{application, AppName, AppData0}]} = file:consult(AppFile),

    %% maybe replace excluded apps
    AppData1 = maybe_exclude_apps(Applications, IncludedApplications,
                                  AppData0, rlx_state:exclude_apps(State)),

    AppData2 = maybe_exclude_modules(AppData1, proplists:get_value(Name,
                                                                   rlx_state:exclude_modules(State),
                                                                   [])),

    Spec = [{application, AppName, AppData2}],
    case write_file_if_contents_differ(AppFile, Spec) of
        ok ->
            ok;
        Error ->
            erlang:error(?RLX_ERROR({rewrite_app_file, AppFile, Error}))
    end.

maybe_exclude_apps(_Applications, _IncludedApplications, AppData, []) ->
    AppData;
maybe_exclude_apps(Applications, IncludedApplications, AppData, ExcludeApps) ->
    AppData1 = lists:keyreplace(applications,
                                1,
                                AppData,
                                {applications, Applications -- ExcludeApps}),
    lists:keyreplace(included_applications,
                     1,
                     AppData1,
                     {included_applications, IncludedApplications -- ExcludeApps}).

maybe_exclude_modules(AppData, []) ->
    AppData;
maybe_exclude_modules(AppData, ExcludeModules) ->
    Modules = proplists:get_value(modules, AppData, []),
    lists:keyreplace(modules,
                     1,
                     AppData,
                     {modules, Modules -- ExcludeModules}).

write_file_if_contents_differ(Filename, Spec) ->
    ToWrite = io_lib:format("~p.\n", Spec),
    case file:consult(Filename) of
        {ok, Spec} ->
            ok;
        {ok,  _} ->
            file:write_file(Filename, ToWrite);
        {error,  _} ->
            file:write_file(Filename, ToWrite)
    end.

remove_symlink_or_directory(TargetDir) ->
    case rlx_file_utils:is_symlink(TargetDir) of
        true ->
            ok = rlx_file_utils:remove(TargetDir);
        false ->
            case rlx_file_utils:is_dir(TargetDir) of
                true ->
                    ok = rlx_file_utils:remove(TargetDir, [recursive]);
                false ->
                    ok
            end
    end.

link_directory(AppDir, TargetDir) ->
    case rlx_file_utils:symlink_or_copy(AppDir, TargetDir) of
        {error, Reason} ->
            erlang:error(?RLX_ERROR({unable_to_make_symlink, AppDir, TargetDir, Reason}));
        ok ->
            ok
    end.

copy_directory(State, App, AppDir, TargetDir) ->
    [copy_dir(State, App, AppDir, TargetDir, SubDir)
    || SubDir <- ["ebin",
                  "include",
                  "priv" |
                  case include_src_or_default(State) of
                      true ->
                          ["src",
                           "lib",
                           "c_src"];
                      false ->
                          []
                  end]].

copy_dir(State, App, AppDir, TargetDir, SubDir) ->
    SubSource = filename:join(AppDir, SubDir),
    SubTarget = filename:join(TargetDir, SubDir),
    case rlx_file_utils:is_dir(SubSource) of
        true ->
            ok = rlx_file_utils:mkdir_p(SubTarget),
            %% get a list of the modules to be excluded from this app
            AppName = rlx_app_info:name(App),
            ExcludedModules = proplists:get_value(AppName, rlx_state:exclude_modules(State),
                                                  []),
            ExcludedFiles = [filename:join([binary_to_list(SubSource),
                                            atom_to_list(M) ++ ".beam"]) ||
                                M <- ExcludedModules],
            case copy_dir(SubSource, SubTarget, ExcludedFiles) of
                {error, E} ->
                    erlang:error(?RLX_ERROR({rlx_file_utils_error, AppDir, SubTarget, E}));
                ok ->
                    ok
            end;
        false ->
            ok
    end.

%% no files are excluded, just copy the whole dir
copy_dir(SourceDir, TargetDir, []) ->
     case rlx_file_utils:copy(SourceDir, TargetDir, [recursive, {file_info, [mode, time]}]) of
        {error, E} -> {error, E};
        ok ->
            ok
    end;
copy_dir(SourceDir, TargetDir, ExcludeFiles) ->
    SourceFiles = filelib:wildcard(
                    filename:join([binary_to_list(SourceDir), "*"])),
    lists:foreach(fun(F) ->
                    ok = rlx_file_utils:copy(F,
                                      filename:join([TargetDir,
                                                     filename:basename(F)]), [recursive, {file_info, [mode, time]}])
                  end, SourceFiles -- ExcludeFiles).

create_release_info(State0, Release0, OutputDir) ->
    RelName = atom_to_list(rlx_release:name(Release0)),
    ReleaseDir = rlx_util:release_output_dir(State0, Release0),
    ReleaseFile = filename:join([ReleaseDir, RelName ++ ".rel"]),
    StartCleanFile = filename:join([ReleaseDir, "start_clean.rel"]),
    NoDotErlFile = filename:join([ReleaseDir, "no_dot_erlang.rel"]),
    ok = rlx_file_utils:mkdir_p(ReleaseDir),
    Release1 = rlx_release:relfile(Release0, ReleaseFile),
    State1 = rlx_state:update_realized_release(State0, Release1),

    ReleaseSpec = rlx_release:metadata(Release1),

    StartCleanMeta = rlx_release:start_clean_metadata(Release1),
    NoDotErlMeta = rlx_release:no_dot_erlang_metadata(Release1),
    ok = rlx_file_utils:write_term(ReleaseFile, ReleaseSpec),
    ok = rlx_file_utils:write_term(StartCleanFile, StartCleanMeta),
    ok = rlx_file_utils:write_term(NoDotErlFile, NoDotErlMeta),
    write_bin_file(State1, Release1, OutputDir, ReleaseDir),
    {ok, State1}.

write_bin_file(State, Release, OutputDir, RelDir) ->
    RelName = erlang:atom_to_list(rlx_release:name(Release)),
    RelVsn = rlx_release:vsn(Release),
    BinDir = filename:join([OutputDir, "bin"]),
    ok = rlx_file_utils:mkdir_p(BinDir),
    VsnRel = filename:join(BinDir, rlx_release:canonical_name(Release)),
    BareRel = filename:join(BinDir, RelName),
    ErlOpts = rlx_state:get(State, erl_opts, ""),
    {OsFamily, _OsName} = rlx_util:os_type(State),

    %% erl_call is a bin in erl_interface that we must make a copy of
    %% so it is usable in release scripts from the release main bin dir
    Bin = code:lib_dir(erl_interface, bin),
    ErlCall = filename:join(Bin, "erl_call"),
    LocalErlCall = filename:join(BinDir, "erl_call"),
    ec_file:copy(ErlCall, LocalErlCall),

    %% always include the install_upgrade.escript tool
    install_upgrade_escript(BinDir),

    StartFile = case rlx_state:get(State, extended_start_script, false) of
                    false ->
                        bin_file_contents(OsFamily, RelName, RelVsn,
                                          rlx_release:erts(Release),
                                          ErlOpts);
                    true ->
                        Hooks = expand_hooks(BinDir,
                                             rlx_state:get(State,
                                                           extended_start_script_hooks,
                                                           []),
                                             State),
                        Extensions = rlx_state:get(State,
                                                   extended_start_script_extensions,
                                                   []),
                        extended_bin_file_contents(OsFamily, RelName, RelVsn,
                                                   rlx_release:erts(Release), ErlOpts,
                                                   Hooks, Extensions)
                end,
    %% We generate the start script by default, unless the user
    %% tells us not too
    case rlx_state:get(State, generate_start_script, true) of
        false ->
            ok;
        _ ->
            VsnRelStartFile = case OsFamily of
                unix -> VsnRel;
                win32 -> rlx_string:concat(VsnRel, ".cmd")
            end,
            ok = file:write_file(VsnRelStartFile, StartFile),
            ok = file:change_mode(VsnRelStartFile, 8#755),
            BareRelStartFile = case OsFamily of
                unix -> BareRel;
                win32 -> rlx_string:concat(BareRel, ".cmd")
            end,
            ok = file:write_file(BareRelStartFile, StartFile),
            ok = file:change_mode(BareRelStartFile, 8#755)
    end,
    ReleasesDir = filename:join(OutputDir, "releases"),
    generate_start_erl_data_file(Release, ReleasesDir),
    ok = copy_or_generate_vmargs_file(State, Release, RelDir),
    ok = copy_or_generate_sys_config_file(State, RelDir),
    include_erts(State, Release, OutputDir, RelDir).

expand_hooks(_Bindir, [], _State) -> [];
expand_hooks(BinDir, Hooks, _State) ->
    expand_hooks(BinDir, Hooks, [], _State).

expand_hooks(_BinDir, [], Acc, _State) -> Acc;
expand_hooks(BinDir, [{Phase, Hooks0} | Rest], Acc, State) ->
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
                        ok = render_hook(hook_template(Hook), HookScriptFilename, State),
                        %% and return the invocation that's to be templated in the
                        %% extended script
                        Acc0 ++ [hook_invocation(Hook)];
                    false ->
                        ?log_error("~p hook is not allowed in the ~p phase, ignoring it", [Hook, Phase]),
                        Acc0
                end
            end, [], Hooks0),
    expand_hooks(BinDir, Rest, Acc ++ [{Phase, Hooks}], State).

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
%% as well as status hooks
validate_hook(status, _) -> true;
%% deny all others
validate_hook(_, _) -> false.

hook_filename({custom, CustomScript}) -> CustomScript;
hook_filename(pid) -> "hooks/builtin/pid";
hook_filename({pid, _}) -> "hooks/builtin/pid";
hook_filename(wait_for_vm_start) -> "hooks/builtin/wait_for_vm_start";
hook_filename({wait_for_process, _}) -> "hooks/builtin/wait_for_process";
hook_filename(builtin_status) -> "hooks/builtin/status".

hook_invocation({custom, CustomScript}) -> CustomScript;
%% the pid builtin hook with no arguments writes to pid file
%% at /var/run/{{ rel_name }}.pid
hook_invocation(pid) -> rlx_string:join(["hooks/builtin/pid",
                                     "/var/run/$REL_NAME.pid"], "|");
hook_invocation({pid, PidFile}) -> rlx_string:join(["hooks/builtin/pid",
                                                PidFile], "|");
hook_invocation(wait_for_vm_start) -> "hooks/builtin/wait_for_vm_start";
hook_invocation({wait_for_process, Name}) ->
    %% wait_for_process takes an atom as argument
    %% which is the process name to wait for
    rlx_string:join(["hooks/builtin/wait_for_process",
                 atom_to_list(Name)], "|");
hook_invocation(builtin_status) -> "hooks/builtin/status".

hook_template({custom, _}) -> custom;
hook_template(pid) -> builtin_hook_pid;
hook_template({pid, _}) -> builtin_hook_pid;
hook_template(wait_for_vm_start) -> builtin_hook_wait_for_vm_start;
hook_template({wait_for_process, _}) -> builtin_hook_wait_for_process;
hook_template(builtin_status) -> builtin_hook_status.

%% custom hooks are not rendered, they should
%% be copied by the release overlays
render_hook(custom, _, _) -> ok;
render_hook(TemplateName, Script, _State) ->
    ?log_info("rendering ~p hook to ~p", [TemplateName, Script]),
    Template = render(TemplateName),
    ok = filelib:ensure_dir(Script),
    _ = rlx_file_utils:remove(Script),
    ok = file:write_file(Script, Template),
    ok = file:change_mode(Script, 8#755).

install_upgrade_escript(BinDir) ->
    InstallUpgradeFile = install_upgrade_escript_contents(),
    InstallUpgrade = filename:join([BinDir, "install_upgrade.escript"]),
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
-spec copy_or_generate_vmargs_file(rlx_state:t(), rlx_release:t(), file:name()) -> ok.
copy_or_generate_vmargs_file(State, Release, RelDir) ->
    RelVmargsPath = filename:join([RelDir, "vm.args"]),
    RelVmargsSrcPath = filename:join([RelDir, "vm.args.src"]),
    case rlx_state:vm_args_src(State) of
        undefined ->
            case rlx_state:vm_args(State) of
                false ->
                    ok;
                undefined ->
                    RelName = erlang:atom_to_list(rlx_release:name(Release)),
                    unless_exists_write_default(RelVmargsPath, vm_args_file(RelName));
                ArgsPath ->
                    case filelib:is_regular(ArgsPath) of
                        false ->
                            erlang:error(?RLX_ERROR({vmargs_does_not_exist, ArgsPath}));
                        true ->
                            copy_or_symlink_config_file(State, ArgsPath, RelVmargsPath)
                    end
            end;
        ArgsSrcPath ->
            %% print a warning if vm_args is also set
            case rlx_state:vm_args(State) of
                undefined ->
                    ok;
                _->
                    ?log_warn("Both vm_args_src and vm_args are set, vm_args will be ignored")
            end,

            case filelib:is_regular(ArgsSrcPath) of
                false ->
                    erlang:error(?RLX_ERROR({vmargs_src_does_not_exist, ArgsSrcPath}));
                true ->
                    copy_or_symlink_config_file(State, ArgsSrcPath, RelVmargsSrcPath)
            end
    end.

%% @doc copy config/sys.config or generate one to releases/VSN/sys.config
-spec copy_or_generate_sys_config_file(rlx_state:t(), file:name()) -> ok.
copy_or_generate_sys_config_file(State, RelDir) ->
    RelSysConfPath = filename:join([RelDir, "sys.config"]),
    RelSysConfSrcPath = filename:join([RelDir, "sys.config.src"]),
    case rlx_state:sys_config_src(State) of
        undefined ->
            case rlx_state:sys_config(State) of
                false ->
                    ok;
                undefined ->
                    unless_exists_write_default(RelSysConfPath, sys_config_file());
                ConfigPath ->
                    case filelib:is_regular(ConfigPath) of
                        false ->
                            erlang:error(?RLX_ERROR({config_does_not_exist, ConfigPath}));
                        true ->
                            %% validate sys.config is valid Erlang terms
                            case file:consult(ConfigPath) of
                                {ok, _} ->
                                    copy_or_symlink_config_file(State, ConfigPath, RelSysConfPath);
                                {error, Reason} ->
                                    erlang:error(?RLX_ERROR({sys_config_parse_error, ConfigPath, Reason}))
                            end
                    end
            end;
        ConfigSrcPath ->
            %% print a warning if sys_config is also set
            case rlx_state:sys_config(State) of
                P when P =:= false orelse P =:= undefined ->
                    ok;
                _->
                    ?log_warn("Both sys_config_src and sys_config are set, sys_config will be ignored")
            end,

            case filelib:is_regular(ConfigSrcPath) of
                false ->
                    erlang:error(?RLX_ERROR({config_src_does_not_exist, ConfigSrcPath}));
                true ->
                    copy_or_symlink_config_file(State, ConfigSrcPath, RelSysConfSrcPath)
            end
    end.

%% @doc copy config/sys.config or generate one to releases/VSN/sys.config
-spec copy_or_symlink_config_file(rlx_state:t(), file:name(), file:name()) ->
                                         ok.
copy_or_symlink_config_file(State, ConfigPath, RelConfPath) ->
    ensure_not_exist(RelConfPath),
    case rlx_state:dev_mode(State) of
        true ->
            ok = rlx_file_utils:symlink_or_copy(ConfigPath, RelConfPath);
        _ ->
            ok = rlx_file_utils:copy(ConfigPath, RelConfPath, [{file_info, [mode, time]}])
    end.

%% @doc Optionally add erts directory to release, if defined.
-spec include_erts(rlx_state:t(), rlx_release:t(),  file:name(), file:name()) -> ok.
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
            ?log_info("Including Erts from ~s", [Prefix]),
            ErtsVersion = rlx_release:erts(Release),
            ErtsBinDir = filename:join([Prefix, "erts-" ++ ErtsVersion, "bin"]),
            LocalErtsBin = filename:join([OutputDir, "erts-" ++ ErtsVersion, "bin"]),
            {OsFamily, _OsName} = rlx_util:os_type(State),
            case rlx_file_utils:is_dir(ErtsBinDir) of
                false ->
                    erlang:error(?RLX_ERROR({specified_erts_does_not_exist, ErtsVersion}));
                true ->
                    ok = rlx_file_utils:mkdir_p(LocalErtsBin),
                    ok = rlx_file_utils:copy(ErtsBinDir, LocalErtsBin, [recursive, {file_info, [mode, time]}]),

                    case OsFamily of
                        unix ->
                            DynErl = filename:join([LocalErtsBin, "dyn_erl"]),
                            Erl = filename:join([LocalErtsBin, "erl"]),
                            rlx_file_utils:copy(DynErl, Erl);
                        win32 ->
                            DynErl = filename:join([LocalErtsBin, "dyn_erl.ini"]),
                            Erl = filename:join([LocalErtsBin, "erl.ini"]),
                            rlx_file_utils:copy(DynErl, Erl)
                    end,

                    %% drop yielding_c_fun binary if it exists
                    %% it is large (1.1MB) and only used at compile time
                    _ = rlx_file_utils:remove(filename:join([LocalErtsBin, "yielding_c_fun"])),

                    make_boot_script(State, Release, OutputDir, RelDir)
            end
    end.

-spec make_boot_script(rlx_state:t(), rlx_release:t(), file:name(), file:name()) -> ok.
make_boot_script(State, Release, OutputDir, RelDir) ->
    IncludeSrc = include_src_or_default(State),
    WarningsAsErrors = rlx_state:warnings_as_errors(State),
    SrcTests = rlx_state:src_tests(State),
    Options = [{path, [RelDir | rlx_util:get_code_paths(Release, OutputDir)]},
               {outdir, RelDir},
               %% TODO: if dev_mode -> local,
               {variables, make_boot_script_variables(Release, State)},
               silent | case {WarningsAsErrors, SrcTests andalso IncludeSrc} of
                            {true, true} -> [warnings_as_errors, src_tests];
                            {true, false} -> [warnings_as_errors];
                            {false, true} -> [src_tests];
                            {false, false} -> []
                        end],
    Name = atom_to_list(rlx_release:name(Release)),
    ReleaseFile = filename:join([RelDir, [Name, ".rel"]]),
    IsRelxSasl = rlx_state:is_relx_sasl(State),
    case make_start_script(Name, RelDir, Options, IsRelxSasl) of
        Result when Result =:= ok orelse (is_tuple(Result) andalso
                                          element(1, Result) =:= ok) ->
            maybe_print_warnings(Result),
            ?log_debug("release start script created"),
            create_RELEASES(OutputDir, ReleaseFile),
            create_no_dot_erlang(RelDir, OutputDir, Options, State),
            create_start_clean(RelDir, OutputDir, Options, State);
        error ->
            erlang:error(?RLX_ERROR({release_script_generation_error, ReleaseFile}));
        {error, Module, Error} ->
            erlang:error(?RLX_ERROR({release_script_generation_error, Module, Error}))
    end.

%% when running `release' the default is to include src so `src_tests' can do checks
include_src_or_default(State) ->
    case rlx_state:include_src(State) of
        undefined ->
            true;
        IncludeSrc ->
            IncludeSrc
    end.

make_start_script(Name, RelDir, Options, IsRelxSasl) ->
    case IsRelxSasl of
        true ->
            %% systools in sasl version 3.5 and above has the `script_name' option
            systools:make_script(Name, [{script_name, "start"} | Options]);
        false ->
            case systools:make_script(Name, Options) of
                Result when Result =:= ok orelse (is_tuple(Result) andalso
                                          element(1, Result) =:= ok) ->
                    copy_to_start(RelDir, Name),
                    Result;
                Error ->
                    Error
            end
    end.

%% systools:make_script in sasl <3.5 do not support `script_name'
%% so the `Name.boot' file must be manually copied to `start.boot'
copy_to_start(RelDir, Name) ->
    BootFile = [Name, ".boot"],
    case rlx_file_utils:copy(filename:join([RelDir, BootFile]),
                             filename:join(RelDir, "start.boot")) of
        ok ->
            ok;
        {error, Reason} ->
            %% it isn't absolutely necesary for start.boot to exist so just warn
            ?log_warn("Unable to copy boot file ~s to start.boot: ~p", [BootFile, Reason]),
            ok
    end.

maybe_print_warnings({ok, Module, Warnings}) when Warnings =/= [] ->
    FormattedWarnings = unicode:characters_to_list(Module:format_warning(Warnings)),
    Trimmed = rlx_string:trim(FormattedWarnings, trailing, "\n"),
    ?log_warn("Warnings generating release:~n~s", [Trimmed]);
maybe_print_warnings(_) ->
    ok.

make_boot_script_variables(Release, State) ->
    % A boot variable is needed when {system_libs, false} and the application
    % directories are split between the release/lib directory and the erlang
    % install directory on the target host.
    % The built-in $ROOT variable points to the erts directory on Windows
    % (dictated by erl.ini [erlang] Rootdir=) and so a boot variable is made
    % pointing to the release directory
    % On non-Windows, $ROOT is set by the ROOTDIR environment variable as the
    % release directory, so a boot variable is made pointing to the erts
    % directory.
    % NOTE the boot variable can point to either the release/erts root directory
    % or the release/erts lib directory, as long as the usage here matches the
    % usage used in the start up scripts
    case {os:type(), rlx_state:get(State, system_libs, true)} of
        {{win32, _}, false} ->
            [{"RELEASE_DIR", filename:join(rlx_state:base_output_dir(State),
                                           rlx_release:name(Release))}];
        {{win32, _}, true} ->
            [];
        _ ->
            [{"SYSTEM_LIB_DIR", code:lib_dir()}]
    end.

create_no_dot_erlang(RelDir, OutputDir, Options, _State) ->
    create_boot_file(RelDir, [no_dot_erlang | Options], "no_dot_erlang"),
    copy_boot_to_bin(RelDir, OutputDir, "no_dot_erlang").

create_start_clean(RelDir, _OutputDir, Options, _State) ->
    create_boot_file(RelDir, Options, "start_clean").

create_boot_file(RelDir, Options, Name) ->
    case systools:make_script(Name, [{outdir, RelDir},
                                     no_warn_sasl | Options]) of
        Result when Result =:= ok orelse (is_tuple(Result) andalso
                                          element(1, Result) =:= ok) ->
            remove_rel_and_script(RelDir, Name);
        error ->
            erlang:error(?RLX_ERROR({boot_script_generation_error, Name}));
        {error, Module, Error} ->
            erlang:error(?RLX_ERROR({boot_script_generation_error, Name, Module, Error}))
    end.

%% escript requires boot files in bin/ and not under the release dir
copy_boot_to_bin(RelDir, OutputDir, Name) ->
    From = filename:join([RelDir, Name++".boot"]),
    To = filename:join([OutputDir, "bin", Name++".boot"]),
    case rlx_file_utils:copy(From, To) of
        ok ->
            ok;
        {error, Reason} ->
            erlang:error({failed_copy_boot_to_bin, From, To, Reason})
    end.

remove_rel_and_script(RelDir, Name) ->
    rlx_file_utils:remove(filename:join([RelDir, Name++".rel"])),
    rlx_file_utils:remove(filename:join([RelDir, Name++".script"])).

create_RELEASES(OutputDir, ReleaseFile) ->
    {ok, OldCWD} = file:get_cwd(),
    file:set_cwd(OutputDir),
    release_handler:create_RELEASES("./",
                                    "releases",
                                    ReleaseFile,
                                    []),
    file:set_cwd(OldCWD).

unless_exists_write_default(Path, File) ->
    case rlx_file_utils:exists(Path) of
        true ->
            ok;
        false ->
            ok = file:write_file(Path, File)
    end.

-spec ensure_not_exist(file:name()) -> ok.
ensure_not_exist(RelConfPath)     ->
    case rlx_file_utils:exists(RelConfPath) of
        false ->
            ok;
        _ ->
            rlx_file_utils:remove(RelConfPath)
    end.

bin_file_contents(OsFamily, RelName, RelVsn, ErtsVsn, ErlOpts) ->
    Template = case OsFamily of
        unix -> bin;
        win32 -> bin_windows
    end,
    render(Template, [{rel_name, RelName}, {rel_vsn, RelVsn},
                      {erts_vsn, ErtsVsn}, {erl_opts, ErlOpts}]).

extended_bin_file_contents(OsFamily, RelName, RelVsn, ErtsVsn, ErlOpts, Hooks, Extensions) ->
    Template = case OsFamily of
        unix -> extended_bin;
        win32 -> extended_bin_windows
    end,
    %% turn all the hook lists into space separated strings
    PreStartHooks = rlx_string:join(proplists:get_value(pre_start, Hooks, []), " "),
    PostStartHooks = rlx_string:join(proplists:get_value(post_start, Hooks, []), " "),
    PreStopHooks = rlx_string:join(proplists:get_value(pre_stop, Hooks, []), " "),
    PostStopHooks = rlx_string:join(proplists:get_value(post_stop, Hooks, []), " "),
    PreInstallUpgradeHooks = rlx_string:join(proplists:get_value(pre_install_upgrade,
                                                Hooks, []), " "),
    PostInstallUpgradeHooks = rlx_string:join(proplists:get_value(post_install_upgrade,
                                                 Hooks, []), " "),
    StatusHook = rlx_string:join(proplists:get_value(status, Hooks, []), " "),
    {ExtensionsList1, ExtensionDeclarations1} =
        lists:foldl(fun({Name, Script},
                        {ExtensionsList0, ExtensionDeclarations0}) ->
                            ExtensionDeclaration = atom_to_list(Name) ++
                                                   "_extension=\"" ++
                                                   Script ++ "\"",
                            {ExtensionsList0 ++ [atom_to_list(Name)],
                             ExtensionDeclarations0 ++ [ExtensionDeclaration]}
                    end, {[], []}, Extensions),
    % pipe separated string of extensions, to show on the start script usage
    % (eg. foo|bar)
    ExtensionsList = rlx_string:join(ExtensionsList1 ++ ["undefined"], "|"),
    % command separated string of extension script declarations
    % (eg. foo_extension="path/to/foo_script")
    ExtensionDeclarations = rlx_string:join(ExtensionDeclarations1, ";"),
    render(Template, [{rel_name, RelName}, {rel_vsn, RelVsn},
                      {erts_vsn, ErtsVsn}, {erl_opts, ErlOpts},
                      {pre_start_hooks, PreStartHooks},
                      {post_start_hooks, PostStartHooks},
                      {pre_stop_hooks, PreStopHooks},
                      {post_stop_hooks, PostStopHooks},
                      {pre_install_upgrade_hooks, PreInstallUpgradeHooks},
                      {post_install_upgrade_hooks, PostInstallUpgradeHooks},
                      {status_hook, StatusHook},
                      {extensions, ExtensionsList},
                      {extension_declarations, ExtensionDeclarations}]).

install_upgrade_escript_contents() ->
    render(install_upgrade_escript).

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

%%

-spec format_error(ErrorDetail::term()) -> iolist().
format_error({failed_copy_boot_to_bin, From, To, Reason}) ->
    io_lib:format("Unable to copy ~s to ~s for reason: ~p", [From, To, Reason]);
format_error({unresolved_release, RelName, RelVsn}) ->
    io_lib:format("The release has not been resolved ~p-~s", [RelName, RelVsn]);
format_error({rlx_file_utils_error, AppDir, TargetDir, E}) ->
    io_lib:format("Unable to copy OTP App from ~s to ~s due to ~p",
                  [AppDir, TargetDir, E]);
format_error({vmargs_does_not_exist, Path}) ->
    io_lib:format("The vm.args file specified for this release (~s) does not exist!",
                  [Path]);
format_error({vmargs_src_does_not_exist, Path}) ->
    io_lib:format("The vm.args.src file specified for this release (~s) does not exist!",
                  [Path]);
format_error({config_does_not_exist, Path}) ->
    io_lib:format("The sys.config file specified for this release (~s) does not exist!",
                  [Path]);
format_error({config_src_does_not_exist, Path}) ->
    io_lib:format("The sys.config.src file specified for this release (~s) does not exist!",
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
format_error({unable_to_create_output_dir, OutputDir}) ->
    io_lib:format("Unable to create output directory (possible permissions issue): ~s",
                  [OutputDir]);
format_error({release_script_generation_error, Module, Errors}) ->
    ["Error generating release: \n", Module:format_error(Errors)];
format_error({unable_to_make_symlink, AppDir, TargetDir, Reason}) ->
    io_lib:format("Unable to symlink directory ~s to ~s because \n~s~s",
                  [AppDir, TargetDir, rlx_util:indent(2),
                   file:format_error(Reason)]);
format_error({boot_script_generation_error, Name}) ->
    io_lib:format("Unknown internal release error generating ~s.boot", [Name]);
format_error({boot_script_generation_error, Name, Module, Errors}) ->
    [io_lib:format("Errors generating ~s.boot: \n", [Name]),
     rlx_util:indent(2), Module:format_error(Errors)];
format_error({strip_release, Reason}) ->
    io_lib:format("Stripping debug info from release beam files failed becuase ~s",
                  [beam_lib:format_error(Reason)]);
format_error({rewrite_app_file, AppFile, Error}) ->
    io_lib:format("Unable to rewrite .app file ~s due to ~p",
                  [AppFile, Error]).
