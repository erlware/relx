-module(rlx_tar).

-export([make_tar/3,
         format_error/1]).

-include("relx.hrl").
-include("rlx_log.hrl").

make_tar(Release, OutputDir, State) ->
    IsRelxSaslVsn = rlx_util:relx_sasl_vsn(),

    Name = rlx_release:name(Release),
    Vsn = rlx_release:vsn(Release),
    ErtsVersion = rlx_release:erts(Release),

    OverlayVars = rlx_overlay:generate_overlay_vars(State, Release),
    OverlayFiles = overlay_files(OverlayVars, rlx_state:get(State, overlay, undefined), OutputDir),
    ConfigFiles = config_files(Vsn, OutputDir),

    StartClean = filename:join(["releases", Vsn, "start_clean.boot"]),
    NoDotErlang = filename:join(["releases", Vsn, "no_dot_erlang.boot"]),

    ExtraFiles = OverlayFiles ++ ConfigFiles ++
        [{StartClean, filename:join([OutputDir, StartClean])},
         {NoDotErlang, filename:join([OutputDir, NoDotErlang])},
         {filename:join(["releases", "start_erl.data"]),
          filename:join([OutputDir, "releases", "start_erl.data"])},
         {filename:join(["releases", "RELEASES"]),
          filename:join([OutputDir, "releases", "RELEASES"])},
         {"bin", filename:join([OutputDir, "bin"])}],

    Opts = [{path, [filename:join([OutputDir, "lib", "*", "ebin"])]},
            {dirs, app_dirs(State)},
            silent,
            {outdir, OutputDir} |
            case rlx_state:get(State, include_erts, true) of
                true ->
                    [{erts, code:root_dir()}];
                false ->
                    [];
                ErtsDir ->
                    [{erts, ErtsDir}]
            end] ++ case IsRelxSaslVsn of
                        true ->
                            %% file tuples for erl_tar:add are the reverse of erl_tar:create so swap them
                            [{extra_files, [{From, To} || {To, From} <- ExtraFiles]}];
                        false ->
                            []
                    end,
    try systools:make_tar(filename:join([OutputDir, "releases", Vsn, Name]), Opts) of
        Result when Result =:= ok orelse (is_tuple(Result) andalso
                                          element(1, Result) =:= ok) ->
            maybe_print_warnings(Result),
            case IsRelxSaslVsn of
                true ->
                    %% nothing more to do, we used extra_files to copy in the overlays
                    {ok, State};
                false ->
                    %% unpack the tarball to a temporary directory and repackage it with
                    %% the overlays and other files we need to complete the target system
                    TempDir = rlx_file_utils:mkdtemp(),
                    try
                        update_tar(ExtraFiles, State, TempDir, OutputDir, Name, Vsn, ErtsVersion)
                    catch
                        ?WITH_STACKTRACE(Type, Exception, Stacktrace)
                           ?log_debug("exception updating tarball ~p:~p stacktrace=~p",
                                      [Type, Exception, Stacktrace]),
                           erlang:error(?RLX_ERROR({tar_update_error, Type, Exception}))
                    after
                        rlx_file_utils:remove(TempDir, [recursive])
                    end
            end;
        error ->
            ?RLX_ERROR({tar_unknown_generation_error, Name, Vsn});
        {error, Module, Errors} ->
            ?RLX_ERROR({tar_generation_error, Module, Errors})
    catch
        _:{badarg, Args} ->
            erlang:exit(?RLX_ERROR({make_tar, {badarg, Args}}))
    end.

maybe_print_warnings({ok, Module, Warnings}) when Warnings =/= [] ->
    ?log_warn("Warnings generating release:~n~s", [Module:format_warning(Warnings)]);
maybe_print_warnings(_) ->
    ok.

format_error({make_tar, {badarg, Args}}) ->
    io_lib:format("Unknown args given to systools:make_tar/2: ~p", [Args]);
format_error({tar_unknown_generation_error, Module, Vsn}) ->
    io_lib:format("Tarball generation error of ~s ~s", [Module, Vsn]);
format_error({tar_update_error, Type, Exception}) ->
    io_lib:format("Exception updating contents of release tarball ~s:~s", [Type, Exception]);
format_error({tar_generation_error, Module, Errors}) ->
    io_lib:format("Tarball generation error for ~p", [Module:format_error(Errors)]).

%%

%% used to add additional files to the release tarball when using systools
%% before the `extra_files' feature was added to `make_tar'
update_tar(ExtraFiles, State, TempDir, OutputDir, Name, Vsn, ErtsVersion) ->
    IncludeErts = rlx_state:get(State, include_erts, true),
    SystemLibs = rlx_state:get(State, system_libs, IncludeErts),
    TarFile = filename:join(OutputDir, [Name, "-", Vsn, ".tar.gz"]),
    file:rename(filename:join(OutputDir, [Name, ".tar.gz"]), TarFile),
    erl_tar:extract(TarFile, [{cwd, TempDir}, compressed]),
    ok =
        erl_tar:create(TarFile,
                       [{"releases", filename:join(TempDir, "releases")},
                        {filename:join(["releases", "start_erl.data"]),
                         filename:join([OutputDir, "releases", "start_erl.data"])},
                        {filename:join(["releases", "RELEASES"]),
                         filename:join([OutputDir, "releases", "RELEASES"])},
                        {"bin", filename:join([OutputDir, "bin"])} |
                        case IncludeErts of
                            false ->
                                %% Remove system libs from tarball
                                case SystemLibs of
                                    false ->
                                        Libs = filelib:wildcard("*", filename:join(TempDir, "lib")),
                                        AllSystemLibs = filelib:wildcard("*", code:lib_dir()),
                                        [{filename:join("lib", LibDir), filename:join([TempDir, "lib", LibDir])} ||
                                            LibDir <- lists:subtract(Libs, AllSystemLibs)];
                                    _ ->
                                        [{"lib", filename:join(TempDir, "lib")}]
                                end;
                            _ ->
                                [{"lib", filename:join(TempDir, "lib")},
                                 {"erts-"++ErtsVersion, filename:join(TempDir, "erts-"++ErtsVersion)}]
                        end]++ExtraFiles, [dereference,compressed]),
    ?log_info("tarball ~s successfully created!", [TarFile]),
    {ok, State}.

%% include each of these config files if they exist
config_files(Vsn, OutputDir) ->
    VMArgs = {filename:join(["releases", Vsn, "vm.args"]),
              filename:join([OutputDir, "releases", Vsn, "vm.args"])},
    VMArgsSrc = {filename:join(["releases", Vsn, "vm.args.src"]),
                 filename:join([OutputDir, "releases", Vsn, "vm.args.src"])},
    VMArgsOrig = {filename:join(["releases", Vsn, "vm.args.orig"]),
                  filename:join([OutputDir, "releases", Vsn, "vm.args.orig"])},
    SysConfigOrig = {filename:join(["releases", Vsn, "sys.config.orig"]),
                     filename:join([OutputDir, "releases", Vsn, "sys.config.orig"])},
    [{NameInArchive, Filename} || {NameInArchive, Filename} <- [VMArgsSrc, VMArgs, VMArgsOrig, SysConfigOrig],
                                  filelib:is_file(Filename)].

%% convert overlays to a list of {NameInArchive, Filename} tuples to pass to `erl_tar' or `make_tar'
overlay_files(_, undefined, _) ->
    [];
overlay_files(OverlayVars, Overlay, OutputDir) ->
    [begin
         To = to(O),
         File = rlx_overlay:render_string(OverlayVars, To),
         {rlx_util:to_string(File), rlx_util:to_string(filename:join(OutputDir, File))}
     end || O <- Overlay, filter(O)].

to({link, _, To}) ->
    To;
to({copy, From, "."}) ->
    filename:basename(From);
to({copy, _, To}) ->
    To;
to({mkdir, To}) ->
    To;
to({template, _, To}) ->
    To.

filter({_, _, "bin/"++_}) ->
    false;
filter({link, _, _}) ->
    true;
filter({copy, _, _}) ->
    true;
filter({mkdir, _}) ->
    true;
filter({template, _, _}) ->
    true;
filter(_) ->
    false.

%% if `include_src' is true then include the `src' and `include' directories of each application
app_dirs(State) ->
    case rlx_state:get(State, include_src, true) of
        false ->
            [];
        true ->
            [include, src]
    end.
