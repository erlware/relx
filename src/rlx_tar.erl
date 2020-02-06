-module(rlx_tar).

-export([make_tar/3,
         format_error/1]).

-include("relx.hrl").
-include("rlx_log.hrl").

format_error({tar_unknown_generation_error, Module, Vsn}) ->
    io_lib:format("Tarball generation error of ~s ~s",
                  [Module, Vsn]);
format_error({tar_generation_warn, Module, Warnings}) ->
    io_lib:format("Tarball generation warnings for ~p : ~p",
                  [Module, Warnings]);
format_error({tar_generation_error, Module, Errors}) ->
    io_lib:format("Tarball generation error for ~p", [Module:format_error(Errors)]).

make_tar(State, Release, OutputDir) ->
    Name = atom_to_list(rlx_release:name(Release)),
    Vsn = rlx_release:vsn(Release),
    ErtsVersion = rlx_release:erts(Release),
    Opts = [{path, [filename:join([OutputDir, "lib", "*", "ebin"])]},
            {dirs, [include | maybe_src_dirs(State)]},
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
    IncludeErts = rlx_state:get(State, include_erts, true),
    SystemLibs = rlx_state:get(State, include_system_libs, IncludeErts),
    {RelName, RelVsn} = rlx_state:default_configured_release(State),
    Release = rlx_state:get_realized_release(State, RelName, RelVsn),
    TarFile = filename:join(OutputDir, Name++"-"++Vsn++".tar.gz"),
    file:rename(filename:join(OutputDir, Name++".tar.gz"), TarFile),
    erl_tar:extract(TarFile, [{cwd, TempDir}, compressed]),
    OverlayVars = rlx_overlay:generate_overlay_vars(State, Release),
    OverlayFiles = overlay_files(OverlayVars, rlx_state:get(State, overlay, undefined), OutputDir),
    ConfigFiles = config_files(Vsn, OutputDir),
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
                                 {"erts-"++ErtsVersion, filename:join(OutputDir, "erts-"++ErtsVersion)}]
                        end]++ConfigFiles++OverlayFiles, [dereference,compressed]),
    ?log_info("tarball ~s successfully created!", [TarFile]),
    ec_file:remove(TempDir, [recursive]),
    {ok, State}.

config_files(Vsn, OutputDir) ->
    VMArgs = {filename:join(["releases", Vsn, "vm.args"]), filename:join([OutputDir, "releases", Vsn, "vm.args"])},
    VMArgsSrc = {filename:join(["releases", Vsn, "vm.args.src"]), filename:join([OutputDir, "releases", Vsn, "vm.args.src"])},
    VMArgsOrig = {filename:join(["releases", Vsn, "vm.args.orig"]), filename:join([OutputDir, "releases", Vsn, "vm.args.orig"])},
    SysConfigOrig = {filename:join(["releases", Vsn, "sys.config.orig"]), filename:join([OutputDir, "releases", Vsn, "sys.config.orig"])},
    [{NameInArchive, Filename} || {NameInArchive, Filename} <- [VMArgsSrc, VMArgs, VMArgsOrig, SysConfigOrig], filelib:is_file(Filename)].


overlay_files(_, undefined, _) ->
    [];
overlay_files(OverlayVars, Overlay, OutputDir) ->
    [begin
         To = to(O),
         File = rlx_overlay:render_string(OverlayVars, To),
         {ec_cnv:to_list(File), ec_cnv:to_list(filename:join(OutputDir, File))}
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

maybe_src_dirs(State) ->
    case rlx_state:get(State, include_src, true) of
        false -> [];
        true -> [src]
    end.
