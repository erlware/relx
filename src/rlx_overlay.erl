-module(rlx_overlay).

-export([render/2,
         format_error/1]).

-export([generate_overlay_vars/2,
         render_string/2]).

-define(DIRECTORY_RE, ".*(\/|\\\\)$").

-include("relx.hrl").
-include("rlx_log.hrl").

render(Release, State) ->
    case generate_overlay_vars(State, Release) of
        {error, Reason} ->
            {error, Reason};
        OverlayVars ->
            Files = rlx_util:template_files(),
            do_overlay(State, Files, OverlayVars)
    end.

-spec format_error(ErrorDetail::term()) -> iolist().
format_error({unresolved_release, RelName, RelVsn}) ->
    io_lib:format("The release has not been resolved ~p-~s", [RelName, RelVsn]);
format_error({ec_file_error, AppDir, TargetDir, E}) ->
    io_lib:format("Unable to copy OTP App from ~s to ~s due to ~p",
                  [AppDir, TargetDir, E]);
format_error({unable_to_read_varsfile, FileName, Reason}) ->
    io_lib:format("Unable to read vars file (~s) for overlay due to: ~p",
                  [FileName, Reason]);
format_error({read_template, FileName, Reason}) ->
    io_lib:format("Unable to read template file (~s) for overlay due to: ~s",
                  [FileName, file:format_error(Reason)]);
format_error({overlay_failed, Errors}) ->
    [[format_error(rlx_util:error_reason(Error)), "\n"] || Error <- Errors];
format_error({unable_to_make_symlink, AppDir, TargetDir, Reason}) ->
    io_lib:format("Unable to symlink directory ~s to ~s because \n~s~s",
                  [AppDir, TargetDir, rlx_util:indent(2),
                   file:format_error(Reason)]);
format_error({copy_failed, FromFile, ToFile, Err}) ->
    io_lib:format("Unable to copy from ~s to ~s because of ~p",
                  [FromFile, ToFile, Err]);
format_error({unable_to_write, ToFile, Reason}) ->
    io_lib:format("Unable to write to ~s because ~p",
                  [ToFile, Reason]);
format_error({unable_to_enclosing_dir, ToFile, Reason}) ->
    io_lib:format("Unable to create enclosing directory for ~s because ~p",
                  [ToFile, Reason]);
format_error({unable_to_render_template, FromFile, Reason}) ->
    io_lib:format("Unable to render template ~s because ~p",
                  [FromFile, Reason]);
format_error({unable_to_compile_template, FromFile, Reason}) ->
    io_lib:format("Unable to compile template ~s because \n~s",
                  [FromFile, [format_errors(F, Es) || {F, Es} <- Reason]]);
format_error({unable_to_make_dir, Absolute, Error}) ->
    io_lib:format("Unable to make directory ~s because ~p",
                  [Absolute, Error]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

format_errors(File, [{none, Mod, E}|Es]) ->
    [io_lib:format("~s~s: ~ts~n",
                   [rlx_util:indent(2), File,
                    Mod:format_error(E)])
     |format_errors(File, Es)];
format_errors(File, [{{Line, Col}, Mod, E}|Es]) ->
    [io_lib:format("~s~s:~w:~w: ~ts~n",
                   [rlx_util:indent(2), File, Line, Col,
                    Mod:format_error(E)])
     |format_errors(File, Es)];
format_errors(File, [{Line, Mod, E}|Es]) ->
    [io_lib:format("~s~s:~w: ~ts~n",
                   [rlx_util:indent(2), File, Line,
                    Mod:format_error(E)])
     |format_errors(File, Es)];
format_errors(_, []) -> [].


-spec generate_overlay_vars(rlx_state:t(), rlx_release:t()) ->
                                   proplists:proplist() | relx:error().
generate_overlay_vars(State, Release) ->
    StateVars = generate_state_vars(State),
    ReleaseVars = generate_release_vars(Release),
    get_overlay_vars_from_file(State, StateVars ++ ReleaseVars).

-spec get_overlay_vars_from_file(rlx_state:t(), proplists:proplist()) ->
                                        proplists:proplist() | relx:error().
get_overlay_vars_from_file(State, OverlayVars) ->
    case rlx_state:get(State, overlay_vars, undefined) of
        undefined ->
            OverlayVars;
        [] ->
            OverlayVars;
        [H | _]=FileNames when is_list(H) ;
                               is_tuple(H) ->
            read_overlay_vars(State, OverlayVars, FileNames);
        FileName when is_list(FileName) ->
            read_overlay_vars(State, OverlayVars, [FileName])
    end.

-spec read_overlay_vars(rlx_state:t(), proplists:proplist(), [file:name()]) ->
                               proplists:proplist() | relx:error().
read_overlay_vars(State, OverlayVars, FileNames) ->
    OverlayVarsValues = rlx_state:get(State, overlay_vars_values),
    Terms = merge_overlay_vars(State, FileNames),
    case render_overlay_vars(OverlayVars ++ OverlayVarsValues, Terms, []) of
        {ok, NewTerms} ->
            % We place `OverlayVarsvalues' at the end on purpose; their
            % definitions should be overwrittenable by both internal
            % and rendered vars, as not to change behaviour in
            % setups preceding the support for overlays from the caller.
            OverlayVars ++ NewTerms ++ OverlayVarsValues;
        Error ->
            Error
    end.

-spec check_overlay_inclusion(rlx_state:t(), string(), proplists:proplist()) ->
                              proplists:proplist().
check_overlay_inclusion(State, RelativeRoot, Terms) ->
    check_overlay_inclusion(State, RelativeRoot, Terms, []).

-spec check_overlay_inclusion(rlx_state:t(), string(), proplists:proplist(), proplists:proplist()) ->
                              proplists:proplist().
check_overlay_inclusion(State, RelativeRoot, [File|T], Terms) when is_list(File) ->
    IncludedTerms = merge_overlay_vars(State, [filename:join(RelativeRoot, File)]),
    check_overlay_inclusion(State, RelativeRoot, T, Terms ++ IncludedTerms);
check_overlay_inclusion(State, RelativeRoot, [Tuple|T], Terms) ->
    check_overlay_inclusion(State, RelativeRoot, T, Terms ++ [Tuple]);
check_overlay_inclusion(_State, _RelativeRoot, [], Terms) ->
    Terms.

-spec merge_overlay_vars(rlx_state:t(), [file:name()]) ->
                                proplists:proplist().
merge_overlay_vars(State, FileNames) ->
    RelativeRoot = get_relative_root(State),
    lists:foldl(fun(FileName, Acc) when is_list(FileName) ->
                        RelativePath = filename:join(RelativeRoot, iolist_to_binary(FileName)),
                        case file:consult(RelativePath) of
                            {ok, Terms} ->
                                %% the location of the included overlay files will be relative
                                %% to the current one being read
                                OverlayRelativeRoot = filename:dirname(FileName),
                                NewTerms = check_overlay_inclusion(State, OverlayRelativeRoot, Terms),
                                %% Remove already defined variables from Acc,
                                %% append NewTerms, preserving order
                                lists:foldl(fun(NewTerm, A) ->
                                                    lists:keydelete(element(1, NewTerm), 1, A)
                                            end, Acc, NewTerms) ++ NewTerms;
                            {error, Reason} ->
                                ?log_warn(format_error({unable_to_read_varsfile, FileName, Reason})),
                                Acc
                        end;
                   (Var, Acc) ->
                        lists:keystore(element(1, Var), 1, Acc, Var)
                end, [], FileNames).

-spec render_overlay_vars(proplists:proplist(), proplists:proplist(),
                          proplists:proplist()) ->
                                 {ok, proplists:proplist()} | relx:error().
render_overlay_vars(OverlayVars, [{Key, Value} | Rest], Acc)
  when erlang:is_list(Value) ->
    case io_lib:printable_list(Value) of
        true ->
            case render_template(Acc ++ OverlayVars, erlang:iolist_to_binary(Value)) of
                {ok, Data} ->
                    %% Adding to the end sucks, but ordering needs to be retained
                    render_overlay_vars(OverlayVars, Rest, Acc ++ [{Key, Data}]);
                Error ->
                    Error
            end;
        false ->
            case render_overlay_vars(Acc ++ OverlayVars, Value, []) of
                {ok, NewValue} ->
                    render_overlay_vars(OverlayVars, Rest, Acc ++ [{Key, NewValue}]);
                Error ->
                    Error
            end
    end;
render_overlay_vars(OverlayVars, [{Key, Value} | Rest], Acc)
  when erlang:is_binary(Value) ->
    case render_template(Acc ++ OverlayVars, erlang:iolist_to_binary(Value)) of
        {ok, Data} ->
            render_overlay_vars(OverlayVars, Rest, Acc ++ [{Key, erlang:iolist_to_binary(Data)}]);
        Error ->
            Error
    end;
render_overlay_vars(OverlayVars, [KeyValue | Rest], Acc) ->
    render_overlay_vars(OverlayVars, Rest, Acc ++ [KeyValue]);
render_overlay_vars(_OverlayVars, [], Acc) ->
    {ok, Acc}.

-spec generate_release_vars(rlx_release:t()) -> proplists:proplist().
generate_release_vars(Release) ->
    [{erts_vsn, rlx_release:erts(Release)},
     {erts_dir, code:root_dir()},
     {release_erts_version, rlx_release:erts(Release)},
     {release_name, rlx_release:name(Release)},
     {rel_vsn, rlx_release:vsn(Release)},
     {release_version, rlx_release:vsn(Release)}].

-spec generate_state_vars(rlx_state:t()) -> proplists:proplist().
generate_state_vars(State) ->
    [{output_dir, rlx_state:output_dir(State)},
     {target_dir, rlx_state:output_dir(State)},
     {overridden, [AppName || {AppName, _} <- rlx_state:overrides(State)]},
     {overrides, rlx_state:overrides(State)},
     {lib_dirs, rlx_state:lib_dirs(State)},
     {vm_args, rlx_state:vm_args(State)},
     {sys_config, rlx_state:sys_config(State)},
     {root_dir, rlx_state:root_dir(State)},
     {default_release_name, case rlx_state:default_configured_release(State) of
                                {Name0, _} ->
                                    Name0
                            end},
     {default_release_version, case rlx_state:default_configured_release(State) of
                                   {_, Vsn0} ->
                                       Vsn0
                               end},
     {default_release, case rlx_state:default_configured_release(State) of
                           {Name1, undefined} ->
                               erlang:atom_to_list(Name1);
                           {Name1, Vsn1} ->
                               erlang:atom_to_list(Name1) ++ "-" ++ Vsn1
                       end}].

-spec do_overlay(rlx_state:t(), list(), proplists:proplist()) ->
                                   {ok, rlx_state:t()} | relx:error().
do_overlay(State, Files, OverlayVars) ->
    case rlx_state:get(State, overlay, undefined) of
        undefined ->
            {ok, State};
        Overlays ->
            handle_errors(State,
                          lists:map(fun(Overlay) ->
                                            do_individual_overlay(State, Files, OverlayVars,
                                                                  Overlay)
                                    end, Overlays))
    end.

-spec handle_errors(rlx_state:t(), [ok | relx:error()]) ->
                           {ok, rlx_state:t()} | relx:error().
handle_errors(State, Result) ->
    case [Error || Error <- Result,
                   rlx_util:is_error(Error)] of
        Errors = [_|_] ->
            ?RLX_ERROR({overlay_failed, Errors});
        [] ->
            {ok, State}
    end.

-spec do_individual_overlay(rlx_state:t(), list(), proplists:proplist(),
                            OverlayDirective::term()) ->
                                   {ok, rlx_state:t()} | relx:error().
do_individual_overlay(State, _Files, OverlayVars, {chmod, Mode, Path}) ->
    % mode can be specified directly as an integer value, or if it is
    % not an integer we assume it's a template, which we render and convert
    % blindly to an integer.  So this will crash with an exception if for
    % some reason something other than an integer is used
    NewMode =
        case is_integer(Mode) of
            true -> Mode;
            false -> erlang:binary_to_integer(render_string (OverlayVars, Mode))
        end,

    file_render_do(OverlayVars, Path,
                   fun(NewPath) ->
                            Absolute = absolute_path_to(State, NewPath),
                            case file:change_mode(Absolute, NewMode) of
                                {error, Error} ->
                                    ?RLX_ERROR({unable_to_chmod, NewMode, NewPath, Error});
                                ok -> ok
                            end
                   end);
do_individual_overlay(State, _Files, OverlayVars, {mkdir, Dir}) ->
    file_render_do(OverlayVars, Dir,
                   fun(Dir0) ->
                       Absolute = absolute_path_to(State, Dir0),
                       case rlx_util:mkdir_p(Absolute) of
                           {error, Error} ->
                               ?RLX_ERROR({unable_to_make_dir, Absolute, Error});
                           ok ->
                               ok
                       end
                   end);
do_individual_overlay(State, _Files, OverlayVars, {copy, From, To}) ->
    file_render_do(OverlayVars, From,
                   fun(FromFile) ->
                           file_render_do(OverlayVars, To,
                                          fun(ToFile) ->
                                                  copy_to(State, FromFile, ToFile)
                                          end)
                   end);
do_individual_overlay(State, Files, OverlayVars, {link, From, To}) ->
    case rlx_state:dev_mode(State) of
        false ->
            do_individual_overlay(State, Files, OverlayVars, {copy, From, To});
        true  ->
            file_render_do(OverlayVars, From,
                           fun(FromFile) ->
                                   file_render_do(OverlayVars, To,
                                                  fun(ToFile) ->
                                                          link_to(State, FromFile, ToFile)
                                                  end)
                           end)
    end;
do_individual_overlay(State, _Files, OverlayVars, {template, From, To}) ->
    file_render_do(OverlayVars, From,
                   fun(FromFile) ->
                           file_render_do(OverlayVars, To,
                                          fun(ToFile) ->
                                                  write_template(OverlayVars,
                                                                 absolute_path_from(State, FromFile),
                                                                 absolute_path_to(State, ToFile))
                                          end)
                   end).

-spec wildcard_copy(rlx_state:t(), file:filename_all(), file:filename_all(),
      fun((file:filename_all(), file:filename_all()) -> ok | {error, term()}),
      ErrorTag :: atom()) -> ok | relx:error().
wildcard_copy(State, FromFile0, ToFile0, CopyFun, ErrorTag) ->
    FromFile1 = absolute_path_from(State, FromFile0),
    ToFile1 = absolute_path_to(State, ToFile0),

    Res = case is_directory(ToFile0, ToFile1) of
              false ->
                  filelib:ensure_dir(ToFile1),
                  CopyFun(FromFile1, ToFile1);
              true ->
                  Root = absolute_path_from(State, "."),
                  FromFiles = if
                      is_list(FromFile0) -> filelib:wildcard(FromFile0, Root);
                      true -> [FromFile1]
                  end,
                  rlx_util:mkdir_p(ToFile1),
                  lists:foldl(fun
                      (_, {error, _} = Error) -> Error;
                      (FromFile, ok) ->
                          CopyFun(filename:join(Root, FromFile), filename:join(ToFile1, filename:basename(FromFile)))
                  end, ok, FromFiles)
    end,
                  
    case Res of
        ok ->
            ok;
        {error, Err} ->
            ?RLX_ERROR({ErrorTag,
                        FromFile1,
                        ToFile1, Err})
    end.


-spec copy_to(rlx_state:t(), file:name(), file:name()) -> ok | relx:error().
copy_to(State, FromFile0, ToFile0) ->
    wildcard_copy(State, FromFile0, ToFile0,
                  fun(FromPath, ToPath) -> ec_file:copy(FromPath, ToPath, [recursive, {file_info, [mode, time]}]) end,
                  copy_failed).

-spec link_to(rlx_state:t(), file:name(), file:name()) -> ok | relx:error().
link_to(State, FromFile0, ToFile0) ->
    wildcard_copy(State, FromFile0, ToFile0,
                  fun make_link/2,
                  link_failed).

make_link(FromFile, ToFile) ->
    case ec_file:is_symlink(ToFile) of
        true -> file:delete(ToFile);
        false -> ec_file:remove(ToFile, [recursive])
    end,
    file:make_symlink(FromFile, ToFile).

get_relative_root(State) ->
    rlx_state:root_dir(State).

absolute_path_from(State, Path) ->
    absolutize(State, filename:join(get_relative_root(State), Path)).

absolute_path_to(State, Path) ->
    absolutize(State, filename:join(rlx_state:output_dir(State), Path)).

-spec is_directory(file:name(), file:name()) -> boolean().
is_directory(ToFile0, ToFile1) ->
    case re:run(ToFile0, ?DIRECTORY_RE) of
        nomatch ->
            filelib:is_dir(ToFile1);
        _ ->
            true
    end.

-spec render_template(proplists:proplist(), binary()) ->
                             {ok, binary()} | relx:error().
render_template(OverlayVars, Data) ->
    case rlx_util:render(Data, OverlayVars) of
        {ok, IoData} ->
            {ok, IoData};
        {error, Reason} ->
            ?RLX_ERROR({unable_to_render_template, Data, Reason})
    end.

-spec write_template(proplists:proplist(), file:name(), file:name()) ->
                            ok | relx:error().
write_template(OverlayVars, FromFile, ToFile) ->
    case file:read_file(FromFile) of
        {ok, File} ->
            case render_template(OverlayVars, File) of
                {ok, IoData} ->
                    case filelib:ensure_dir(ToFile) of
                        ok ->
                            %% we were asked to render a template
                            %% onto a symlink, this would cause an overwrite
                            %% of the original file, so we delete the symlink
                            %% and go ahead with the template render
                            case ec_file:is_symlink(ToFile) of
                                true -> ec_file:remove(ToFile);
                                false -> ok
                            end,
                            case file:write_file(ToFile, IoData) of
                                ok ->
                                    ok = ec_file:copy_file_info(ToFile, FromFile, [mode, time]),
                                    ok;
                                {error, Reason} ->
                                    ?RLX_ERROR({unable_to_write, ToFile, Reason})
                            end;
                        {error, Reason} ->
                            ?RLX_ERROR({unable_to_enclosing_dir, ToFile, Reason})
                    end;
                Error ->
                    Error
            end;
        {error, Error} ->
            ?RLX_ERROR({read_template, FromFile, Error})
    end.

-spec render_string(proplists:proplist(), iolist()) ->
                      binary() | relx:error().
render_string(OverlayVars, Data) ->
    case rlx_util:render(Data, OverlayVars) of
        {ok, Bin} -> Bin;
        {error, Error} ->
            ?RLX_ERROR({render_failed, Data, Error})
    end.

-spec file_render_do(proplists:proplist(), iolist(),
                     fun((string() | binary()) -> {ok, rlx_state:t()} | relx:error())) ->
                            {ok, rlx_state:t()} | relx:error().
file_render_do(OverlayVars, File, NextAction) ->
    case rlx_util:render(File, OverlayVars) of
        {ok, Binary} when is_binary(File) ->
            NextAction(Binary);
        {ok, Binary} when is_list(File) ->
            NextAction(binary_to_list(Binary));
        {error, Error} ->
            ?RLX_ERROR({render_failed, File, Error})
    end.

absolutize(State, FileName) ->
    filename:absname(filename:join(rlx_state:root_dir(State), FileName)).
