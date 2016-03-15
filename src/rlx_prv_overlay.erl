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
-module(rlx_prv_overlay).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([generate_overlay_vars/2,
         render_string/2]).

-define(DIRECTORY_RE, ".*(\/|\\\\)$").

-include("relx.hrl").

-define(PROVIDER, overlay).
-define(DEPS, [resolve_release]).

%%============================================================================
%% API
%%============================================================================

-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    State1 = rlx_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {deps, ?DEPS}])),
    {ok, State1}.

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State) ->
    {RelName, RelVsn} = rlx_state:default_configured_release(State),
    Release = rlx_state:get_realized_release(State, RelName, RelVsn),
    case rlx_release:realized(Release) of
        true ->
            case generate_overlay_vars(State, Release) of
                {error, Reason} ->
                    {error, Reason};
                OverlayVars ->
                    Files = rlx_util:template_files(),
                    do_overlay(State, Files, OverlayVars)
            end;
        false ->
            ?RLX_ERROR({unresolved_release, RelName, RelVsn})
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
format_error({dir_render_failed, Dir, Error}) ->
    io_lib:format("rendering mkdir path failed ~s with ~p",
                  [Dir, Error]);
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
        [H | _]=FileNames when is_list(H) ->
            read_overlay_vars(State, OverlayVars, FileNames);
        FileName when is_list(FileName) ->
            read_overlay_vars(State, OverlayVars, [FileName])
    end.

-spec read_overlay_vars(rlx_state:t(), proplists:proplist(), [file:name()]) ->
                               proplists:proplist() | relx:error().
read_overlay_vars(State, OverlayVars, FileNames) ->
    Terms = merge_overlay_vars(State, FileNames),
    case render_overlay_vars(OverlayVars, Terms, []) of
        {ok, NewTerms} ->
            OverlayVars ++ NewTerms;
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
    lists:foldl(fun(FileName, Acc) ->
                        RelativePath = filename:join(RelativeRoot, erlang:iolist_to_binary(FileName)),
                        case file:consult(RelativePath) of
                            %% {ok, [Terms]} ->
                            %%     lists:ukeymerge(1, lists:ukeysort(1, Terms), Acc);
                            %%     % the location of the included overlay files will be relative
                            %%     %% to the current one being read
                            %%     %% OverlayRelativeRoot = filename:dirname(FileName),
                            %%     %% NewTerms = check_overlay_inclusion(State, OverlayRelativeRoot, Terms),

                            %%     %% lists:ukeymerge(1, lists:ukeysort(1, NewTerms), Acc);
                            {ok, Terms} ->
                                %% the location of the included overlay files will be relative
                                %% to the current one being read
                                OverlayRelativeRoot = filename:dirname(FileName),
                                NewTerms = check_overlay_inclusion(State, OverlayRelativeRoot, Terms),
                                lists:foldl(fun(NewTerm, A) ->
                                                lists:keystore(element(1, NewTerm), 1, A, NewTerm)
                                            end, Acc, NewTerms);
                            {error, Reason} ->
                                ec_cmd_log:warn(rlx_state:log(State),
                                                format_error({unable_to_read_varsfile, FileName, Reason})),
                                Acc
                        end
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
     {release_erts_version, rlx_release:erts(Release)},
     {release_name, rlx_release:name(Release)},
     {rel_vsn, rlx_release:vsn(Release)},
     {release_version, rlx_release:vsn(Release)}].

-spec generate_state_vars(rlx_state:t()) -> proplists:proplist().
generate_state_vars(State) ->
    [{log, ec_cmd_log:format(rlx_state:log(State))},
     {output_dir, rlx_state:output_dir(State)},
     {target_dir, rlx_state:output_dir(State)},
     {overridden, [AppName || {AppName, _} <- rlx_state:overrides(State)]},
     {overrides, rlx_state:overrides(State)},
     {goals, [rlx_depsolver:format_constraint(Constraint) ||
                 Constraint <- rlx_state:goals(State)]},
     {lib_dirs, rlx_state:lib_dirs(State)},
     {config_file, rlx_state:config_file(State)},
     {providers, rlx_state:providers(State)},
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
do_individual_overlay(State, _Files, OverlayVars, {mkdir, Dir}) ->
    case rlx_util:render(erlang:iolist_to_binary(Dir), OverlayVars) of
        {ok, IoList} ->
            Absolute = absolutize(State,
                                  filename:join(rlx_state:output_dir(State),
                                                erlang:iolist_to_binary(IoList))),
            case rlx_util:mkdir_p(Absolute) of
                {error, Error} ->
                    ?RLX_ERROR({unable_to_make_dir, Absolute, Error});
                ok ->
                    ok
            end;
        {error, Error} ->
            ?RLX_ERROR({dir_render_failed, Dir, Error})
    end;
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
                                                  RelativeRoot = get_relative_root(State),
                                                  FromFile0 = absolutize(State,
                                                                         filename:join(RelativeRoot,
                                                                                       erlang:iolist_to_binary(FromFile))),
                                                  FromFile1 = erlang:binary_to_list(FromFile0),
                                                  write_template(OverlayVars,
                                                                 FromFile1,
                                                                 absolutize(State,
                                                                            filename:join(rlx_state:output_dir(State),
                                                                                          erlang:iolist_to_binary(ToFile))))
                                          end)
                   end).

-spec copy_to(rlx_state:t(), file:name(), file:name()) -> ok | relx:error().
copy_to(State, FromFile0, ToFile0) ->
    RelativeRoot = get_relative_root(State),
    ToFile1 = absolutize(State, filename:join(rlx_state:output_dir(State),
                                              erlang:iolist_to_binary(ToFile0))),

    FromFile1 = absolutize(State, filename:join(RelativeRoot,
                                                erlang:iolist_to_binary(FromFile0))),
    ToFile2 = case is_directory(ToFile0, ToFile1) of
                  false ->
                      filelib:ensure_dir(ToFile1),
                      ToFile1;
                  true ->
                      rlx_util:mkdir_p(ToFile1),
                      erlang:iolist_to_binary(filename:join(ToFile1,
                                                            filename:basename(FromFile1)))
              end,
    case ec_file:copy(FromFile1, ToFile2, [recursive]) of
        ok ->
            {ok, FileInfo} = file:read_file_info(FromFile1),
            ok = file:write_file_info(ToFile2, FileInfo),
            ok;
        {error, Err} ->
            ?RLX_ERROR({copy_failed,
                        FromFile1,
                        ToFile1, Err})
    end.

-spec link_to(rlx_state:t(), file:name(), file:name()) -> ok | relx:error().
link_to(State, FromFile0, ToFile0) ->
    RelativeRoot = get_relative_root(State),
    ToFile1 = absolutize(State, filename:join(rlx_state:output_dir(State),
                                              erlang:iolist_to_binary(ToFile0))),

    FromFile1 = absolutize(State, filename:join(RelativeRoot,
                                                erlang:iolist_to_binary(FromFile0))),
    ToFile2 = case is_directory(ToFile0, ToFile1) of
                  false ->
                      filelib:ensure_dir(ToFile1),
                      ToFile1;
                  true ->
                      rlx_util:mkdir_p(ToFile1),
                      erlang:iolist_to_binary(filename:join(ToFile1,
                                                            filename:basename(FromFile1)))
              end,
    case ec_file:is_symlink(ToFile2) of
        true  -> file:delete(ToFile2);
        false -> ec_file:remove(ToFile2, [recursive])
    end,
    case file:make_symlink(FromFile1, ToFile2) of
        ok -> ok;
        {error, Err} ->
            ?RLX_ERROR({link_failed,
                        FromFile1,
                        ToFile1, Err})
    end.

get_relative_root(State) ->
    case rlx_state:config_file(State) of
        [] ->
            rlx_state:root_dir(State);
        Config ->
            case filelib:is_regular(Config) of
                true ->
                    filename:dirname(Config);
                false ->
                    rlx_state:root_dir(State)
            end
    end.

-spec is_directory(file:name(), file:name()) -> boolean().
is_directory(ToFile0, ToFile1) ->
    case re:run(ToFile0, ?DIRECTORY_RE) of
        nomatch ->
            filelib:is_dir(ToFile1);
        _ ->
            true
    end.


-spec render_template(proplists:proplist(), iolist()) ->
                             ok | relx:error().
render_template(OverlayVars, Data) ->
    case rlx_util:render(Data, OverlayVars) of
        {ok, IoData} ->
            {ok, IoData};
        {error, Reason} ->
            ?RLX_ERROR({unable_to_render_template, Data, Reason})
    end.

write_template(OverlayVars, FromFile, ToFile) ->
    case file:read_file(FromFile) of
        {ok, File} ->
            case render_template(OverlayVars, File) of
                {ok, IoData} ->
                    case filelib:ensure_dir(ToFile) of
                        ok ->
                            case file:write_file(ToFile, IoData) of
                                ok ->
                                    {ok, FileInfo} = file:read_file_info(FromFile),
                                    ok = file:write_file_info(ToFile, FileInfo),
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

render_string(OverlayVars, Data) ->
    case rlx_util:render(Data, OverlayVars) of
        {ok, IoList} ->
            erlang:iolist_to_binary(IoList);
        {error, Error} ->
            ?RLX_ERROR({render_failed, Data, Error})
    end.

-spec file_render_do(proplists:proplist(), iolist(),
                     fun((term()) -> {ok, rlx_state:t()} | relx:error())) ->
                            {ok, rlx_state:t()} | relx:error().
file_render_do(OverlayVars, File, NextAction) ->
    case rlx_util:render(File, OverlayVars) of
        {ok, IoList} ->
            NextAction(IoList);
        {error, Error} ->
            ?RLX_ERROR({render_failed, File, Error})
    end.

absolutize(State, FileName) ->
    filename:absname(filename:join(rlx_state:root_dir(State),
                                   erlang:iolist_to_binary(FileName))).
