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
         format_error/2]).

-define(DIRECTORY_RE, ".*(\/|\\\\)$").

-define(ERLYDTL_COMPILE_OPTS, [report_warnings, return_errors, {auto_escape, false}, {out_dir, false}]).

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
                                                             {bare, false},
                                                             {deps, ?DEPS},
                                                             {example, "overlay"},
                                                             {short_desc, ""},
                                                             {desc, ""},
                                                             {opts, []}])),
    {ok, State1}.

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State) ->
    {RelName, RelVsn} = rlx_state:default_configured_release(State),
    Release = rlx_state:get_realized_release(State, RelName, RelVsn),
    case rlx_release:realized(Release) of
        true ->
            generate_overlay_vars(State, Release);
        false ->
            ?RLX_ERROR({unresolved_release, RelName, RelVsn})
    end.

-spec format_error(ErrorDetail::term(), rlx_state:t()) -> iolist().
format_error({unresolved_release, RelName, RelVsn}, _) ->
    io_lib:format("The release has not been resolved ~p-~s", [RelName, RelVsn]);
format_error({ec_file_error, AppDir, TargetDir, E}, _) ->
    io_lib:format("Unable to copy OTP App from ~s to ~s due to ~p",
                  [AppDir, TargetDir, E]);
format_error({unable_to_read_varsfile, FileName, Reason}, _) ->
    io_lib:format("Unable to read vars file (~s) for overlay due to: ~p",
                  [FileName, Reason]);
format_error({overlay_failed, Errors}, State) ->
    [[format_error(rlx_util:error_reason(Error), State), "\n"] || Error <- Errors];
format_error({dir_render_failed, Dir, Error}, _) ->
    io_lib:format("rendering mkdir path failed ~s with ~p",
                  [Dir, Error]);
format_error({unable_to_make_symlink, AppDir, TargetDir, Reason}, _) ->
    io_lib:format("Unable to symlink directory ~s to ~s because \n~s~s",
                  [AppDir, TargetDir, rlx_util:indent(2),
                   file:format_error(Reason)]);
format_error({copy_failed, FromFile, ToFile, Err}, _) ->
    io_lib:format("Unable to copy from ~s to ~s because of ~p",
                  [FromFile, ToFile, Err]);
format_error({unable_to_write, ToFile, Reason}, _) ->
    io_lib:format("Unable to write to ~s because ~p",
                  [ToFile, Reason]);
format_error({unable_to_enclosing_dir, ToFile, Reason}, _) ->
    io_lib:format("Unable to create enclosing directory for ~s because ~p",
                  [ToFile, Reason]);
format_error({unable_to_render_template, FromFile, Reason}, _) ->
    io_lib:format("Unable to render template ~s because ~p",
                  [FromFile, Reason]);
format_error({unable_to_compile_template, FromFile, Reason}, State) ->
    io_lib:format("Unable to compile template ~s because \n~s",
                  [FromFile, [format_errors(F, Es, State) || {F, Es} <- Reason]]);
format_error({unable_to_make_dir, Absolute, Error}, _) ->
    io_lib:format("Unable to make directory ~s because ~p",
                  [Absolute, Error]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

format_errors(File, [{none, Mod, E}|Es], State) ->
    [io_lib:format("~s~s: ~ts~n",
                   [rlx_util:indent(2), File,
                    Mod:format_error(E, State)])
     |format_errors(File, Es, State)];
format_errors(File, [{{Line, Col}, Mod, E}|Es], State) ->
    [io_lib:format("~s~s:~w:~w: ~ts~n",
                   [rlx_util:indent(2), File, Line, Col,
                    Mod:format_error(E, State)])
     |format_errors(File, Es, State)];
format_errors(File, [{Line, Mod, E}|Es], State) ->
    [io_lib:format("~s~s:~w: ~ts~n",
                   [rlx_util:indent(2), File, Line,
                    Mod:format_error(E, State)])
     |format_errors(File, Es, State)];
format_errors(_, [], _State) -> [].


-spec generate_overlay_vars(rlx_state:t(), rlx_release:t()) ->
                                   {ok, rlx_state:t()} | relx:error().
generate_overlay_vars(State, Release) ->
    StateVars = generate_state_vars(State),
    ReleaseVars = generate_release_vars(Release),
    get_overlay_vars_from_file(State, StateVars ++ ReleaseVars).

-spec get_overlay_vars_from_file(rlx_state:t(), proplists:proplist()) ->
                                        {ok, rlx_state:t()} | relx:error().
get_overlay_vars_from_file(State, OverlayVars) ->
    case rlx_state:get(State, overlay_vars, undefined) of
        undefined ->
            do_overlay(State, OverlayVars);
        [] ->
            do_overlay(State, OverlayVars);
        [H | _]=FileNames when is_list(H) ->
            read_overlay_vars(State, OverlayVars, FileNames);
        FileName when is_list(FileName) ->
            read_overlay_vars(State, OverlayVars, [FileName])
    end.

-spec read_overlay_vars(rlx_state:t(), proplists:proplist(), [file:name()]) ->
                                   {ok, rlx_state:t()} | relx:error().
read_overlay_vars(State, OverlayVars, FileNames) ->
    Terms = merge_overlay_vars(State, FileNames),
    case render_overlay_vars(OverlayVars, Terms, []) of
        {ok, NewTerms} ->
            do_overlay(State, OverlayVars ++ NewTerms);
        Error ->
            Error
    end.

-spec merge_overlay_vars(rlx_state:t(), [file:name()]) ->
                                proplists:proplist().
merge_overlay_vars(State, FileNames) ->
    RelativeRoot = get_relative_root(State),
    lists:foldl(fun(FileName, Acc) ->
                        RelativePath = filename:join(RelativeRoot, erlang:iolist_to_binary(FileName)),
                        case file:consult(RelativePath) of
                            {ok, Terms} ->
                                lists:ukeymerge(1, lists:ukeysort(1, Terms), Acc);
                            {error, Reason} ->
                                ec_cmd_log:warn(rlx_state:log(State),
                                                format_error({unable_to_read_varsfile, FileName, Reason}, State)),
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
     {release_version, rlx_release:vsn(Release)},
     {release_applications, lists:map(fun(App) ->
                                              rlx_app_info:name(App)
                                        end, rlx_release:application_details(Release))},
    {release, [generate_app_vars(App)|| App <- rlx_release:application_details(Release)]},
     {release_goals,   [if
                            erlang:is_list(Constraint) ->
                                Constraint;
                            true ->
                                rlx_depsolver:format_constraint(Constraint)
                        end || Constraint <- rlx_release:goals(Release)]}].

-spec generate_app_vars(rlx_app_info:t()) -> AppInfo::tuple().
generate_app_vars(App) ->
    {rlx_app_info:name(App),
     [{version, rlx_app_info:original_vsn(App)},
      {dir, rlx_app_info:dir(App)},
      {active_dependencies, rlx_app_info:active_deps(App)},
      {library_dependencies, rlx_app_info:library_deps(App)},
      {link, rlx_app_info:link(App)}]}.

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

-spec do_overlay(rlx_state:t(), proplists:proplist()) ->
                                   {ok, rlx_state:t()} | relx:error().
do_overlay(State, OverlayVars) ->
    case rlx_state:get(State, overlay, undefined) of
        undefined ->
            {ok, State};
        Overlays ->
            handle_errors(State,
                          lists:map(fun(Overlay) ->
                                            do_individual_overlay(State, OverlayVars,
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

-spec do_individual_overlay(rlx_state:t(), proplists:proplist(),
                            OverlayDirective::term()) ->
                                   {ok, rlx_state:t()} | relx:error().
do_individual_overlay(State, OverlayVars, {mkdir, Dir}) ->
    ModuleName = make_template_name("rlx_mkdir_template", Dir),
    case erlydtl:compile(erlang:iolist_to_binary(Dir), ModuleName, ?ERLYDTL_COMPILE_OPTS) of
        {ok, ModuleName} ->
            case render(ModuleName, OverlayVars) of
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
        {error, Reason, _Warnings} ->
            ?RLX_ERROR({unable_to_compile_template, Dir, Reason})
    end;
do_individual_overlay(State, OverlayVars, {copy, From, To}) ->
    FromTemplateName = make_template_name("rlx_copy_from_template", From),
    ToTemplateName = make_template_name("rlx_copy_to_template", To),
    file_render_do(OverlayVars, From, FromTemplateName,
                   fun(FromFile) ->
                           file_render_do(OverlayVars, To, ToTemplateName,
                                          fun(ToFile) ->
                                                  copy_to(State, FromFile, ToFile)
                                          end)
                   end);
do_individual_overlay(State, OverlayVars, {template, From, To}) ->
    FromTemplateName = make_template_name("rlx_template_from_template", From),
    ToTemplateName = make_template_name("rlx_template_to_template", To),
    file_render_do(OverlayVars, From, FromTemplateName,
                   fun(FromFile) ->
                           file_render_do(OverlayVars, To, ToTemplateName,
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
    TemplateName = make_template_name("rlx_template_renderer", Data),
    case erlydtl:compile(Data, TemplateName, ?ERLYDTL_COMPILE_OPTS) of
        Good when Good =:= ok; Good =:= {ok, TemplateName} ->
            case render(TemplateName, OverlayVars) of
                {ok, IoData} ->
                    {ok, IoData};
                {error, Reason} ->
                    ?RLX_ERROR({unable_to_render_template, Data, Reason})
            end;
        {error, Reason, _Warnings} ->
            ?RLX_ERROR({unable_to_compile_template, Data, Reason})
    end.

write_template(OverlayVars, FromFile, ToFile) ->
    case render_template(OverlayVars, FromFile) of
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
    end.

-spec file_render_do(proplists:proplist(), iolist(), module(),
                     fun((term()) -> {ok, rlx_state:t()} | relx:error())) ->
                            {ok, rlx_state:t()} | relx:error().
file_render_do(OverlayVars, Data, TemplateName, NextAction) ->
    case erlydtl:compile(erlang:iolist_to_binary(Data), TemplateName, ?ERLYDTL_COMPILE_OPTS) of
        {ok, TemplateName} ->
            case render(TemplateName, OverlayVars) of
                {ok, IoList} ->
                    NextAction(IoList);
                {error, Error} ->
                    ?RLX_ERROR({render_failed, Data, Error})
            end;
        {error, Reason, _Warnings} ->
            ?RLX_ERROR({unable_to_compile_template, Data, Reason})
    end.

-spec make_template_name(string(), term()) -> module().
make_template_name(Base, Value) ->
    %% Seed so we get different values each time
    random:seed(erlang:now()),
    Hash = erlang:phash2(Value),
    Ran = random:uniform(10000000),
    erlang:list_to_atom(Base ++ "_" ++
                            erlang:integer_to_list(Hash) ++
                            "_" ++ erlang:integer_to_list(Ran)).

-spec render(module(), proplists:proplist()) -> {ok, iolist()} | {error, Reason::term()}.
render(ModuleName, OverlayVars) ->
    try
        ModuleName:render(OverlayVars)
    catch
        _:Reason ->
            {error, Reason}
    end.

absolutize(State, FileName) ->
    filename:absname(filename:join(rlx_state:root_dir(State),
                                   erlang:iolist_to_binary(FileName))).
