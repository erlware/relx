%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
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
-module(rcl_prv_overlay).

-behaviour(rcl_provider).

-export([init/1,
         do/1,
         format_error/1]).

-include_lib("relcool/include/relcool.hrl").

%%============================================================================
%% API
%%============================================================================
-spec init(rcl_state:t()) -> {ok, rcl_state:t()}.
init(State) ->
    {ok, State}.

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rcl_state:t()) -> {ok, rcl_state:t()} | relcool:error().
do(State) ->
    {RelName, RelVsn} = rcl_state:default_release(State),
    Release = rcl_state:get_release(State, RelName, RelVsn),
    case rcl_release:realized(Release) of
        true ->
            generate_overlay_vars(State, Release);
        false ->
            ?RCL_ERROR({unresolved_release, RelName, RelVsn})
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
format_error({overlay_failed, Errors}) ->
    [[format_error(rcl_util:error_reason(Error)), "\n"] || Error <- Errors];
format_error({dir_render_failed, Dir, Error}) ->
    io_lib:format("rendering mkdir path failed ~s with ~p",
                  [Dir, Error]);
format_error({unable_to_make_symlink, AppDir, TargetDir, Reason}) ->
    io_lib:format("Unable to symlink directory ~s to ~s because \n~s~s",
                  [AppDir, TargetDir, rcl_util:indent(1),
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
    io_lib:format("Unable to compile template ~s because ~p",
                  [FromFile, Reason]);
format_error({unable_to_make_dir, Absolute, Error}) ->
    io_lib:format("Unable to make directory ~s because ~p",
                  [Absolute, Error]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec generate_overlay_vars(rcl_state:t(), rcl_release:t()) ->
                                   {ok, rcl_state:t()} | relcool:error().
generate_overlay_vars(State, Release) ->
    StateVars = generate_state_vars(State),
    ReleaseVars = generate_release_vars(Release),
    get_overlay_vars_from_file(State, StateVars ++ ReleaseVars).

-spec get_overlay_vars_from_file(rcl_state:t(), proplists:proplist()) ->
                                        {ok, rcl_state:t()} | relcool:error().
get_overlay_vars_from_file(State, OverlayVars) ->
    case rcl_state:get(State, overlay_vars, undefined) of
        undefined ->
            do_overlay(State, OverlayVars);
        FileName ->
            read_overlay_vars(State, OverlayVars, FileName)
    end.

-spec read_overlay_vars(rcl_state:t(), proplists:proplist(), file:name()) ->
                                   {ok, rcl_state:t()} | relcool:error().
read_overlay_vars(State, OverlayVars, FileName) ->
    case file:consult(FileName) of
        {ok, Terms} ->
            do_overlay(State, OverlayVars ++ Terms);
        {error, Reason} ->
            ?RCL_ERROR({unable_to_read_varsfile, FileName, Reason})
    end.

-spec generate_release_vars(rcl_release:t()) -> proplists:proplist().
generate_release_vars(Release) ->
    [{erts_vsn, rcl_release:erts(Release)},
     {release_erts_version, rcl_release:erts(Release)},
     {release_name, rcl_release:name(Release)},
     {rel_vsn, rcl_release:vsn(Release)},
     {release_version, rcl_release:vsn(Release)},
     {release_applications, lists:map(fun(App) ->
                                              rcl_app_info:name(App)
                                        end, rcl_release:application_details(Release))},
    {release, [generate_app_vars(App)|| App <- rcl_release:application_details(Release)]},
     {release_goals,   [if
                            erlang:is_list(Constraint) ->
                                Constraint;
                            true ->
                                rcl_depsolver:format_constraint(Constraint)
                        end || Constraint <- rcl_release:goals(Release)]}].

-spec generate_app_vars(rcl_app_info:t()) -> AppInfo::tuple().
generate_app_vars(App) ->
    {rcl_app_info:name(App),
     [{version, rcl_app_info:vsn_as_string(App)},
      {dir, rcl_app_info:dir(App)},
      {active_dependencies, rcl_app_info:active_deps(App)},
      {library_dependencies, rcl_app_info:library_deps(App)},
      {link, rcl_app_info:link(App)}]}.

-spec generate_state_vars(rcl_state:t()) -> proplists:proplist().
generate_state_vars(State) ->
    [{log, rcl_log:format(rcl_state:log(State))},
     {output_dir, rcl_state:output_dir(State)},
     {target_dir, rcl_state:output_dir(State)},
     {overridden, [AppName || {AppName, _} <- rcl_state:overrides(State)]},
     {overrides, rcl_state:overrides(State)},
     {goals, [rcl_depsolver:format_constraint(Constraint) ||
                 Constraint <- rcl_state:goals(State)]},
     {lib_dirs, rcl_state:lib_dirs(State)},
     {config_files, rcl_state:config_files(State)},
     {providers, rcl_state:providers(State)},
     {sys_config, rcl_state:sys_config(State)},
     {root_dir, rcl_state:root_dir(State)},
     {default_release_name, case rcl_state:default_release(State) of
                                {Name0, _} ->
                                    Name0
                            end},
     {default_release_version, case rcl_state:default_release(State) of
                                   {_, Vsn0} ->
                                       Vsn0
                               end},
     {default_release, case rcl_state:default_release(State) of
                           {Name1, undefined} ->
                               erlang:atom_to_list(Name1);
                           {Name1, Vsn1} ->
                               erlang:atom_to_list(Name1) ++ "-" ++ Vsn1
                       end}].

-spec do_overlay(rcl_state:t(), proplists:proplist()) ->
                                   {ok, rcl_state:t()} | relcool:error().
do_overlay(State, OverlayVars) ->
    case rcl_state:get(State, overlay, undefined) of
        undefined ->
            {ok, State};
        Overlays ->
            handle_errors(State,
                          ec_plists:map(fun(Overlay) ->
                                                io:format("--->Doing ~p~n", [Overlay]),
                                                Res = do_individual_overlay(State, OverlayVars,
                                                                            Overlay),
                                                io:format("-->Done ~p~n", [Overlay]),
                                                Res
                                        end, Overlays))
    end.

-spec handle_errors(rcl_state:t(), [ok | relcool:error()]) ->
                           {ok, rcl_state:t()} | relcool:error().
handle_errors(State, Result) ->
    case [Error || Error <- Result,
                   rcl_util:is_error(Error)] of
        Errors = [_|_] ->
            ?RCL_ERROR({overlay_failed, Errors});
        [] ->
            {ok, State}
    end.

-spec do_individual_overlay(rcl_state:t(), proplists:proplist(),
                            OverlayDirective::term()) ->
                                   {ok, rcl_state:t()} | relcool:error().
do_individual_overlay(State, OverlayVars, {mkdir, Dir}) ->
    ModuleName = make_template_name("rcl_mkdir_template", Dir),
    io:format("compiling to ~p ~n", [ModuleName]),
    case erlydtl:compile(erlang:iolist_to_binary(Dir), ModuleName) of
        {ok, ModuleName} ->
            io:format("compiled ~n"),
            case render(ModuleName, OverlayVars) of
                {ok, IoList} ->
                    io:format("rendered ~n"),
                    Absolute = filename:absname(filename:join(rcl_state:root_dir(State),
                                                              erlang:iolist_to_binary(IoList))),
                    io:format("got ~p ~n", [Absolute]),
                    case rcl_util:mkdir_p(Absolute) of
                        {error, Error} ->
                            ?RCL_ERROR({unable_to_make_dir, Absolute, Error});
                        ok ->
                            ok
                    end;
                {error, Error} ->
                    ?RCL_ERROR({dir_render_failed, Dir, Error})
            end;
        {error, Reason} ->
            ?RCL_ERROR({unable_to_compile_template, Dir, Reason})
    end;
do_individual_overlay(State, OverlayVars, {copy, From, To}) ->
    FromTemplateName = make_template_name("rcl_copy_from_template", From),
    ToTemplateName = make_template_name("rcl_copy_to_template", To),
    file_render_do(State, OverlayVars, From, FromTemplateName,
                   fun(FromFile) ->
                           file_render_do(State, OverlayVars, To, ToTemplateName,
                                          fun(ToFile) ->
                                                  filelib:ensure_dir(ToFile),
                                                  case ec_file:copy(FromFile, ToFile) of
                                                      ok ->
                                                          ok;
                                                      {error, Err} ->
                                                          ?RCL_ERROR({copy_failed,
                                                                      FromFile,
                                                                      ToFile, Err})
                                                  end
                                          end)
                   end);
do_individual_overlay(State, OverlayVars, {template, From, To}) ->
    FromTemplateName = make_template_name("rcl_template_from_template", From),
    ToTemplateName = make_template_name("rcl_template_to_template", To),
    file_render_do(State, OverlayVars, From, FromTemplateName,
                   fun(FromFile) ->
                           file_render_do(State, OverlayVars, To, ToTemplateName,
                                          fun(ToFile) ->
                                                  render_template(OverlayVars,
                                                                  erlang:binary_to_list(FromFile),
                                                                  ToFile)
                                          end)
                   end).

-spec render_template(proplists:proplist(), iolist(), file:name()) ->
                             ok | relcool:error().
render_template(OverlayVars, FromFile, ToFile) ->
    TemplateName = make_template_name("rcl_template_renderer", FromFile),
    case erlydtl:compile(FromFile, TemplateName) of
        Good when Good =:= ok; ok =:= {ok, TemplateName} ->
            case render(TemplateName, OverlayVars) of
                {ok, IoData} ->
                    io:format("Rendering ~p~n", [IoData]),
                    case filelib:ensure_dir(ToFile) of
                        ok ->
                            case file:write_file(ToFile, IoData) of
                                ok ->
                                    ok;
                                {error, Reason} ->
                                    ?RCL_ERROR({unable_to_write, ToFile, Reason})
                            end;
                        {error, Reason} ->
                            ?RCL_ERROR({unable_to_enclosing_dir, ToFile, Reason})
                    end;
                {error, Reason} ->
                    ?RCL_ERROR({unable_to_render_template, FromFile, Reason})
            end;
        {error, Reason} ->
            ?RCL_ERROR({unable_to_compile_template, FromFile, Reason})
    end.

-spec file_render_do(rcl_state:t(), proplists:proplist(), iolist(), module(),
                     fun((term()) -> {ok, rcl_state:t()} | relcool:error())) ->
                            {ok, rcl_state:t()} | relcool:error().
file_render_do(State, OverlayVars, Data, TemplateName, NextAction) ->
    case erlydtl:compile(erlang:iolist_to_binary(Data), TemplateName) of
        {ok, TemplateName} ->
            case render(TemplateName, OverlayVars) of
                {ok, IoList} ->
                    Absolute = filename:absname(filename:join(rcl_state:root_dir(State),
                                                              erlang:iolist_to_binary(IoList))),
                    NextAction(Absolute);
                {error, Error} ->
                    ?RCL_ERROR({render_failed, Data, Error})
            end;
        {error, Reason} ->
            ?RCL_ERROR({unable_to_compile_template, Data, Reason})
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
