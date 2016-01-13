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
%%% @doc This provider uses the lib_dir setting of the state. It searches the
%%% Lib Dirs looking for all OTP Applications that are available. When it finds
%%% those OTP Applications it loads the information about them and adds them to
%%% the state of available apps. This implements the provider behaviour.
-module(rlx_app_discovery).

-export([do/2,
         format_error/1]).

-include("relx.hrl").

%%============================================================================
%% API
%%============================================================================

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rlx_state:t(), [file:name()]) -> {ok, [rlx_app_info:t()]} | relx:error().
do(State, LibDirs) ->
    ec_cmd_log:info(rlx_state:log(State),
                    fun() ->
                            ["Resolving OTP Applications from directories:\n",
                             string:join([[rlx_util:indent(2), LibDir] || LibDir <- LibDirs], "\n")]
                    end),
    resolve_app_metadata(State, LibDirs).

-spec format_error([ErrorDetail::term()]) -> iolist().
format_error(ErrorDetails)
  when erlang:is_list(ErrorDetails) ->
    [[format_detail(ErrorDetail), "\n"] || ErrorDetail <- ErrorDetails].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec app_files(list(binary())) -> list(binary()).
app_files(LibDirs) ->
    lists:foldl(fun(LibDir, Acc) ->
                        Files = app_files_paths(LibDir),
                        BinFiles = lists:map(fun(F) ->
                                                     list_to_binary(F)
                                             end, Files),
                        Acc ++ BinFiles
                end, [], LibDirs).

-spec app_files_paths(binary()) -> list(string()).
app_files_paths(LibDir) ->
    %% Search for Erlang apps in the lib dir itself
    Path1 = filename:join([binary_to_list(LibDir),
                           "*.app"]),
    %% Search for Erlang apps in subdirs of lib dir
    Path2 = filename:join([binary_to_list(LibDir),
                           "*",
                           "ebin",
                           "*.app"]),
    lists:foldl(fun(Path, Acc) ->
                        Files = filelib:wildcard(Path),
                        Files ++ Acc
                end, [], [Path1, Path2]).

-spec get_app_metadata(rlx_state:t(), list(binary())) -> list({ok, rlx_app_info:t()}).
get_app_metadata(State, LibDirs) ->
    lists:foldl(fun(AppFile, Acc) ->
                        case is_valid_otp_app(AppFile) of
                            {ok, _} = AppMeta ->
                                [AppMeta|Acc];
                            {warning, W} ->
                                ec_cmd_log:debug(rlx_state:log(State), format_detail(W)),
                                Acc;
                            {error, E} ->
                                ec_cmd_log:error(rlx_state:log(State), format_detail(E)),
                                Acc;
                            _ ->
                                Acc
                        end
                end, [], app_files(LibDirs)).

resolve_app_metadata(State, LibDirs) ->
    AppMeta0 = case rlx_state:get(State, enable_shallow_app_discovery, false) of
                   true ->
                       get_app_metadata(State, LibDirs);
                   false ->
                       lists:flatten(rlx_dscv_util:do(fun discover_dir/2, LibDirs))
               end,
    case [case Err of
              {error, Ret} ->
                  Ret
          end
          || Err <- AppMeta0,
             case Err of
                 {error, _} ->
                     true;
                 {warning, W} ->
                     ec_cmd_log:debug(rlx_state:log(State), format_detail(W)),
                     false;
                 _ ->
                     false
             end] of
        [] ->
            SkipApps = rlx_state:skip_apps(State),
            ExcludeApps = rlx_state:exclude_apps(State),
            AppMeta1 = [rm_exclude_apps(App, ExcludeApps) ||
                           {ok, App} <- setup_overrides(State, AppMeta0),
                               not lists:keymember(rlx_app_info:name(App), 1, SkipApps++ExcludeApps)],
            ec_cmd_log:debug(rlx_state:log(State),
                             fun() ->
                                     ["Resolved the following OTP Applications from the system: \n",
                                      [[rlx_app_info:format(2, App), "\n"] || App <- AppMeta1]]
                             end),
            {ok, AppMeta1};
        Errors ->
            ?RLX_ERROR(Errors)
    end.
%% Apps listed in {exclude_apps, [...]} must be removed from applications lists
rm_exclude_apps(App, ExcludeApps) ->
    ActiveApps = lists:subtract(rlx_app_info:active_deps(App), ExcludeApps),
    LibraryApps = lists:subtract(rlx_app_info:library_deps(App), ExcludeApps),
    rlx_app_info:library_deps(rlx_app_info:active_deps(App, ActiveApps), LibraryApps).

app_name({warning, _}) ->
    undefined;
app_name({error, _}) ->
    undefined;
app_name({ok, AppMeta}) ->
    rlx_app_info:name(AppMeta).

setup_overrides(State, AppMetas0) ->
    Overrides = rlx_state:overrides(State),
    AppMetas1 = [AppMeta || AppMeta <- AppMetas0,
                            not lists:keymember(app_name(AppMeta), 1, Overrides)],
    [resolve_override(AppName, FileName) || {AppName, FileName} <- Overrides] ++ AppMetas1.

resolve_override(AppName, FileName0) ->
    FileName1 = filename:absname(FileName0),
    case is_valid_otp_app(filename:join([FileName1, <<"ebin">>,
                                         erlang:atom_to_list(AppName) ++ ".app"])) of
        {noresult, false} ->
            {error, {invalid_override, AppName, FileName1}};
        Error = {error, _} ->
            Error;
        {ok, App} ->
            {ok, rlx_app_info:link(App, true)}
    end.

-spec format_detail(ErrorDetail::term()) -> iolist().
format_detail({missing_beam_file, Module, BeamFile}) ->
    io_lib:format("Missing beam file ~p ~s", [Module, BeamFile]);
format_detail({error, {invalid_override, AppName, FileName}}) ->
    io_lib:format("Override {~p, ~p} is not a valid OTP App. Perhaps you forgot to build it?",
                  [AppName, FileName]);
format_detail({accessing, File, eaccess}) ->
    io_lib:format("permission denied accessing file ~s", [File]);
format_detail({accessing, File, Type}) ->
    io_lib:format("error (~p) accessing file ~s", [Type, File]);
format_detail({no_beam_files, EbinDir}) ->
    io_lib:format("no beam files found in directory ~s", [EbinDir]);
format_detail({not_a_directory, EbinDir}) ->
    io_lib:format("~s is not a directory when it should be a directory", [EbinDir]);
format_detail({unable_to_load_app, AppDir, _}) ->
    io_lib:format("Unable to load the application metadata from ~s", [AppDir]);
format_detail({invalid_app_file, File}) ->
    io_lib:format("Application metadata file exists but is malformed: ~s",
                  [File]);
format_detail({unversioned_app, AppDir, _AppName}) ->
    io_lib:format("Application metadata exists but version is not available: ~s",
                  [AppDir]);
format_detail({app_info_error, {Module, Detail}}) ->
    Module:format_error(Detail).

-spec discover_dir([file:name()], directory | file) ->
                          {ok, rlx_app_info:t()} | {error, Reason::term()}.
discover_dir(_File, directory) ->
    {noresult, true};
discover_dir(File, file) ->
    is_valid_otp_app(File).

-spec is_valid_otp_app(file:name()) -> {ok, rlx_app_info:t()} |
                                       {warning, Reason::term()} |
                                       {error, Reason::term()} |
                                       {noresult, false}.
is_valid_otp_app(File) ->
    %% Is this an ebin dir?
    EbinDir = filename:dirname(File),
    case filename:basename(EbinDir) of
        <<"ebin">> ->
            case filename:extension(File) of
                <<".app">> ->
                    gather_application_info(EbinDir, File);
                _ ->
                    {noresult, false}
            end;
        _ ->
            {noresult, false}
    end.


-spec gather_application_info(file:name(), file:filename()) ->
                                     {ok, rlx_app_info:t()} |
                                     {warning, Reason::term()} |
                                     {error, Reason::term()}.
gather_application_info(EbinDir, File) ->
    AppDir = filename:dirname(EbinDir),
    case file:consult(File) of
        {ok, [{application, AppName, AppDetail}]} ->
            validate_application_info(EbinDir, File, AppName, AppDetail);
        {error, Reason} ->
            {warning, {unable_to_load_app, AppDir, Reason}};
        _ ->
            {warning, {invalid_app_file, File}}
    end.

-spec validate_application_info(file:name(),
                                file:name(),
                                atom(),
                                proplists:proplist()) ->
                                       {ok, list()} |
                                       {warning, Reason::term()} |
                                       {error, Reason::term()}.
validate_application_info(EbinDir, AppFile, AppName, AppDetail) ->
    AppDir = filename:dirname(EbinDir),
    case get_modules_list(AppFile, AppDetail) of
        {ok, List} ->
            case has_all_beams(EbinDir, List) of
                ok ->
                    get_vsn(AppDir, AppName, AppDetail);
                Error1 ->
                    Error1
            end;
        Error -> Error
    end.

-spec get_modules_list(file:name(), proplists:proplist()) ->
                              {ok, list()} |
                              {warning, Reason::term()} |
                              {error, Reason::term()}.
get_modules_list(AppFile, AppDetail) ->
    case proplists:get_value(modules, AppDetail) of
        undefined ->
            {warning, {invalid_app_file, AppFile}};
        ModulesList ->
            {ok, ModulesList}
    end.

-spec has_all_beams(file:name(), list()) ->
                           ok | {error, Reason::term()}.
has_all_beams(EbinDir, [Module | ModuleList]) ->
    BeamFile = filename:join([EbinDir,
                              list_to_binary(atom_to_list(Module) ++ ".beam")]),
    case ec_file:exists(BeamFile) of
        true ->
            has_all_beams(EbinDir, ModuleList);
        false ->
            {warning, {missing_beam_file, Module, BeamFile}}
    end;
has_all_beams(_, []) ->
    ok.

-spec get_vsn(file:name(), atom(), proplists:proplist()) ->
                     {ok, rlx_app_info:t()} | {error, Reason::term()}.
get_vsn(AppDir, AppName, AppDetail) ->
    case proplists:get_value(vsn, AppDetail) of
        undefined ->
            {error, {unversioned_app, AppDir, AppName}};
        AppVsn ->
            case get_deps(AppDir, AppName, AppVsn, AppDetail) of
                {ok, App} ->
                    {ok, App};
                {error, Detail} ->
                    {error, {app_info_error, Detail}}
            end
    end.

-spec get_deps(file:name(), atom(), string(), proplists:proplist()) ->
                      {ok, rlx_app_info:t()} | {error, Reason::term()}.
get_deps(AppDir, AppName, AppVsn, AppDetail) ->
    ActiveApps = proplists:get_value(applications, AppDetail, []),
    LibraryApps = proplists:get_value(included_applications, AppDetail, []),
    rlx_app_info:new(AppName, AppVsn, AppDir, ActiveApps, LibraryApps).

%%%===================================================================
%%% Test Functions
%%%===================================================================
