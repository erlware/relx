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
%%% the state of available apps. This implements the rcl_provider behaviour.
-module(rcl_app_discovery).

-export([do/2,
         format_error/1]).

-include_lib("relcool/include/relcool.hrl").

%%============================================================================
%% API
%%============================================================================

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rcl_state:t(), [filename:name()]) -> {ok, [rcl_app_info:t()]} | relcool:error().
do(State, LibDirs) ->
    rcl_log:info(rcl_state:log(State),
                 fun() ->
                          ["Resolving OTP Applications from directories:\n",
                           [[rcl_util:indent(1), LibDir, "\n"] || LibDir <- LibDirs]]
                 end),
    resolve_app_metadata(State, LibDirs).

-spec format_error([ErrorDetail::term()]) -> iolist().
format_error(ErrorDetails)
  when erlang:is_list(ErrorDetails) ->
    [[format_detail(ErrorDetail), "\n"] || ErrorDetail <- ErrorDetails].

%%%===================================================================
%%% Internal Functions
%%%===================================================================
resolve_app_metadata(State, LibDirs) ->
    AppMeta0 = lists:flatten(rcl_dscv_util:do(fun discover_dir/2, LibDirs)),
    case [case Err of
              {error, Ret} ->
                  Ret
          end
          || Err <- AppMeta0,
             case Err of
                 {error, _} ->
                         true;
                     _ ->
                         false
                 end] of
        [] ->
            AppMeta1 = [App || {ok, App} <- setup_overrides(State, AppMeta0)],
            rcl_log:debug(rcl_state:log(State),
                          fun() ->
                                  ["Resolved the following OTP Applications from the system: \n",
                                   [[rcl_app_info:format(1, App), "\n"] || App <- AppMeta1]]
                          end),
            {ok, AppMeta1};
        Errors ->
            ?RCL_ERROR(Errors)
    end.

app_name({error, _}) ->
    undefined;
app_name({ok, AppMeta}) ->
    rcl_app_info:name(AppMeta).

setup_overrides(State, AppMetas0) ->
    Overrides = rcl_state:overrides(State),
    AppMetas1 = [AppMeta || AppMeta <- AppMetas0,
                            not lists:keymember(app_name(AppMeta), 1, Overrides)],
    [case is_valid_otp_app(filename:join([FileName, "ebin",
                                         erlang:atom_to_list(AppName) ++ ".app"])) of
         {noresult, false} ->
             {error, {invalid_override, AppName, FileName}};
         Error = {error, _} ->
             Error;
         {ok, App} ->
             {ok, rcl_app_info:link(App, true)}
     end || {AppName, FileName} <- Overrides] ++ AppMetas1.


-spec format_detail(ErrorDetail::term()) -> iolist().
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
                          {ok, rcl_app_info:t()} | {error, Reason::term()}.
discover_dir(_File, directory) ->
    {noresult, true};
discover_dir(File, file) ->
    is_valid_otp_app(File).

-spec is_valid_otp_app(file:name()) -> {ok, rcl_app_info:t()} | {error, Reason::term()} |
                                       {noresult, false}.

is_valid_otp_app(File) ->
    %% Is this an ebin dir?
    EbinDir = filename:dirname(File),
    case filename:basename(EbinDir) of
        "ebin" ->
            case lists:suffix(".app", File) of
                true ->
                    has_at_least_one_beam(EbinDir, File);
                false ->
                    {noresult, false}
            end;
        _ ->
            {noresult, false}
    end.

-spec has_at_least_one_beam(file:name(), file:filename()) ->
                                   {ok, rcl_app_info:t()} | {error, Reason::term()}.
has_at_least_one_beam(EbinDir, File) ->
    case file:list_dir(EbinDir) of
        {ok, List} ->
            case lists:any(fun(NFile) -> lists:suffix(".beam", NFile) end, List) of
                true ->
                    gather_application_info(EbinDir, File);
                false ->
                    {error, {no_beam_files, EbinDir}}
            end;
        _ ->
            {error, {not_a_directory, EbinDir}}
    end.

-spec gather_application_info(file:name(), file:filename()) ->
                                     {ok, rcl_app_info:t()} | {error, Reason::term()}.
gather_application_info(EbinDir, File) ->
    AppDir = filename:dirname(EbinDir),
    case file:consult(File) of
        {ok, [{application, AppName, AppDetail}]} ->
            get_vsn(AppDir, AppName, AppDetail);
        {error, Reason} ->
            {error, {unable_to_load_app, AppDir, Reason}};
        _ ->
            {error, {invalid_app_file, File}}
    end.

-spec get_vsn(file:name(), atom(), proplists:proplist()) ->
                     {ok, rcl_app_info:t()} | {error, Reason::term()}.
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
                      {ok, rcl_app_info:t()} | {error, Reason::term()}.
get_deps(AppDir, AppName, AppVsn, AppDetail) ->
    ActiveApps = proplists:get_value(applications, AppDetail, []),
    LibraryApps = proplists:get_value(included_applications, AppDetail, []),
    rcl_app_info:new(AppName, AppVsn, AppDir, ActiveApps, LibraryApps).

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
