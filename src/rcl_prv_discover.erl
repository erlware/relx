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
%%% @doc This provider uses the lib_dir setting of the state. It searches the
%%% Lib Dirs looking for all OTP Applications that are available. When it finds
%%% those OTP Applications it loads the information about them and adds them to
%%% the state of available apps. This implements the rcl_provider behaviour.
-module(rcl_prv_discover).
-behaviour(rcl_provider).

-export([init/1,
         do/1,
         format_error/1]).

%%============================================================================
%% API
%%============================================================================
-spec init(rcl_state:t()) -> {ok, rcl_state:t()}.
init(State) ->
    {ok, State}.

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rcl_state:t()) -> {error, Reason::term()} | {ok, rcl_state:t()}.
do(State) ->
    LibDirs = get_lib_dirs(State),
    rcl_log:info(rcl_state:log(State),
                  fun() ->
                          ["Resolving OTP Applications from directories:\n",
                           [[rcl_util:indent(1), LibDir, "\n"] || LibDir <- LibDirs]]
                  end),

    AppMeta0 = lists:flatten(ec_plists:map(fun discover_dir/1, LibDirs)),
    Errors = [case El of
                  {error, Ret} -> Ret;
                  _ -> El
              end
              || El <- AppMeta0,
                 case El of
                     {error, _} ->
                         true;
                     _ ->
                         false
                 end],

    lists:filter(fun({error, _}) -> true;
                    (_) -> false
                 end, AppMeta0),
    case Errors of
        [] ->
            AppMeta1 = lists:flatten(AppMeta0),
            rcl_log:debug(rcl_state:log(State),
                          fun() ->
                                  ["Resolved the following OTP Applications from the system: \n",
                                   [[rcl_app_info:format(1, App), "\n"] || App <- AppMeta1]]
                          end),
            {ok, rcl_state:available_apps(State, AppMeta1)};
        _ ->
            {error, Errors}
    end.

-spec format_error({error, [ErrorDetail::term()]}) -> iolist().
format_error({error, ErrorDetails}) ->
    [[format_detail(ErrorDetail), "\n"] || ErrorDetail <- ErrorDetails].

%%%===================================================================
%%% Internal Functions
%%%===================================================================
get_lib_dirs(State) ->
    LibDirs0 = rcl_state:lib_dirs(State),
    add_rebar_deps_dir(State, LibDirs0).

-spec add_rebar_deps_dir(rcl_state:t(), [file:name()]) -> [file:name()].
add_rebar_deps_dir(State, LibDirs) ->
    ExcludeRebar = rcl_state:get(State, discover_exclude_rebar, false),
    case ExcludeRebar of
        true ->
            add_system_lib_dir(State, LibDirs);
        false ->
            %% Check to see if there is a rebar.config. If so then look for a deps
            %% dir. If both are there then we add that to the lib dirs.
            {ok, Cwd} = file:get_cwd(),

            RebarConfig = filename:join([Cwd, "rebar.config"]),
            DepsDir = filename:join([Cwd, "deps"]),
            case filelib:is_regular(RebarConfig) andalso filelib:is_dir(DepsDir) of
                true ->
                    add_system_lib_dir(State, [filename:absname(Cwd) | LibDirs]);
                false ->
                    add_system_lib_dir(State, LibDirs)
            end
    end.

-spec add_system_lib_dir(rcl_state:t(), [file:name()]) -> [file:name()].
add_system_lib_dir(State, LibDirs) ->
    ExcludeSystem = rcl_state:get(State, discover_exclude_system, false),

    case ExcludeSystem of
        true ->
            LibDirs;
        false ->
            SystemLibDir0 = code:lib_dir(),
            case filelib:is_dir(SystemLibDir0) of
                true ->
                    [SystemLibDir0 | LibDirs];
                false ->
                    LibDirs
            end
    end.

-spec format_detail(ErrorDetail::term()) -> iolist().
format_detail({accessing, File, eaccess}) ->
    io_lib:format("permission denied accessing file ~s", [File]);
format_detail({accessing, File, Type}) ->
    io_lib:format("error (~p) accessing file ~s", [Type, File]);
format_detail({error, {no_beam_files, EbinDir}}) ->
    io_lib:format("no beam files found in directory ~s", [EbinDir]);
format_detail({error, {not_a_directory, EbinDir}}) ->
    io_lib:format("~s is not a directory when it should be a directory", [EbinDir]);
format_detail({error, {unable_to_load_app, AppDir, _}}) ->
    io_lib:format("Unable to load the application metadata from ~s", [AppDir]);
format_detail({error, {invalid_app_file, File}}) ->
    io_lib:format("Application metadata file exists but is malformed: ~s",
                  [File]);
format_detail({error, {unversioned_app, AppDir, _AppName}}) ->
    io_lib:format("Application metadata exists but version is not available: ~s",
                  [AppDir]);
format_detail({error, {app_info_error, Detail}}) ->
    rcl_app_info:format_error({error, Detail}).


-spec discover_dir(file:name()) ->
                          [rcl_app_info:t() | {error, Reason::term()}] |
                          rcl_app_info:t() | {error, Reason::term()}.
discover_dir(File) ->
    case filelib:is_dir(File) of
        true ->
            case file:list_dir(File) of
                {error, Reason} ->
                    {error, {accessing, File, Reason}};
                {ok, List} ->
                    ec_plists:map(fun discover_dir/1, [filename:join([File, Dir]) || Dir <- List])
            end;
        false ->
            is_valid_otp_app(File)
    end.

-spec is_valid_otp_app(file:name()) -> [rcl_app_info:t() | {error, Reason::term()} | []].
is_valid_otp_app(File) ->
    %% Is this an ebin dir?
    EbinDir = filename:dirname(File),
    case filename:basename(EbinDir) of
        "ebin" ->
            case lists:suffix(".app", File) of
                true ->
                    has_at_least_one_beam(EbinDir, File);
                false ->
                    []
            end;
        _ ->
            []
    end.

-spec has_at_least_one_beam(file:name(), file:filename()) ->
                                   rcl_app_info:t() | {error, Reason::term()}.
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
                                     rcl_app_info:t() | {error, Reason::term()}.
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
                     rcl_app_info:t() | {error, Reason::term()}.
get_vsn(AppDir, AppName, AppDetail) ->
    case proplists:get_value(vsn, AppDetail) of
        undefined ->
            {error, {unversioned_app, AppDir, AppName}};
        AppVsn ->
            case get_deps(AppDir, AppName, AppVsn, AppDetail) of
                {ok, App} ->
                    App;
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
