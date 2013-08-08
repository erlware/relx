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
%%% the state of available apps. This implements the rlx_provider behaviour.
-module(rlx_rel_discovery).

-export([do/3,
         format_error/1]).

-include_lib("relx/include/relx.hrl").

%%============================================================================
%% API
%%============================================================================

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rlx_state:t(), [filename:name()], [rlx_app_info:t()]) ->
                {ok, [rlx_release:t()]} | relx:error().
do(State, LibDirs, AppMeta) ->
    rlx_log:info(rlx_state:log(State),
                 fun() ->
                         ["Resolving available releases from directories:\n",
                          [[rlx_util:indent(1), LibDir, "\n"] || LibDir <- LibDirs]]
                 end),
    resolve_rel_metadata(State, LibDirs, AppMeta).

-spec format_error([ErrorDetail::term()]) -> iolist().
format_error(ErrorDetails)
  when erlang:is_list(ErrorDetails) ->
    [[format_detail(ErrorDetail), "\n"] || ErrorDetail <- ErrorDetails].

%%%===================================================================
%%% Internal Functions
%%%===================================================================
resolve_rel_metadata(State, LibDirs, AppMeta) ->
    ReleaseMeta0 = lists:flatten(rlx_dscv_util:do(fun(LibDir, FileType) ->
                                                          discover_dir(LibDir,
                                                                       AppMeta,
                                                                       FileType)
                                                  end, LibDirs)),

        Errors = [case El of
                      {error, Ret} -> Ret;
                      _ -> El
                  end
                  || El <- ReleaseMeta0,
                     case El of
                         {error, _} ->
                             true;
                         _ ->
                             false
                     end],

    case Errors of
        [] ->
            ReleaseMeta1 = [RelMeta || {ok, RelMeta} <- ReleaseMeta0],
            rlx_log:debug(rlx_state:log(State),
                          fun() ->
                                  ["Resolved the following OTP Releases from the system: \n",
                                   [[rlx_release:format(1, Rel), "\n"] || Rel <- ReleaseMeta1]]
                          end),
            {ok, ReleaseMeta1};
        _ ->
            ?RLX_ERROR(Errors)
    end.

-spec format_detail(ErrorDetail::term()) -> iolist().
format_detail({accessing, File, eaccess}) ->
    io_lib:format("permission denied accessing file ~s", [File]);
format_detail({accessing, File, Type}) ->
    io_lib:format("error (~p) accessing file ~s", [Type, File]);
format_detail({Module,Reason}) ->
    io_lib:format("~s~n", [Module:format_error(Reason)]).


-spec discover_dir(file:name(), [rlx_app_info:t()], directory | file) ->
                          {ok, rlx_release:t()}
                              | {error, Reason::term()}
                              | {noresult, false}.
discover_dir(_File, _AppMeta, directory) ->
    {noresult, true};
discover_dir(File, AppMeta, file) ->
    is_valid_release(File, AppMeta).

-spec is_valid_release(file:name(),
                       [rlx_app_info:t()]) ->
                              {ok, rlx_release:t()}
                                  | {error, Reason::term()}
                                  | {noresult, false}.
is_valid_release(File, AppMeta) ->
    case filename:extension(File) of
        <<".rel">>->
            resolve_release(File, AppMeta);
        _ ->
           {noresult, false}
    end.

resolve_release(RelFile, AppMeta) ->
    case file:consult(RelFile) of
        {ok, [{release, {RelName, RelVsn},
               {erts, ErtsVsn},
               Apps}]} ->
            build_release(RelFile, RelName, RelVsn, ErtsVsn, Apps, AppMeta);
        {ok, InvalidRelease} ->
            ?RLX_ERROR({invalid_release_information, InvalidRelease});
        {error, Reason} ->
            ?RLX_ERROR({unable_to_read, RelFile, Reason})
    end.

build_release(RelFile, RelName, RelVsn, ErtsVsn, Apps, AppMeta) ->
    Release = rlx_release:erts(rlx_release:new(RelName, RelVsn, RelFile),
                               ErtsVsn),
    resolve_apps(Apps, AppMeta, Release, []).

resolve_apps([], _AppMeta, Release, Acc) ->
    {ok, rlx_release:application_details(Release, Acc)};
resolve_apps([AppInfo | Apps], AppMeta, Release, Acc) ->
    AppName = erlang:element(1, AppInfo),
    AppVsn = ec_semver:parse(erlang:element(2, AppInfo)),
    case find_app(AppName, AppVsn, AppMeta) of
        Error = {error, _} ->
            Error;
        ResolvedApp ->
            resolve_apps(Apps, AppMeta, Release, [ResolvedApp | Acc])
    end.

find_app(AppName, AppVsn, AppMeta) ->
    case ec_lists:find(fun(App) ->
                               NAppName = rlx_app_info:name(App),
                               NAppVsn = rlx_app_info:vsn(App),
                               AppName == NAppName andalso
                                   AppVsn == NAppVsn
                       end, AppMeta) of
        {ok, Head} ->
            Head;
        error ->
            ?RLX_ERROR({could_not_find, {AppName, AppVsn}})
    end.
