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
%%% @doc This module represents useful, relevant information about an
%%% application. The relevant information is.
%%%
%%% <ul>
%%%  <li> Name - The application name as an atom </li>
%%%  <li> Vsn - The application version as a list </li>
%%%  <li> The root directory of the application. The directory that contains the
%%%  ebin/src/priv etc </li>
%%%  <li> Active Deps - The Active or 'application' dependencies of the OTP
%%%  App. That is the things in the 'applications' property of the application
%%%  metadata </li>
%%%  <li> Library Deps - The Inactive or Library dependencies of the ATP
%%%  app. That is the things in the 'included_applications property of the
%%%  application metadata.
%%%  </ul>
%%%
-module(rlx_app_info).

-export([new/0,
         new/5,
         name/1,
         name/2,
         original_vsn/1,
         vsn/1,
         vsn/2,
         vsn_as_string/1,
         dir/1,
         dir/2,
         active_deps/1,
         active_deps/2,
         library_deps/1,
         library_deps/2,
         link/1,
         link/2,
         format_error/1,
         format/2,
         format/1]).

-export_type([t/0]).

-include("relx.hrl").

-record(app_info_t, {name :: atom(),
                     original_vsn :: string(),
                     vsn :: ec_semver:semver(),
                     dir :: binary(),
                     link=false :: boolean(),
                     active_deps=[]:: [atom()],
                     library_deps=[] :: [atom()]}).

%%============================================================================
%% types
%%============================================================================
-opaque t() :: #app_info_t{}.

%%============================================================================
%% API
%% ============================================================================
%% @doc Build a new, empty, app info value. This is not of a lot of use and you
%% probably wont be doing this much.
-spec new() -> {ok, t()}.
new() ->
    {ok, #app_info_t{}}.

%% @doc build a complete version of the app info with all fields set.
-spec new(atom(), string(), file:name(), [atom()], [atom()]) ->
                 {ok, t()} | relx:error().
new(AppName, Vsn, Dir, ActiveDeps, LibraryDeps) ->
    new(AppName, Vsn, Dir, ActiveDeps, LibraryDeps, false).

%% @doc build a complete version of the app info with all fields set.
-spec new(atom(), string(), file:name(), [atom()], [atom()], boolean()) ->
                 {ok, t()} | relx:error().
new(AppName, Vsn, Dir, ActiveDeps, LibraryDeps, Link)
  when erlang:is_atom(AppName),
       erlang:is_list(ActiveDeps),
       erlang:is_list(LibraryDeps) ->
    case parse_version(Vsn) of
        {fail, _} ->
            ?RLX_ERROR({vsn_parse, AppName});
        ParsedVsn ->
            {ok, #app_info_t{name=AppName, original_vsn=Vsn,
                             vsn=ParsedVsn,
                             dir=Dir,
                             active_deps=ActiveDeps,
                             library_deps=LibraryDeps,
                             link=Link}}
    end.

-spec name(t()) -> atom().
name(#app_info_t{name=Name}) ->
    Name.

-spec name(t(), atom()) -> t().
name(AppInfo=#app_info_t{}, AppName)
  when erlang:is_atom(AppName) ->
    AppInfo#app_info_t{name=AppName}.

-spec vsn(t()) -> ec_semver:semver().
vsn(#app_info_t{vsn=Vsn}) ->
    Vsn.

-spec original_vsn(t()) -> string().
original_vsn(#app_info_t{original_vsn=Vsn}) ->
    Vsn.

-spec vsn_as_string(t()) -> string().
vsn_as_string(#app_info_t{vsn=Vsn}) ->
    erlang:binary_to_list(erlang:iolist_to_binary(ec_semver:format(Vsn))).

-spec vsn(t(), string()) -> {ok, t()} | relx:error().
vsn(AppInfo=#app_info_t{name=AppName}, AppVsn)
  when erlang:is_list(AppVsn) ->
    case parse_version(AppVsn) of
        {fail, _} ->
            ?RLX_ERROR({vsn_parse, AppName});
        ParsedVsn ->
            {ok, AppInfo#app_info_t{vsn=ParsedVsn}}
    end.

-spec dir(t()) -> file:name().
dir(#app_info_t{dir=Dir}) ->
    Dir.
-spec dir(t(), file:name()) -> t().
dir(AppInfo=#app_info_t{}, Dir) ->
    AppInfo#app_info_t{dir=Dir}.

-spec active_deps(t()) -> [atom()].
active_deps(#app_info_t{active_deps=Deps}) ->
    Deps.
-spec active_deps(t(), [atom()]) -> t().
active_deps(AppInfo=#app_info_t{}, ActiveDeps)
  when erlang:is_list(ActiveDeps) ->
    AppInfo#app_info_t{active_deps=ActiveDeps}.

-spec library_deps(t()) -> [atom()].
library_deps(#app_info_t{library_deps=Deps}) ->
    Deps.

-spec library_deps(t(), [atom()]) -> t().
library_deps(AppInfo=#app_info_t{}, LibraryDeps)
  when erlang:is_list(LibraryDeps) ->
    AppInfo#app_info_t{library_deps=LibraryDeps}.

-spec link(t()) -> boolean().
link(#app_info_t{link=Link}) ->
    Link.

-spec link(t(), boolean()) -> t().
link(AppInfo, NewLink) ->
    AppInfo#app_info_t{link=NewLink}.

-spec format_error(Reason::term()) -> iolist().
format_error({vsn_parse, AppName}) ->
    io_lib:format("Error parsing version for ~p",
                  [AppName]).

-spec format(t()) -> iolist().
format(AppInfo) ->
    format(0, AppInfo).

-spec format(non_neg_integer(), t()) -> iolist().
format(Indent, #app_info_t{name=Name, vsn=Vsn, dir=Dir,
                           active_deps=Deps, library_deps=LibDeps,
                           link=Link}) ->
    [rlx_util:indent(Indent), erlang:atom_to_list(Name), "-", ec_semver:format(Vsn),
     ": ", Dir, "\n",
     rlx_util:indent(Indent + 1), "Symlink: ", erlang:atom_to_list(Link), "\n",
     rlx_util:indent(Indent + 1), "Active Dependencies:\n",
     [[rlx_util:indent(Indent + 2), erlang:atom_to_list(Dep), ",\n"] || Dep <- Deps],
     rlx_util:indent(Indent + 1), "Library Dependencies:\n",
     [[rlx_util:indent(Indent + 2), erlang:atom_to_list(LibDep), ",\n"] || LibDep <- LibDeps]].

%%%===================================================================
%%% Internal Functions
%%%===================================================================
parse_version(Vsn)
  when erlang:is_list(Vsn) ->
    ec_semver:parse(Vsn);
parse_version(Vsn = {_, {_, _}}) ->
    Vsn.
