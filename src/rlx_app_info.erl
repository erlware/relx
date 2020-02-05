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
         vsn/1,
         vsn/2,
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

-include("relx.hrl").

%% @doc Build a new, empty, app info value. This is not of a lot of use and you
%% probably wont be doing this much.
-spec new() -> {ok, rlx_app:t()}.
new() ->
    {ok, rlx_app:new()}.

%% @doc build a complete version of the app info with all fields set.
-spec new(atom(), string(), file:name(), [atom()], [atom()]) -> {ok, rlx_app:t()}.
new(AppName, Vsn, Dir, ActiveDeps, LibraryDeps) ->
    new(AppName, Vsn, Dir, ActiveDeps, LibraryDeps, false).

%% @doc build a complete version of the app info with all fields set.
-spec new(atom(), string(), file:name(), [atom()], [atom()], boolean()) -> {ok, rlx_app:t()}.
new(AppName, Vsn, Dir, ActiveDeps, LibraryDeps, Link)
  when erlang:is_atom(AppName),
       erlang:is_list(ActiveDeps),
       erlang:is_list(LibraryDeps) ->
    {ok, rlx_app:new(AppName, Vsn, Dir, ActiveDeps, LibraryDeps, Link)}.

-spec name(rlx_app:t()) -> atom().
name(#{name := Name}) ->
    Name.

-spec name(rlx_app:t(), atom()) -> rlx_app:t().
name(AppInfo, AppName)
  when erlang:is_atom(AppName) ->
    AppInfo#{name => AppName}.

-spec vsn(rlx_app:t()) -> ec_semver:semver().
vsn(#{vsn := Vsn}) ->
    Vsn.

-spec vsn(rlx_app:t(), string()) -> {ok, rlx_app:t()} | relx:error().
vsn(AppInfo, Vsn)
  when erlang:is_list(Vsn) ->
    {ok, AppInfo#{vsn => Vsn}}.

-spec dir(rlx_app:t()) -> binary().
dir(#{dir := Dir}) ->
    Dir.

-spec dir(rlx_app:t(), binary()) -> rlx_app:t().
dir(AppInfo, Dir) ->
    AppInfo#{dir => Dir}.

-spec active_deps(rlx_app:t()) -> [atom()].
active_deps(#{applications := Deps}) ->
    Deps.

-spec active_deps(rlx_app:t(), [atom()]) -> rlx_app:t().
active_deps(AppInfo, ActiveDeps)
  when erlang:is_list(ActiveDeps) ->
    AppInfo#{applications => ActiveDeps}.

-spec library_deps(rlx_app:t()) -> [atom()].
library_deps(#{included_applications := Deps}) ->
    Deps.

-spec library_deps(rlx_app:t(), [atom()]) -> rlx_app:t().
library_deps(AppInfo, LibraryDeps)
  when erlang:is_list(LibraryDeps) ->
    AppInfo#{included_applications => LibraryDeps}.

-spec link(rlx_app:t()) -> boolean().
link(#{link := Link}) ->
    Link.

-spec link(rlx_app:t(), boolean()) -> rlx_app:t().
link(AppInfo, NewLink) ->
    AppInfo#{link => NewLink}.

-spec format_error(Reason::term()) -> iolist().
format_error(Error) ->
    io_lib:format("~p", [Error]).

-spec format(rlx_app:t()) -> iolist().
format(AppInfo) ->
    format(0, AppInfo).

-spec format(non_neg_integer(), rlx_app:t()) -> iolist().
format(Indent, #{name := Name,
                 vsn := Vsn,
                 dir := Dir,
                 applications := Deps,
                 included_applications := LibDeps,
                 link := Link}) ->
    [rlx_util:indent(Indent), erlang:atom_to_list(Name), "-", Vsn,
     ": ", Dir, "\n",
     rlx_util:indent(Indent + 1), "Symlink: ", erlang:atom_to_list(Link), "\n",
     rlx_util:indent(Indent + 1), "Active Dependencies:\n",
     [[rlx_util:indent(Indent + 2), erlang:atom_to_list(Dep), ",\n"] || Dep <- Deps],
     rlx_util:indent(Indent + 1), "Library Dependencies:\n",
     [[rlx_util:indent(Indent + 2), erlang:atom_to_list(LibDep), ",\n"] || LibDep <- LibDeps]].

