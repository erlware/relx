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
-module(rcl_app_info).

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
         format/1]).

-export_type([t/0]).

-record(app_info_t, {name :: atom(),
                     vsn :: string(),
                     dir :: file:name(),
                     active_deps :: [atom()],
                     library_deps :: [atom()]}).

%%============================================================================
%% types
%%============================================================================
-opaque t() :: record(app_info_t).

%%============================================================================
%% API
%% ============================================================================
%% @doc Build a new, empty, app info value. This is not of a lot of use and you
%% probably wont be doing this much.
-spec new() -> t().
new() ->
    #app_info_t{}.

%% @doc build a complete version of the app info with all fields set.
-spec new(atom(), string(), file:name(), [atom()], [atom()]) -> t().
new(AppName, Vsn, Dir, ActiveDeps, LibraryDeps)
  when erlang:is_atom(AppName),
       erlang:is_list(Vsn),
       erlang:is_list(Dir),
       erlang:is_list(ActiveDeps),
       erlang:is_list(LibraryDeps) ->
    #app_info_t{name=AppName, vsn=Vsn, dir=Dir,
                active_deps=ActiveDeps,
                library_deps=LibraryDeps}.

-spec name(t()) -> atom().
name(#app_info_t{name=Name}) ->
    Name.

-spec name(t(), atom()) -> t().
name(AppInfo=#app_info_t{}, AppName)
  when erlang:is_atom(AppName) ->
    AppInfo#app_info_t{name=AppName}.

-spec vsn(t()) -> string().
vsn(#app_info_t{vsn=Vsn}) ->
    Vsn.

-spec vsn(t(), string()) -> t().
vsn(AppInfo=#app_info_t{}, AppVsn)
  when erlang:is_list(AppVsn) ->
    AppInfo#app_info_t{vsn=AppVsn}.

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

-spec format(t()) -> iolist().
format(#app_info_t{name=Name, vsn=Vsn, dir=Dir,
                   active_deps=Deps, library_deps=LibDeps}) ->
    [erlang:atom_to_list(Name), "-", Vsn, ": ", Dir, "\n",
     rcl_util:indent(1), "Active Dependencies:\n",
     [[rcl_util:indent(2), erlang:atom_to_list(Dep), ",\n"] || Dep <- Deps],
     rcl_util:indent(1), "Library Dependencies:\n",
     [[rcl_util:indent(2), erlang:atom_to_list(LibDep), ",\n"] || LibDep <- LibDeps]].



%%%===================================================================
%%% Test Functions
%%%===================================================================
