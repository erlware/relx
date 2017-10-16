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
%%%-------------------------------------------------------------------
%%% @author Joe Armstrong
%%% @author Eric Merritt
%%% @author Konstantin Tcepliaev
%%% @doc
%%%  This is a pretty simple topological sort for erlang. It was
%%%  originally written for ermake by Joe Armstrong back in '98. It
%%%  has been pretty heavily modified by Eric Merritt since '06 and modified again for Relx.
%%%  Konstantin Tcepliaev rewrote the algorithm in 2017.
%%%
%%%  A partial order on the set S is a set of pairs {Xi,Xj} such that
%%%  some relation between Xi and Xj is obeyed.
%%%
%%%  A topological sort of a partial order is a sequence of elements
%%%  [X1, X2, X3 ...] such that if whenever {Xi, Xj} is in the partial
%%%  order i &lt; j
%%%
%%%  This particular implementation guarantees that nodes closer to
%%%  the top level of the graph will be put as close as possible to
%%%  the beginning of the resulting list - this ensures that dependencies
%%%  are started as late as possible, and top-level apps are started
%%%  as early as possible.
%%% @end
%%%-------------------------------------------------------------------
-module(rlx_topo).

-export([sort_apps/1,
         format_error/1]).

-include("relx.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc This only does a topo sort on the list of applications and
%% assumes that there is only *one* version of each app in the list of
%% applications. This implies that you have already done the
%% constraint solve before you pass the list of apps here to be
%% sorted.
-spec sort_apps([rlx_app_info:t()]) ->
                       {ok, [rlx_app_info:t()]} |
                       relx:error().
sort_apps(Apps) ->
    AppDeps = [{rlx_app_info:name(App),
                rlx_app_info:active_deps(App) ++ rlx_app_info:library_deps(App)}
               || App <- Apps],
    {AppNames, _} = lists:unzip(AppDeps),
    case lists:foldl(fun iterator/2, {ok, [], AppDeps, []}, AppNames) of
        {ok, Names, _, _} ->
            {ok, names_to_apps(lists:reverse(Names), Apps)};
        E ->
            E
    end.

%% @doc nicely format the error from the sort.
-spec format_error(Reason::term()) -> iolist().
format_error({cycle, App, Path}) ->
    ["Cycle detected in dependency graph, this must be resolved "
     "before we can continue:\n",
     rlx_util:indent(2),
     [[erlang:atom_to_list(A), " -> "] || A <- lists:reverse(Path)],
     erlang:atom_to_list(App)].

%%====================================================================
%% Internal Functions
%%====================================================================

-type name() :: AppName::atom().
-type app_dep() :: {AppName::name(), [DepName::name()]}.
-type iterator_state() :: {ok, [Acc::name()],
                           [Apps::app_dep()],
                           [Path::name()]}.

-spec iterator(name(), iterator_state() | relx:error()) ->
                      iterator_state() | relx:error().
iterator(App, {ok, Acc, Apps, Path}) ->
    case lists:member(App, Acc) of
        false ->
            %% haven't seen this app yet
            case lists:keytake(App, 1, Apps) of
                {value, {App, Deps}, NewApps} ->
                    DepInit = {ok, Acc, NewApps, [App | Path]},
                    %% recurse over deps
                    case lists:foldl(fun iterator/2, DepInit, Deps) of
                        {ok, DepAcc, DepApps, _} ->
                            {ok, [App | DepAcc], DepApps, Path};
                        Error ->
                            Error
                    end;
                false ->
                    %% we have visited this app before,
                    %% that means there's a cycle
                    ?RLX_ERROR({cycle, App, Path})
            end;
        true ->
            %% this app and its deps were already processed
            {ok, Acc, Apps, Path}
    end;
iterator(_, Error) ->
    Error.

-spec names_to_apps([atom()], [rlx_app_info:t()]) -> [rlx_app_info:t()].
names_to_apps(Names, Apps) ->
 [find_app_by_name(Name, Apps) || Name <- Names].

-spec find_app_by_name(atom(), [rlx_app_info:t()]) -> rlx_app_info:t().
find_app_by_name(Name, Apps) ->
    {ok, App1} =
        ec_lists:find(fun(App) ->
                              rlx_app_info:name(App) =:= Name
                      end, Apps),
    App1.

%%====================================================================
%% Tests
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

topo_apps_cycle_test() ->
    {ok, App1} = rlx_app_info:new(app1, "0.1", "/no-dir", [app2], [stdlib]),
    {ok, App2} = rlx_app_info:new(app2, "0.1", "/no-dir", [app1], []),
    Apps = [App1, App2],
    ?assertMatch({error, {_, {cycle, app1, [app2, app1]}}},
                 sort_apps(Apps)).

topo_apps_good_test() ->
    Apps = [App ||
               {ok, App} <-
                   [rlx_app_info:new(app1, "0.1", "/no-dir", [app2, zapp1], [stdlib, kernel]),
                    rlx_app_info:new(app2, "0.1", "/no-dir", [app3], []),
                    rlx_app_info:new(app3, "0.1", "/no-dir", [kernel], []),
                    rlx_app_info:new(zapp1, "0.1", "/no-dir", [app2,app3,zapp2], []),
                    rlx_app_info:new(stdlib, "0.1", "/no-dir", [], []),
                    rlx_app_info:new(kernel, "0.1", "/no-dir", [], []),
                    rlx_app_info:new(zapp2, "0.1", "/no-dir", [], [])]],
    {ok, Sorted} = sort_apps(Apps),
    ?assertMatch([kernel, app3, app2, zapp2, zapp1, stdlib, app1],
                 [rlx_app_info:name(App) || App <- Sorted]).

topo_apps_1_test() ->
    Apps = [App ||
               {ok, App} <-
                   [rlx_app_info:new(app0, "0.1", "/no-dir", [], [stdlib, dep1, dep2, dep3]),
                    rlx_app_info:new(app1, "0.1", "/no-dir", [], [stdlib, kernel]),
                    rlx_app_info:new(dep1, "0.1", "/no-dir", [], []),
                    rlx_app_info:new(dep2, "0.1", "/no-dir", [], []),
                    rlx_app_info:new(dep3, "0.1", "/no-dir", [], []),
                    rlx_app_info:new(stdlib, "0.1", "/no-dir", [], []),
                    rlx_app_info:new(kernel, "0.1", "/no-dir", [], [])]],
    {ok, Sorted} = sort_apps(Apps),
    ?assertMatch([stdlib, dep1, dep2, dep3, app0, kernel, app1],
                 [rlx_app_info:name(App) || App <- Sorted]).

topo_apps_2_test() ->
    Apps = [App ||
               {ok, App} <-
                   [rlx_app_info:new(app1, "0.1", "/no-dir", [app2, app3, app4, app5,
                                                              stdlib, kernel],
                                                             []),
                    rlx_app_info:new(app2, "0.1", "/no-dir", [stdlib, kernel], []),
                    rlx_app_info:new(app3, "0.1", "/no-dir", [stdlib, kernel], []),
                    rlx_app_info:new(app4, "0.1", "/no-dir", [stdlib, kernel], []),
                    rlx_app_info:new(app5, "0.1", "/no-dir", [stdlib, kernel], []),
                    rlx_app_info:new(stdlib, "0.1", "/no-dir", [], []),
                    rlx_app_info:new(kernel, "0.1", "/no-dir", [], [])
                   ]],
    {ok, Sorted} = sort_apps(Apps),
    ?assertMatch([stdlib, kernel, app2,
                  app3, app4, app5, app1],
                 [rlx_app_info:name(App) || App <- Sorted]).

-endif.
