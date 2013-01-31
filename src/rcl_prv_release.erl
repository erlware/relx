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
-module(rcl_prv_release).

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
    DepGraph = create_dep_graph(State),
    find_default_release(State, DepGraph).

-spec format_error(ErrorDetail::term()) -> iolist().
format_error(no_goals_specified) ->
    "No goals specified for this release ~n";
format_error({no_release_name, Vsn}) ->
    io_lib:format("A target release version was specified (~s) but no name", [Vsn]);
format_error({invalid_release_info, Info}) ->
    io_lib:format("Target release information is in an invalid format ~p", [Info]);
format_error({multiple_release_names, RelA, RelB}) ->
    io_lib:format("No default release name was specified and there are multiple "
                  "releases in the config: ~s, ~s",
                  [RelA, RelB]);
format_error(no_releases_in_system) ->
    "No releases have been specified in the system!";
format_error({no_releases_for, RelName}) ->
    io_lib:format("No releases exist in the system for ~s!", [RelName]);
format_error({release_not_found, {RelName, RelVsn}}) ->
    io_lib:format("No releases exist in the system for ~p:~s!", [RelName, RelVsn]);
format_error({failed_solve, Error}) ->
    io_lib:format("Failed to solve release:\n ~s",
                  [rcl_depsolver:format_error({error, Error})]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec create_dep_graph(rcl_state:t()) -> rcl_depsolver:t().
create_dep_graph(State) ->
    Apps = rcl_state:available_apps(State),
    Graph0 = rcl_depsolver:new_graph(),
    lists:foldl(fun(App, Graph1) ->
                        AppName = rcl_app_info:name(App),
                        AppVsn = rcl_app_info:vsn(App),
                        Deps = rcl_app_info:active_deps(App) ++
                            rcl_app_info:library_deps(App),
                        rcl_depsolver:add_package_version(Graph1,
                                                      AppName,
                                                      AppVsn,
                                                      Deps)
                end, Graph0, Apps).


-spec find_default_release(rcl_state:t(), rcl_depsolver:t()) ->
                                  {ok, rcl_state:t()} | relcool:error().
find_default_release(State, DepGraph) ->
    case rcl_state:default_release(State) of
        {undefined, undefined} ->
            resolve_default_release(State, DepGraph);
        {RelName, undefined} ->
            resolve_default_version(State, DepGraph, RelName);
        {undefined, Vsn} ->
            ?RCL_ERROR({no_release_name, Vsn});
        {RelName, RelVsn} ->
            solve_release(State, DepGraph, RelName, RelVsn)
    end.

resolve_default_release(State0, DepGraph) ->
    %% Here we will just get the highest versioned release and run that.
    case lists:sort(fun release_sort/2,
                    ec_dictionary:to_list(rcl_state:releases(State0))) of
        [{{RelName, RelVsn}, _} | _] ->
            State1 = rcl_state:default_release(State0, RelName, RelVsn),
            solve_release(State1, DepGraph, RelName, RelVsn);
        [] ->
            ?RCL_ERROR(no_releases_in_system)
    end.

resolve_default_version(State0, DepGraph, RelName) ->
    %% Here we will just get the lastest version and run that.
    AllReleases = ec_dictionary:to_list(rcl_state:releases(State0)),
    SpecificReleases = [Rel || Rel={{PossibleRelName, _}, _} <- AllReleases,
                               PossibleRelName =:= RelName],
    case lists:sort(fun release_sort/2, SpecificReleases) of
        [{{RelName, RelVsn}, _} | _] ->
            State1 = rcl_state:default_release(State0, RelName, RelVsn),
            solve_release(State1, DepGraph, RelName, RelVsn);
        [] ->
            ?RCL_ERROR({no_releases_for, RelName})
    end.

-spec release_sort({{rcl_release:name(),rcl_release:vsn()}, term()},
                   {{rcl_release:name(),rcl_release:vsn()}, term()}) ->
                          boolean().
release_sort({{RelName, RelVsnA}, _},
             {{RelName, RelVsnB}, _}) ->
    ec_semver:lte(RelVsnA, RelVsnB);
release_sort({{RelNameA, RelVsnA}, _}, {{RelNameB, RelVsnB}, _}) ->
    %% The release names are different. When the releases are named differently
    %% we can not just take the lastest version. You *must* provide a default
    %% release name at least. So we throw an error here that the top can catch
    %% and return
    erlang:atom_to_list(RelNameA) =< erlang:atom_to_list(RelNameB) andalso
        ec_semver:lte(RelVsnA, RelVsnB).

solve_release(State0, DepGraph, RelName, RelVsn) ->
    rcl_log:debug(rcl_state:log(State0),
                  "Solving Release ~p-~s~n",
                  [RelName, RelVsn]),
    try
        io:format("Solving ~p ~p", [RelName, RelVsn]),
        Release = rcl_state:get_release(State0, RelName, RelVsn),
        Goals = rcl_release:goals(Release),
        case Goals of
            [] ->
                ?RCL_ERROR(no_goals_specified);
            _ ->
                case rcl_depsolver:solve(DepGraph, Goals) of
                    {ok, Pkgs} ->
                        set_resolved(State0, Release, Pkgs);
                    {error, Error} ->
                        ?RCL_ERROR({failed_solve, Error})
                end
        end
    catch
        throw:not_found ->
            ?RCL_ERROR({release_not_found, RelName, RelVsn})
    end.

set_resolved(State, Release0, Pkgs) ->
   case rcl_release:realize(Release0, Pkgs, rcl_state:available_apps(State)) of
       {ok, Release1} ->
           rcl_log:info(rcl_state:log(State),
                        "Resolved ~p-~s~n",
                        [rcl_release:name(Release1),
                         rcl_release:vsn(Release1)]),
           rcl_log:debug(rcl_state:log(State),
                         fun() ->
                                 rcl_release:format(1, Release1)
                         end),
           {ok, rcl_state:update_release(State, Release1)};
       {error, E} ->
           ?RCL_ERROR({release_error, E})
   end.

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
