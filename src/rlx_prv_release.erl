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
-module(rlx_prv_release).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("relx.hrl").

-define(PROVIDER, resolve_release).
-define(DEPS, [app_discover]).

%%============================================================================
%% API
%%============================================================================

-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    State1 = rlx_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {deps, ?DEPS}])),
    {ok, State1}.

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State) ->
    find_default_release(State).

-spec format_error(ErrorDetail::term()) -> iolist().
format_error(no_goals_specified) ->
    "No goals specified for this release ~n";
format_error({release_erts_error, Dir}) ->
    io_lib:format("Unable to find erts in ~s~n", [Dir]);
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
                  [rlx_depsolver:format_error({error, Error})]);
format_error({release_error, Error}) ->
    io_lib:format("Failed to resolve release:\n ~p~n", [Error]).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec find_default_release(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
find_default_release(State) ->
    try
        case rlx_state:default_configured_release(State) of
            {undefined, undefined} ->
                resolve_default_release(State);
            {RelName, undefined} ->
                resolve_default_version(State, RelName);
            {undefined, Vsn} ->
                ?RLX_ERROR({no_release_name, Vsn});
            {RelName, RelVsn} ->
                solve_release(State, RelName, RelVsn);
            undefined ->
                ?RLX_ERROR(no_releases_in_system)
        end
    catch
        throw:{multiple_release_names, _, _}=Error ->
            ?RLX_ERROR(Error)
    end.

resolve_default_release(State0) ->
    %% Here we will just get the highest versioned release and run that.
    case lists:sort(fun release_sort/2,
                    maps:to_list(rlx_state:configured_releases(State0))) of
        [{{RelName, RelVsn}, _} | _] ->
            State1 = rlx_state:default_configured_release(State0, RelName, RelVsn),
            solve_release(State1, RelName, RelVsn);
        [] ->
            ?RLX_ERROR(no_releases_in_system)
    end.

resolve_default_version(State0, RelName) ->
    %% Here we will just get the lastest version and run that.
    AllReleases = maps:to_list(rlx_state:configured_releases(State0)),
    SpecificReleases = [Rel || Rel={{PossibleRelName, _}, _} <- AllReleases,
                               PossibleRelName =:= RelName],
    case lists:sort(fun release_sort/2, SpecificReleases) of
        [{{RelName, RelVsn}, _} | _] ->
            State1 = rlx_state:default_configured_release(State0, RelName, RelVsn),
            solve_release(State1, RelName, RelVsn);
        [] ->
            ?RLX_ERROR({no_releases_for, RelName})
    end.

-spec release_sort({{rlx_release:name(),rlx_release:vsn()}, term()},
                   {{rlx_release:name(),rlx_release:vsn()}, term()}) ->
                          boolean().
release_sort({{RelName, RelVsnA}, _},
             {{RelName, RelVsnB}, _}) ->
    ec_semver:lte(RelVsnB, RelVsnA);
release_sort({{RelA, _}, _}, {{RelB, _}, _}) ->
    %% The release names are different. When the releases are named differently
    %% we can not just take the lastest version. You *must* provide a default
    %% release name at least. So we throw an error here that the top can catch
    %% and return
    erlang:throw({multiple_release_names, RelA, RelB}).

solve_release(State0, RelName, RelVsn) ->
    ec_cmd_log:debug(rlx_state:log(State0),
                     "Solving Release ~p-~s~n",
                     [RelName, RelVsn]),
    AllApps = rlx_state:available_apps(State0),
    try
        Release =
            case get_realized_release(State0, RelName, RelVsn) of
                undefined ->
                    rlx_state:get_configured_release(State0, RelName, RelVsn);
                {ok, Release0} ->
                    rlx_release:relfile(rlx_state:get_configured_release(State0, RelName, RelVsn),
                                        rlx_release:relfile(Release0))
            end,

        %% get per release config values and override the State with them
        Config = rlx_release:config(Release),
        {ok, State1} = lists:foldl(fun rlx_config_terms:load/2, {ok, State0}, Config),
        Goals = rlx_release:goals(Release),
        case Goals of
            [] ->
                ?RLX_ERROR(no_goals_specified);
            _ ->
                Pkgs = subset(maps:to_list(Goals), AllApps),
                set_resolved(State1, Release, Pkgs)
        end
    catch
        throw:not_found ->
            ?RLX_ERROR({release_not_found, {RelName, RelVsn}})
    end.

subset(Apps, World) ->
    subset(Apps, World, sets:new(), []).

subset([], _World, _Seen, Acc) ->
    Acc;
subset([Goal | Rest], World, Seen, Acc) ->
    {Name, Vsn} = name_version(Goal),
    case sets:is_element(Name, Seen) of
        true ->
            subset(Rest, World, Seen, Acc);
        _ ->
            AppInfo=#{applications := Applications,
                      included_applications := IncludedApplications} =
                case ec_lists:find(fun(App) ->
                                           case Vsn of
                                               undefined ->
                                                   rlx_app_info:name(App) =:= Name;
                                                _ ->
                                                   rlx_app_info:name(App) =:= Name
                                                       andalso rlx_app_info:vsn(App) =:= Vsn
                                           end
                                   end, World) of
                          {ok, A} ->
                              A;
                          error ->
                              throw({error, {notfound, Name}})
                              %% TODO: Support overriding the dirs to search
                              %% precedence: apps > deps > erl_libs > system
                              %% Dir = code:lib_dir(Name),
                              %% case rebar_app_discover:find_app(Dir, valid) of
                              %%     {true, A} ->
                              %%         A;
                              %%     _ ->
                              %%         throw({app_not_found, Name})
                              %% end
                      end,

            subset(Rest ++ Applications ++ IncludedApplications,
                   World,
                   sets:add_element(Name, Seen),
                   Acc ++ [AppInfo])
    end.


set_resolved(State, Release0, Pkgs) ->
    case rlx_release:realize(Release0, Pkgs) of
        {ok, Release1} ->
            ec_cmd_log:info(rlx_state:log(State),
                            "Resolved ~p-~s~n",
                            [rlx_release:name(Release1),
                             rlx_release:vsn(Release1)]),
            %% ec_cmd_log:debug(rlx_state:log(State),
            %%                  fun() ->
            %%                          rlx_release:format(0, Release1)
            %%                  end),
            case rlx_state:get(State, include_erts, undefined) of
                IncludeErts when is_atom(IncludeErts) ->
                    {ok, rlx_state:add_realized_release(State, Release1)};
                ErtsDir ->
                    try
                        [Erts | _] = filelib:wildcard(filename:join(ErtsDir, "erts-*")),
                        [_, ErtsVsn] = rlx_string:lexemes(filename:basename(Erts), "-"),
                        {ok, rlx_state:add_realized_release(State, rlx_release:erts(Release1, ErtsVsn))}
                    catch
                        _:_ ->
                            ?RLX_ERROR({release_erts_error, ErtsDir})
                    end
            end
    end.

get_realized_release(State, RelName, RelVsn) ->
    try
        Release = rlx_state:get_realized_release(State, RelName, RelVsn),
        {ok, Release}
    catch
        error:{badkey, _} ->
            undefined
    end.

name_version(Name) when is_atom(Name) ->
    {Name, undefined};
name_version({Name, #{vsn := Vsn}}) ->
    {Name, Vsn}.
