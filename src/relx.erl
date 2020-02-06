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
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012 Erlware, LLC.
%%% @doc
-module(relx).

-export([release/3,
         build_release/2,
         build_release/3,
         build_tar/3,
         build_relup/4,
         format_error/1]).

-include("relx.hrl").

-type error() :: {error, {Module::module(), Reason::term()}}.
-type goal() :: rlx_release:name() |
                {rlx_release:name(), rlx_release:vsn() | rlx_release:type()} |
                {rlx_release:name(), rlx_release:vsn(), rlx_release:type() | rlx_release:incl_apps()} |
                {rlx_release:name(), rlx_release:vsn(), rlx_release:type(), rlx_release:incl_apps()}.

-export_type([goal/0,
              error/0]).

-type release() :: #{name := atom(),
                     vsn := string(),

                     %% top level application list to include in release
                     %% referred to as goals because it is not the complete
                     %% list of applications.
                     goals := [goal()],

                     relfile_path := file:filename_all() | undefined}.

-spec release(rlx_release:name(), rlx_release:vsn(), [goal()]) -> release().
release(Name, Vsn, Goals) ->
    #{name => Name,
      vsn => Vsn,
      goals => Goals,
      relfile_path => undefined}.

-spec build_release(Apps, Config) -> ok | {error, term()} when
      Apps :: [rlx_app:t()],
      Config :: rlx_config:t().
build_release(Apps, Config) ->
    State = config_to_state(Config),
    State1 = resolve_default_release(State),
    {RelName, RelVsn} = rlx_state:default_configured_release(State1),
    Release = #{name => RelName,
                vsn => RelVsn},
    build_release_(Release, Apps, State1).

-spec build_release(Release, Apps, Config) -> ok | {error, term()} when
      Release :: atom() | {atom(), string()} | release(),
      Apps :: [rlx_app:t()],
      Config :: rlx_config:t().
build_release(RelName, Apps, Config) when is_atom(RelName) ->
    State = config_to_state(Config),
    State1 = resolve_default_version(RelName, State),
    {RelName, RelVsn} = rlx_state:default_configured_release(State1),
    Release = #{name => RelName,
                vsn => RelVsn},
    build_release_(Release, Apps, State1);
build_release({RelName, RelVsn}, Apps, Config) when is_atom(RelName) ,
                                                    is_list(RelVsn) ->
    State = config_to_state(Config),
    State1 = rlx_state:default_configured_release(State, RelName, RelVsn),
    Release = #{name => RelName,
                vsn => RelVsn},
    build_release_(Release, Apps, State1);
build_release(Release=#{name := RelName,
                        vsn := RelVsn}, Apps, Config) ->
    State = config_to_state(Config),
    State1 = rlx_state:default_configured_release(State, RelName, RelVsn),
    build_release_(Release, Apps, State1);
build_release(Release, _, _) ->
    ?RLX_ERROR({unrecognized_release, Release}).

-spec build_tar(Release, Apps, Config) -> ok | {error, term()} when
      Release :: atom() | {atom(), string()} | release(),
      Apps :: [rlx_app:t()],
      Config :: rlx_config:t().
build_tar(RelName, Apps, Config) when is_atom(RelName) ->
    State = config_to_state(Config),
    State1 = resolve_default_version(RelName, State),
    {RelName, RelVsn} = rlx_state:default_configured_release(State1),
    Release = #{name => RelName,
                vsn => RelVsn},
    {ok, State2} = build_release_(Release, Apps, State1),
    build_tar_(Release, Apps, State2).

-spec build_relup(rlx_release:name(), rlx_release:vsn(), rlx_release:vsn(), rlx_config:t()) -> ok | {error, term()}.
build_relup(RelName, ToVsn, UpFromVsn, Config) ->
    State = config_to_state(Config),
    rlx_relup:do(RelName, ToVsn, UpFromVsn, State).

-spec format_error(Reason::term()) -> string().
format_error({unrecognized_release, Release}) ->
    io_lib:format("Could not understand release argument ~p~n", [Release]);
format_error({error, {relx, Reason}}) ->
    format_error(Reason);
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
format_error({error, {Module, Reason}}) ->
    io_lib:format("~s~n", [Module:format_error(Reason)]).

%%

config_to_state(Config) ->
    State = rlx_state:new(),
    {ok, State1} = rlx_config_terms:to_state(Config, State),
    State1.

build_tar_(#{name := RelName,
             vsn := RelVsn}, _Apps, State) ->
    OutputDir = rlx_state:output_dir(State),
    Release = rlx_state:get_realized_release(State, RelName, RelVsn),
    rlx_tar:make_tar(State, Release, OutputDir).

build_release_(_Release=#{name := RelName,
                          vsn := RelVsn}, Apps, State) ->
    {ok, RealizedRelease, State1} = rlx_resolve:solve_release(RelName,
                                                              RelVsn,
                                                              rlx_state:available_apps(State, Apps)),
    {ok, State2} = rlx_assemble:do(RealizedRelease, State1),
    rlx_overlay:render(RealizedRelease, State2).

resolve_default_release(State) ->
    %% Here we will just get the highest versioned release and run that.
    case lists:sort(fun release_sort/2, maps:to_list(rlx_state:configured_releases(State))) of
        [{{RelName, RelVsn}, _} | _] ->
            rlx_state:default_configured_release(State, RelName, RelVsn);
        [] ->
            error(?RLX_ERROR(no_releases_in_system))
    end.

resolve_default_version(RelName, State) ->
    %% Here we will just get the lastest version for name RelName and run that.
    AllReleases = maps:to_list(rlx_state:configured_releases(State)),
    SpecificReleases = [Rel || Rel={{PossibleRelName, _}, _} <- AllReleases, PossibleRelName =:= RelName],
    case lists:sort(fun release_sort/2, SpecificReleases) of
        [{{RelName, RelVsn}, _} | _] ->
            rlx_state:default_configured_release(State, RelName, RelVsn);
        [] ->
            error(?RLX_ERROR({no_releases_for, RelName}))
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
    error(?RLX_ERROR({multiple_release_names, RelA, RelB})).
