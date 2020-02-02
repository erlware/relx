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
%%% @doc This module represents a release and its metadata and is used to
%%% manipulate the release metadata.
-module(rlx_release).

-export([new/2,
         new/3,
         relfile/1,
         relfile/2,
         erts/2,
         erts/1,
         goals/2,
         goals/1,
         name/1,
         vsn/1,
         realize/2,
         applications/1,
         application_details/1,
         application_details/2,
         realized/1,
         metadata/1,
         start_clean_metadata/1,
         no_dot_erlang_metadata/1,
         canonical_name/1,
         config/1,
         config/2,
         format/1,
         format/2,
         format_error/1]).

-export_type([t/0,
              name/0,
              vsn/0,
              type/0,
              incl_apps/0,
              application_spec/0,
              goal/0]).

-include("relx.hrl").

-record(release_t, {name :: atom(),
                    vsn :: ec_semver:any_version(),
                    erts :: undefined | ec_semver:any_version(),
                    goals = [] :: [goal()],
                    realized = false :: boolean(),
                    annotations = undefined :: annotations(),
                    applications = [] ::  [rlx_app:t()],
                    relfile :: undefined | string(),
                    app_detail = [] :: [rlx_app_info:t()],
                    config = []}).

%%============================================================================
%% types
%%============================================================================
-type name() :: atom().
-type vsn() :: string().
-type type() :: permanent | transient | temporary | load | none.
-type incl_apps() :: [name()].

-type goal() :: #{name := name(),
                  vsn => vsn() | undefined,
                  type => type(),
                  included_applications => incl_apps()}.

-type annotations() ::  #{name() => {type(), incl_apps() | void}}.

-type application_spec() :: {name(), vsn()} |
                            {name(), vsn(), type() | incl_apps()} |
                            {name(), vsn(), type(), incl_apps()}.

-opaque t() :: #release_t{}.

%%============================================================================
%% API
%%============================================================================
-spec new(atom(), string(), undefined | file:name()) -> t().
new(ReleaseName, ReleaseVsn, Relfile) ->
    #release_t{name=rlx_util:to_atom(ReleaseName),
               vsn=ReleaseVsn,
               relfile = Relfile,
               annotations=#{}}.

-spec new(atom(), string()) -> t().
new(ReleaseName, ReleaseVsn) ->
    new(ReleaseName, ReleaseVsn, undefined).


-spec relfile(t()) -> file:name() | undefined.
relfile(#release_t{relfile=Relfile}) ->
    Relfile.

-spec relfile(t(), file:name()) -> t().
relfile(Release, Relfile) ->
    Release#release_t{relfile=Relfile}.

-spec name(t()) -> atom().
name(#release_t{name=Name}) ->
    Name.

-spec vsn(t()) -> string().
vsn(#release_t{vsn=Vsn}) ->
    Vsn.

-spec erts(t(), vsn()) -> t().
erts(Release, Vsn) ->
    Release#release_t{erts=Vsn}.

-spec erts(t()) -> vsn().
erts(#release_t{erts=Vsn}) ->
    Vsn.

-spec goals(t(), [relx:config_goal()]) -> {ok, t()} | relx:error().
goals(Release, ConfigGoals) ->
    {ok, Release#release_t{goals=parse_goals(ConfigGoals)}}.

-spec goals(t()) -> [goal()].
goals(#release_t{goals=Goals}) ->
    Goals.

-spec realize(t(), [rlx_app:t()]) ->
                     {ok, t()}.
realize(Rel, Pkgs0) ->
    process_specs(realize_erts(Rel), Pkgs0).

%% @doc this gives the application specs for the release. This can only be
%% populated by the 'realize' call in this module.
-spec applications(t()) -> [rlx_app:t()].
applications(#release_t{applications=Apps}) ->
    Apps.

%% @doc this gives the rlx_app_info objects representing the applications in
%% this release. These should only be populated by the 'realize' call in this
%% module or by reading an existing rel file.
-spec application_details(t()) -> [rlx_app:t()].
application_details(#release_t{app_detail=App}) ->
    App.

%% @doc this is only expected to be called by a process building a new release
%% from an existing rel file.
-spec application_details(t(), [rlx_app:t()]) -> t().
application_details(Release, AppDetail) ->
    Release#release_t{app_detail=AppDetail}.

-spec realized(t()) -> boolean().
realized(#release_t{realized=Realized}) ->
    Realized.

-spec metadata(t()) -> term().
metadata(#release_t{name=Name,
                    vsn=Vsn,
                    erts=ErtsVsn,
                    applications=Apps,
                    realized=Realized}) ->
    case Realized of
        true ->
            {ok, {release, {erlang:atom_to_list(Name), Vsn}, {erts, ErtsVsn},
                  Apps}};
        false ->
            ?RLX_ERROR({not_realized, Name, Vsn})
    end.

-spec start_clean_metadata(t()) -> term().
start_clean_metadata(#release_t{name=Name, vsn=Vsn, erts=ErtsVsn, applications=Apps,
                    realized=Realized}) ->
    case Realized of
        true ->
            {value, Kernel, Apps1} = lists:keytake(kernel, 1, Apps),
            {value, StdLib, Apps2} = lists:keytake(stdlib, 1, Apps1),
            {ok, {release, {erlang:atom_to_list(Name), Vsn}, {erts, ErtsVsn},
                  [Kernel, StdLib | none_type_apps(Apps2)]}};
        false ->
            ?RLX_ERROR({not_realized, Name, Vsn})
    end.

none_type_apps([]) ->
    [];
none_type_apps([{Name, Version} | Rest]) ->
    [{Name, Version, none} | none_type_apps(Rest)];
none_type_apps([{Name, Version, _} | Rest]) ->
    [{Name, Version, none} | none_type_apps(Rest)];
none_type_apps([{Name, Version, _, _} | Rest]) ->
    [{Name, Version, none} | none_type_apps(Rest)].

%% The no_dot_erlang.rel.src file is a literal copy of start_clean.rel.src
%% in Erlang/OTP itself.
-spec no_dot_erlang_metadata(t()) -> term().
no_dot_erlang_metadata(T) ->
    start_clean_metadata(T).

%% @doc produce the canonical name (<name>-<vsn>) for this release
-spec canonical_name(t()) -> string().
canonical_name(#release_t{name=Name, vsn=Vsn}) ->
    erlang:binary_to_list(erlang:iolist_to_binary([erlang:atom_to_list(Name), "-",
                                                   Vsn])).


-spec config(t(), list()) -> t().
config(Release, Config) ->
    Release#release_t{config=Config}.

-spec config(t()) -> list().
config(#release_t{config=Config}) ->
    Config.

-spec format(t()) -> iolist().
format(Release) ->
    format(0, Release).

-spec format(non_neg_integer(), t()) -> iolist().
format(Indent, #release_t{name=Name, vsn=Vsn, erts=ErtsVsn, realized=Realized,
                         goals = Goals, applications=Apps}) ->
    BaseIndent = rlx_util:indent(Indent),
    [BaseIndent, "release: ", rlx_util:to_string(Name), "-", Vsn, "\n",
     rlx_util:indent(Indent + 2), " erts-", ErtsVsn,
     ", realized = ",  erlang:atom_to_list(Realized), "\n",
     rlx_util:indent(Indent + 1), "goals: \n",
     [[rlx_util:indent(Indent + 2),  format_goal(Goal), ",\n"] || Goal <- Goals],
     case Realized of
         true ->
             [rlx_util:indent(Indent + 1), "applications: \n",
              [[rlx_util:indent(Indent + 2),  io_lib:format("~p", [App]), ",\n"] ||
                  App <- Apps]];
         false ->
             []
     end].

-spec format_goal(goal()) -> iolist().
format_goal({Constraint, AppType}) ->
    io_lib:format("~p", [{rlx_depsolver:format_constraint(Constraint), AppType}]);
format_goal({Constraint, AppType, AppInc}) ->
    io_lib:format("~p", [{rlx_depsolver:format_constraint(Constraint), AppType, AppInc}]);
format_goal(Constraint) ->
    rlx_depsolver:format_constraint(Constraint).

-spec format_error(Reason::term()) -> iolist().
format_error({failed_to_parse, Con}) ->
    io_lib:format("Failed to parse constraint ~p", [Con]);
format_error({invalid_constraint, _, Con}) ->
    io_lib:format("Invalid constraint specified ~p", [Con]);
format_error({not_realized, Name, Vsn}) ->
    io_lib:format("Unable to produce metadata release: ~p-~s has not been realized",
                  [Name, Vsn]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec realize_erts(t()) -> t().
realize_erts(Rel=#release_t{erts=undefined}) ->
    Rel#release_t{erts=erlang:system_info(version)};
realize_erts(Rel) ->
    Rel.

-spec process_specs(t(), [rlx_app_info:t()]) ->
                           {ok, t()}.
process_specs(Rel=#release_t{goals=Goals}, World) ->
    ActiveApps = lists:flatten([rlx_app_info:active_deps(El) || El <- World] ++ maps:keys(Goals)),
    LibraryApps = lists:flatten([rlx_app_info:library_deps(El) || El <- World]),
    Specs = [create_app_spec(App, Goals, ActiveApps, LibraryApps) || App <- World],
    {ok, Rel#release_t{goals=Goals,
                       applications=Specs,
                       app_detail=World,
                       realized=true}}.

-spec create_app_spec(rlx_app:t(), [goal()], [name()], [name()]) -> application_spec().
create_app_spec(App, Goals, ActiveApps, LibraryApps) ->
    %% If the app only exists as a dependency in a library app then it should
    %% get the 'load' annotation unless the release spec has provided something
    %% else
    AppName = rlx_app_info:name(App),
    Vsn = rlx_app_info:vsn(App),
    TypeAnnot =
        case (lists:member(AppName, LibraryApps) and
              (not lists:member(AppName, ActiveApps))) of
            true ->
                load;
            false ->
                undefined
        end,

    #{type := Type,
      included_applications := IncludedApplications} =
        maps:get(AppName, Goals, #{type => TypeAnnot,
                                   included_applications => undefined}),

    case {Type, IncludedApplications} of
        {undefined, undefined} ->
            {AppName, Vsn};
        {Type, undefined} when Type =:= permanent ;
                               Type =:= transient ;
                               Type =:= temporary ;
                               Type =:= load ;
                               Type =:= none ->
            {AppName, Vsn, Type};
        {undefined, IncludedApplications} ->
            {AppName, Vsn, IncludedApplications};
        {Type, IncludedApplications} ->
            {AppName, Vsn, Type, IncludedApplications};
        _ ->
            error(?RLX_ERROR({bad_app_goal, {AppName, Vsn, Type, IncludedApplications}}))
    end.

parse_goals(ConfigGoals) ->
    lists:foldl(fun(ConfigGoal, Acc) ->
                        Goal=#{name := Name} = parse_goal(ConfigGoal),
                        Acc#{Name => maps:merge(#{vsn=> undefined,
                                                  type => undefined,
                                                  included_applications => undefined}, Goal)}
              end, #{}, ConfigGoals).

-spec parse_goal(relx:config_goal()) -> goal().
parse_goal(AppName) when is_atom(AppName) ->
    #{name => AppName};
parse_goal({AppName, Type}) when Type =:= permanent ;
                                 Type =:= transient ;
                                 Type =:= temporary ;
                                 Type =:= load ;
                                 Type =:= none ->
    #{name => AppName,
      type => Type};
parse_goal({AppName, IncludedApplications=[H|_]}) when is_atom(H) ->
    #{name => AppName,
      included_applications => IncludedApplications};
parse_goal({AppName, []}) when is_atom(AppName) ->
    #{name => AppName,
      included_applications => []};
parse_goal({AppName, Vsn}) when is_list(Vsn) ->
    #{name => AppName,
      vsn => Vsn};
parse_goal({AppName, Vsn, Type})
  when is_list(Vsn) andalso (Type =:= permanent orelse
                             Type =:= transient orelse
                             Type =:= temporary orelse
                             Type =:= load orelse
                             Type =:= none) ->
    #{name => AppName,
      vsn => Vsn,
      type => Type};
parse_goal({AppName, Vsn, IncludedApplications}) when is_list(Vsn) ,
                                                      is_list(IncludedApplications) ->
    #{name => AppName,
      vsn => Vsn,
      included_applications => IncludedApplications};
parse_goal({AppName, Vsn, Type, IncludedApplications})
  when is_list(Vsn) andalso is_list(IncludedApplications) andalso (Type =:= permanent orelse
                                                                   Type =:= transient orelse
                                                                   Type =:= temporary orelse
                                                                   Type =:= load orelse
                                                                   Type =:= none) ->
    #{name => AppName,
      vsn => Vsn,
      type => Type,
      included_applications => IncludedApplications};
parse_goal(Goal) ->
    error(?RLX_ERROR({bad_goal, Goal})).
