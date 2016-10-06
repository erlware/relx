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
         realize/3,
         applications/1,
         application_details/1,
         application_details/2,
         realized/1,
         metadata/1,
         start_clean_metadata/1,
         canonical_name/1,
         config/1,
         config/2,
         format/1,
         format/2,
         format_error/1]).

-export_type([t/0,
              name/0,
              vsn/0,
              app_name/0,
              app_vsn/0,
              app_type/0,
              application_spec/0,
              application_goal/0]).

-include("relx.hrl").

-record(release_t, {name :: atom(),
                    vsn :: ec_semver:any_version(),
                    erts :: undefined | ec_semver:any_version(),
                    goals = [] :: [rlx_depsolver:constraint()],
                    realized = false :: boolean(),
                    annotations = undefined :: annotations(),
                    applications = [] ::  [application_spec()],
                    relfile :: undefined | string(),
                    app_detail = [] :: [rlx_app_info:t()],
                    config = []}).

%%============================================================================
%% types
%%============================================================================
-type name() :: atom().
-type vsn() :: string().
-type app_name() :: atom().
-type app_vsn() :: string().
-type app_type() :: permanent | transient | temporary | load | none.
-type incl_apps() :: [app_name()].

-type application_spec() :: {app_name(),  app_vsn()} |
                            {app_name(), app_vsn(), app_type() | incl_apps()} |
                            {app_name(), app_vsn(), app_type(), incl_apps()}.

-type application_constraint() :: rlx_depsolver:raw_constraint() | string() | binary().
-type application_goal() :: application_constraint()
                          | {application_constraint(), app_type() | incl_apps()}
                          | {application_constraint(), app_type(), incl_apps() | void}.

-type annotations() ::  ec_dictionary:dictionary(app_name(),
                                                 {app_type(), incl_apps() | void}).


-opaque t() :: #release_t{}.

%%============================================================================
%% API
%%============================================================================
-spec new(atom(), string(), undefined | file:name()) -> t().
new(ReleaseName, ReleaseVsn, Relfile) ->
    #release_t{name=to_atom(ReleaseName), vsn=ReleaseVsn,
               relfile = Relfile,
               annotations=ec_dictionary:new(ec_dict)}.

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

-spec erts(t(), app_vsn()) -> t().
erts(Release, Vsn) ->
    Release#release_t{erts=Vsn}.

-spec erts(t()) -> app_vsn().
erts(#release_t{erts=Vsn}) ->
    Vsn.

-spec goals(t(), [application_goal()]) -> {ok, t()} | relx:error().
goals(Release, Goals0) ->
    lists:foldl(fun parse_goal0/2,
                {ok, Release}, Goals0).

-spec goals(t()) -> [application_goal()].
goals(#release_t{goals=Goals}) ->
    Goals.

-spec realize(t(), [{app_name(), app_vsn()}], [rlx_app_info:t()]) ->
                     {ok, t()}.
realize(Rel, Pkgs0, World0) ->
    World1 = subset_world(Pkgs0, World0),
    process_specs(realize_erts(Rel), World1).

%% @doc this gives the application specs for the release. This can only be
%% populated by the 'realize' call in this module.
-spec applications(t()) -> [application_spec()].
applications(#release_t{applications=Apps}) ->
    Apps.

%% @doc this gives the rlx_app_info objects representing the applications in
%% this release. These should only be populated by the 'realize' call in this
%% module or by reading an existing rel file.
-spec application_details(t()) -> [rlx_app_info:t()].
application_details(#release_t{app_detail=App}) ->
    App.

%% @doc this is only expected to be called by a process building a new release
%% from an existing rel file.
-spec application_details(t(), [rlx_app_info:t()]) -> t().
application_details(Release, AppDetail) ->
    Release#release_t{app_detail=AppDetail}.

-spec realized(t()) -> boolean().
realized(#release_t{realized=Realized}) ->
    Realized.

-spec metadata(t()) -> term().
metadata(#release_t{name=Name, vsn=Vsn, erts=ErtsVsn, applications=Apps,
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
            Kernel = lists:keyfind(kernel, 1, Apps),
            StdLib = lists:keyfind(stdlib, 1, Apps),
            {ok, {release, {erlang:atom_to_list(Name), Vsn}, {erts, ErtsVsn},
                  [Kernel, StdLib]}};
        false ->
            ?RLX_ERROR({not_realized, Name, Vsn})
    end.

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

-spec format_goal(application_goal()) -> iolist().
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
process_specs(Rel=#release_t{annotations=Annots,
                             goals=Goals}, World) ->
    ActiveApps = lists:flatten([rlx_app_info:active_deps(El) || El <- World] ++
                                  [case get_app_name(Goal) of
                                       {error, _} -> [];
                                       G -> G
                                   end || Goal <- Goals]),
    LibraryApps = lists:flatten([rlx_app_info:library_deps(El) || El <- World]),
    Specs = [create_app_spec(Annots, App, ActiveApps, LibraryApps) || App <- World],
    {ok, Rel#release_t{annotations=Annots,
                       applications=Specs,
                       app_detail=World,
                       realized=true}}.

-spec create_app_spec(annotations(), rlx_app_info:t(), [app_name()],
                      [app_name()]) ->
                             application_spec().
create_app_spec(Annots, App, ActiveApps, LibraryApps) ->
    %% If the app only exists as a dependency in a library app then it should
    %% get the 'load' annotation unless the release spec has provided something
    %% else
    AppName = rlx_app_info:name(App),
    TypeAnnot =
        case (lists:member(AppName, LibraryApps) and
              (not lists:member(AppName, ActiveApps))) of
            true ->
                load;
            false ->
                void
        end,
    BaseAnnots =
        try
            case ec_dictionary:get(AppName, Annots) of
                {void, Incld} ->
                    {TypeAnnot, Incld};
                Else ->
                    Else
            end
        catch
            throw:not_found ->
                {TypeAnnot, void}
        end,
    Vsn = rlx_app_info:original_vsn(App),
    case BaseAnnots of
        {void, void} ->
            {AppName, Vsn};
        {Type, void} ->
            {AppName, Vsn, Type};
        {void, Incld0} ->
            {AppName, Vsn, Incld0};
        {Type, Incld1} ->
            {AppName, Vsn, Type, Incld1}
    end.

-spec subset_world([{app_name(), app_vsn()}], [rlx_app_info:t()]) -> [rlx_app_info:t()].
subset_world(Pkgs, World) ->
    [get_app_info(Pkg, World) || Pkg <- Pkgs].

-spec get_app_info({app_name(), app_vsn()}, [rlx_app_info:t()]) -> rlx_app_info:t().
get_app_info({PkgName, PkgVsn}, World) ->
    {ok, WorldEl} =
        ec_lists:find(fun(El) ->
                              rlx_app_info:name(El) =:= PkgName andalso
                                  rlx_app_info:vsn(El) =:= PkgVsn
                      end, World),
    WorldEl.

parse_goal0({Constraint0, Annots}, {ok, Release})
  when Annots =:= permanent;
       Annots =:= transient;
       Annots =:= temporary;
       Annots =:= load;
       Annots =:= none ->
    case parse_constraint(Constraint0) of
        {ok, Constraint1} ->
            parse_goal1(Release, Constraint1, {Annots, void});
        Error  ->
            Error
    end;
parse_goal0({Constraint0, Annots, Incls}, {ok, Release})
  when (Annots =:= permanent orelse
            Annots =:= transient orelse
            Annots =:= temporary orelse
            Annots =:= load orelse
            Annots =:= none),
       erlang:is_list(Incls) ->
    case parse_constraint(Constraint0) of
        {ok, Constraint1} ->
            parse_goal1(Release, Constraint1, {Annots, Incls});
        Error  ->
            Error
    end;
parse_goal0(Constraint0, {ok, Release}) ->
    case parse_constraint(Constraint0) of
        {ok, Constraint1} ->
            parse_goal1(Release, Constraint1, {void, void});
        Error  ->
            Error
    end;
parse_goal0(_, E = {error, _}) ->
    E;
parse_goal0(Constraint, _) ->
    ?RLX_ERROR({invalid_constraint, 1, Constraint}).

parse_goal1(Release = #release_t{annotations=Annots,  goals=Goals},
            Constraint, NewAnnots) ->
    case get_app_name(Constraint) of
        E1 = {error, _} ->
            E1;
        AppName ->
            {ok,
             Release#release_t{annotations=ec_dictionary:add(AppName, NewAnnots, Annots),
                               goals = [Constraint | Goals]}}
    end.

-spec parse_constraint(application_constraint()) ->
                              rlx_depsolver:constraint() | relx:error().
parse_constraint(Constraint0)
  when erlang:is_list(Constraint0); erlang:is_binary(Constraint0) ->
    case rlx_goal:parse(Constraint0) of
        {fail, _} ->
            ?RLX_ERROR({failed_to_parse, Constraint0});
        {ok, Constraint1} ->
            {ok, Constraint1}
    end;
parse_constraint(Constraint0)
  when erlang:is_tuple(Constraint0);
       erlang:is_atom(Constraint0) ->
    Constraint1 = parse_version(Constraint0),
    case rlx_depsolver:is_valid_constraint(Constraint1) of
        false ->
            ?RLX_ERROR({invalid_constraint, 2, Constraint0});
        true ->
            {ok, Constraint1}
    end;
parse_constraint(Constraint) ->
    ?RLX_ERROR({invalid_constraint, 3, Constraint}).

-spec get_app_name(rlx_depsolver:raw_constraint()) ->
                          AppName::atom() | relx:error().
get_app_name(AppName) when erlang:is_atom(AppName) ->
    AppName;
get_app_name({AppName, _}) when erlang:is_atom(AppName) ->
    AppName;
get_app_name({AppName, _, _}) when erlang:is_atom(AppName) ->
    AppName;
get_app_name({AppName, _, _, _}) when erlang:is_atom(AppName) ->
    AppName;
get_app_name(V) ->
    ?RLX_ERROR({invalid_constraint, 4, V}).

-spec parse_version(rlx_depsolver:raw_constraint()) ->
                           rlx_depsolver:constraint().
parse_version({AppName, Version})
  when erlang:is_binary(Version);
       erlang:is_list(Version) ->
    {AppName, rlx_depsolver:parse_version(Version)};
parse_version({AppName, Version, Constraint})
  when erlang:is_binary(Version);
       erlang:is_list(Version) ->
    {AppName, rlx_depsolver:parse_version(Version), Constraint};
parse_version({AppName, Version, Constraint0, Constraint1})
  when erlang:is_binary(Version);
       erlang:is_list(Version) ->
    {AppName, rlx_depsolver:parse_version(Version), Constraint1, Constraint0};
parse_version(Constraint) ->
    Constraint.

to_atom(RelName)
  when erlang:is_list(RelName) ->
    erlang:list_to_atom(RelName);
to_atom(Else)
  when erlang:is_atom(Else) ->
    Else.
