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
%%% @doc This module represents a release and its metadata and is used to
%%% manipulate the release metadata.
-module(rcl_release).

-export([new/2,
         erts/2,
         erts/1,
         goals/2,
         goals/1,
         name/1,
         vsn/1,
         format/1,
         format/2]).

-export_type([t/0,
              name/0,
              vsn/0,
              app_name/0,
              app_vsn/0,
              app_type/0,
              application_spec/0,
              application_goal/0]).

-record(release_t, {name :: atom(),
                    vsn :: ec_semver:any_version(),
                    erts :: ec_semver:any_version(),
                    goals = [] :: [depsolver:constraint()],
                    realized = false :: boolean(),
                    annotations = undefined :: ec_dictionary:dictionary(app_name(),
                                                                        app_type() |
                                                                        incl_apps() |
                                                                        {app_type(), incl_apps()}),
                    applications = []:: [application_spec()]}).

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

-type application_goal() :: depsolver:constraint() |
                            {depsolver:constraint(), app_type() | incl_apps()} |
                            {depsolver:constraint(), app_type(), incl_apps()}.

-opaque t() :: record(release_t).

%%============================================================================
%% API
%%============================================================================
-spec new(atom(), string()) -> t().
new(ReleaseName, ReleaseVsn) ->
    #release_t{name=ReleaseName, vsn=ReleaseVsn,
               annotations=ec_dictionary:new(ec_dict)}.

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

-spec goals(t(), [application_goal()]) -> {ok, t()} | {error, Reason::term()}.
goals(Release, Goals0) ->
    lists:foldl(fun parse_goal0/2,
                {ok, Release}, Goals0).

-spec goals(t()) -> [application_goal()].
goals(#release_t{goals=Goals}) ->
    Goals.

-spec format(t()) -> iolist().
format(Release) ->
    format(0, Release).

-spec format(non_neg_integer(), t()) -> iolist().
format(Indent, #release_t{name=Name, vsn=Vsn, erts=ErtsVsn, realized=Realized,
                         goals = Goals, applications = Apps}) ->
    BaseIndent = rcl_util:indent(Indent),
    [BaseIndent, "release: ", erlang:atom_to_list(Name), "-", Vsn,
     " erts-", ErtsVsn, ", realized = ",  erlang:atom_to_list(Realized), "\n",
     BaseIndent, "goals: \n",
     [[rcl_util:indent(Indent + 1),  format_goal(Goal), ",\n"] || Goal <- Goals],
     case Realized of
         true ->
             [BaseIndent, "applications: \n",
              [[rcl_util:indent(Indent + 1),  io_lib:format("~p", [App]), ",\n"]
               || App <- Apps]];
         false ->
             []
     end].
-spec format_goal(application_goal()) -> iolist().
format_goal({Constraint, AppType}) ->
    io_lib:format("~p", [{depsolver:format_constraint(Constraint), AppType}]);
format_goal({Constraint, AppType, AppInc}) ->
    io_lib:format("~p", [{depsolver:format_constraint(Constraint), AppType, AppInc}]);
format_goal(Constraint) ->
    depsolver:format_constraint(Constraint).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec parse_goal0(application_goal(), {ok, t()} | {error, Reason::term()}) ->
                         {ok, t()} | {error, Reason::term()}.
parse_goal0({Constraint0, Annots}, {ok, Release}) ->
    parse_goal1(Release, Constraint0, Annots);
parse_goal0({Constraint0, AnnotsA, AnnotsB}, {ok, Release}) ->
    parse_goal1(Release, Constraint0, {AnnotsA, AnnotsB});
parse_goal0(Constraint0, {ok, Release = #release_t{goals=Goals}}) ->
    case rcl_goal:parse(Constraint0) of
        E = {error, _} ->
            E;
        Constraint1 ->
            {ok, Release#release_t{goals = [Constraint1 | Goals]}}
    end;
parse_goal0(_, E = {error, _}) ->
    E.

-spec parse_goal1(t(), depsolver:constraint() | string(),
                  app_type() | incl_apps() | {app_type(), incl_apps()}) ->
                         {ok, t()} | {error, Reason::term()}.
parse_goal1(Release = #release_t{annotations=Annots,  goals=Goals},
            Constraint0, NewAnnots) ->
   case rcl_goal:parse(Constraint0) of
       E0 = {error, _} ->
           E0;
       Constraint1 ->
           case get_app_name(Constraint1) of
               E1 = {error, _} ->
                   E1;
               AppName ->
                   {ok,
                    Release#release_t{annotations=ec_dictionary:add(AppName, NewAnnots, Annots),
                                    goals = [Constraint1 | Goals]}}
           end
   end.

-spec get_app_name(depsolver:constraint()) ->
                          AppName::atom() | {error, {invalid_constraint, term()}}.
get_app_name(AppName) when erlang:is_atom(AppName) ->
    AppName;
get_app_name({AppName, _, _}) when erlang:is_atom(AppName) ->
    AppName;
get_app_name({AppName, _, _, _}) when erlang:is_atom(AppName) ->
    AppName;
get_app_name(V) ->
    {error, {invalid_constraint, V}}.
