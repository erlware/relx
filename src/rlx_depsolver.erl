%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%%
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @author Eric Merritt <ericbmerritt@gmail.com>
%%
-module(rlx_depsolver).

%% Public Api
-export([solve/2,
         format_error/1,
         is_valid_raw_constraint/1,
         new_graph/0,
         add_package_version/4]).

-export_type([t/0,
              pkg/0,
              raw_constraint/0,
              pkg_name/0,
              constraint/0,
              dependency_set/0]).

%%============================================================================
%% type
%%============================================================================

-type dep_graph() :: gb_trees:tree().

-opaque t() :: {?MODULE, dep_graph()}.
-type vsn() :: ec_semver:any_version().
-type pkg() :: {pkg_name(), vsn()}.
-type pkg_name() :: string() | binary() | atom().
-type raw_vsn() :: ec_semver:any_version().

-type raw_constraint() :: pkg_name()
                        | {pkg_name(), raw_vsn()}.

-type constraint() :: pkg_name()
                    | {pkg_name(), vsn()}.


-type vsn_constraint() :: {raw_vsn(), [raw_constraint()]}.
-type dependency_set() :: {pkg_name(), [vsn_constraint()]}.


%%============================================================================
%% API
%%============================================================================
%% @doc create a new empty dependency graph
-spec new_graph() -> t().
new_graph() ->
    {?MODULE, gb_trees:empty()}.

%% @doc add a set of dependencies to a specific package and version.
%% and its versions and thier dependencies.
%%  ```rlx_depsolver:add_package(Graph, app1, "0.1", [{app2, "0.2"},
%%                                              {app3, "0.2", '>='}]},
%%                                              {"0.2", []},
%%                                              {"0.3", []}]).
%% '''
-spec add_package_version(t(), pkg_name(), raw_vsn(), [raw_constraint()]) -> t().
add_package_version({?MODULE, Dom0}, RawPkg, RawVsn, RawPkgConstraints) ->
    Pkg = fix_pkg(RawPkg),
    Vsn = parse_version(RawVsn),
    %% Incoming constraints are raw
    %% and need to be fixed
    PkgConstraints = [fix_con(PkgConstraint) ||
                         PkgConstraint <- RawPkgConstraints],
    Info2 =
        case gb_trees:lookup(Pkg, Dom0) of
            {value, Info0} ->
                %% Sort Vsns from greatest to lowest
                lists:reverse(
                  lists:sort(fun(X, Y) ->
                                     ec_semver:lte(element(1, X), element(1, Y))
                             end,
                            case lists:keytake(Vsn, 1, Info0) of
                                {value, {Vsn, Constraints}, Info1} ->
                                    [{Vsn, join_constraints(Constraints,
                                                            PkgConstraints)}
                                     | Info1];
                                false ->
                                    [{Vsn,  PkgConstraints}  | Info0]

                            end));
            none ->
                [{Vsn, PkgConstraints}]
        end,
    {?MODULE, gb_trees:enter(Pkg, Info2, Dom0)}.

%% @doc Given a set of goals (in the form of constrains) find a set of packages
%% and versions that satisfy all constraints. If no solution can be found then
%% an exception is thrown.
%% ``` rlx_depsolver:solve(State, [{app1, "0.1", '>='}]).'''
-spec solve(t(),[constraint()]) -> {ok, [pkg()]} | {error, term()}.
solve({?MODULE, DepGraph0}, RawGoals)
  when erlang:length(RawGoals) > 0 ->
    Goals = [fix_con(Goal) || Goal <- RawGoals],
    case trim_unreachable_packages(DepGraph0, Goals) of
        Error = {error, _} ->
            Error;
        DepGraph1 ->
            L = [{N, element(1, hd(O))} || {N, O} <- gb_trees:to_list(DepGraph1)],
            {ok, L}
    end.

%% @doc Parse a string version into a tuple based version
-spec parse_version(raw_vsn() | vsn()) -> vsn().
parse_version(RawVsn)
  when erlang:is_list(RawVsn);
       erlang:is_binary(RawVsn) ->
    ec_semver:parse(RawVsn);
parse_version(Vsn)
  when erlang:is_tuple(Vsn) ; erlang:is_atom(Vsn) ->
    Vsn.

-spec is_valid_raw_constraint(raw_constraint()) -> true; (any()) -> false.
is_valid_raw_constraint(RawConstraint) ->
    try fix_con(RawConstraint)
    of
        Constraint -> is_valid_constraint(Constraint)
    catch
        error:function_clause -> false
    end.

%% @doc Produce a full error message for a given error condition.  This includes
%% details of the paths taken to resolve the dependencies and shows which dependencies
%% could not be satisfied
-spec format_error({error, {unreachable_package, list()} |
                           {invalid_constraints, [constraint()]} |
                           list()}) -> iolist().
format_error(Error) ->
    rlx_depsolver_culprit:format_error(Error).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc
%% fix the package name. If its a list turn it into a binary otherwise leave it as an atom
fix_pkg(Pkg) when is_list(Pkg) ->
    erlang:list_to_binary(Pkg);
fix_pkg(Pkg) when is_binary(Pkg); is_atom(Pkg) ->
    Pkg.

%% @doc
%% fix package. Take a package with a possible invalid version and fix it.
-spec fix_con(raw_constraint()) -> constraint().
fix_con({Pkg, Vsn}) ->
    {fix_pkg(Pkg), parse_version(Vsn)};
fix_con(Pkg) ->
    fix_pkg(Pkg).

%% @doc given two lists of constraints join them in such a way that no
%% constraint is duplicated but the over all order of the constraints is
%% preserved. Order drives priority in this solver and is important for that
%% reason.
-spec join_constraints([constraint()], [constraint()]) ->
                              [constraint()].
join_constraints(NewConstraints, ExistingConstraints) ->
    ECSet = sets:from_list(ExistingConstraints),
    FilteredNewConstraints = [NC || NC <- NewConstraints,
                                    not sets:is_element(NC, ECSet)],
    ExistingConstraints ++ FilteredNewConstraints.

%% @doc
%% given a Pkg | {Pkg, Vsn} | {Pkg, Vsn, Constraint} return Pkg
-spec dep_pkg(constraint()) -> pkg_name().
dep_pkg({Pkg, _Vsn}) ->
    Pkg;
dep_pkg(Pkg) when is_atom(Pkg) orelse is_binary(Pkg) ->
    Pkg.

-spec is_valid_constraint(constraint()) -> boolean().
is_valid_constraint(Pkg) when is_atom(Pkg) orelse is_binary(Pkg) ->
    true;
is_valid_constraint({_Pkg, Vsn}) when is_tuple(Vsn) ->
    true;
is_valid_constraint({_Pkg, Vsn, '='}) when is_tuple(Vsn) ->
    true;
is_valid_constraint({_Pkg, _LVsn, gte}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, '>='}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, lte}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, '<='}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, gt}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, '>'}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, lt}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, '<'}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, pes}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, '~>'}) ->
    true;
is_valid_constraint({_Pkg, _LVsn1, _LVsn2, between}) ->
    true;
is_valid_constraint(_InvalidConstraint) ->
    false.

%% @doc given a graph and a set of top level goals return a graph that contains
%% only those top level packages and those packages that might be required by
%% those packages.
-spec trim_unreachable_packages(dep_graph(), [constraint()]) ->
                                       dep_graph() | {error, term()}.
trim_unreachable_packages(State, Goals) ->
    {_, NewState0} = new_graph(),
    lists:foldl(fun(_Pkg, Error={error, _}) ->
                        Error;
                   (Pkg, NewState1) ->
                        PkgName = dep_pkg(Pkg),
                        find_reachable_packages(State, NewState1, PkgName)
                end, NewState0, Goals).

%% @doc given a list of versions and the constraints for that version rewrite
%% the new graph to reflect the requirements of those versions.
-spec rewrite_vsns(dep_graph(), dep_graph(), [{vsn(), [constraint()]}]) ->
                          dep_graph() | {error, term()}.
rewrite_vsns(ExistingGraph, NewGraph0, Info) ->
    lists:foldl(fun(_, Error={error, _}) ->
                        Error;
                   ({_Vsn, Constraints}, NewGraph1) ->
                        lists:foldl(fun(_DepPkg, Error={error, _}) ->
                                            Error;
                                       (DepPkg, NewGraph2) ->
                                            DepPkgName = dep_pkg(DepPkg),
                                            find_reachable_packages(ExistingGraph,
                                                                    NewGraph2,
                                                                    DepPkgName)
                                    end, NewGraph1, Constraints)
                end, NewGraph0, Info).

%% @doc Rewrite the existing dep graph removing anything that is not reachable
%% required by the goals or any of its potential dependencies.
-spec find_reachable_packages(dep_graph(), dep_graph(), pkg_name()) ->
                                     dep_graph() | {error, term()}.
find_reachable_packages(_ExistingGraph, Error={error, _}, _PkgName) ->
    Error;
find_reachable_packages(ExistingGraph, NewGraph0, PkgName) ->
    case contains_package_version(NewGraph0, PkgName) of
        true ->
            NewGraph0;
        false ->
            case gb_trees:lookup(PkgName, ExistingGraph) of
                {value, Info} ->
                    NewGraph1 = gb_trees:insert(PkgName, Info, NewGraph0),
                    rewrite_vsns(ExistingGraph, NewGraph1, Info);
                none ->
                    {error, {unreachable_package, PkgName}}
            end
    end.

%% @doc
%%  Checks to see if a package name has been defined in the dependency graph
-spec contains_package_version(dep_graph(), pkg_name()) -> boolean().
contains_package_version(Dom0, PkgName) ->
    gb_trees:is_defined(PkgName, Dom0).
