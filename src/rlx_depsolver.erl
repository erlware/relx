%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sx=4 et
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
%%%-------------------------------------------------------------------
%%% @doc
%%% This is a dependency constraint solver. You add your 'world' to the
%%% solver. That is the packages that exist, their versions and their
%%% dependencies. Then give the system a set of targets and ask it to solve.
%%%
%%%  Lets say our world looks as follows
%%%
%%%      app1 that has versions "0.1"
%%%        depends on app3 any version greater then "0.2"
%%%       "0.2" with no dependencies
%%%       "0.3" with no dependencies
%%%
%%%      app2 that has versions "0.1" with no dependencies
%%%       "0.2" that depends on app3 exactly "0.3"
%%%       "0.3" with no dependencies
%%%
%%%      app3 that has versions
%%%       "0.1", "0.2" and "0.3" all with no dependencies
%%%
%%% we can add this world to the system all at once as follows
%%%
%%%      Graph0 = rlx_depsolver:new_graph(),
%%%      Graph1 = rlx_depsolver:add_packages(
%%%             [{app1, [{"0.1", [{app2, "0.2"},
%%%                               {app3, "0.2", '>='}]},
%%%                               {"0.2", []},
%%%                               {"0.3", []}]},
%%%              {app2, [{"0.1", []},
%%%                       {"0.2",[{app3, "0.3"}]},
%%%                       {"0.3", []}]},
%%%              {app3, [{"0.1", []},
%%%                      {"0.2", []},
%%%                      {"0.3", []}]}]).
%%%
%%% We can also build it up incrementally using the other add_package and
%%% add_package_version functions.
%%%
%%% Finally, once we have built up the graph we can ask rlx_depsolver to solve the
%%% dependency constraints. That is to give us a list of valid dependencies by
%%% using the solve function. Lets say we want the app3 version "0.3" and all of
%%% its resolved dependencies. We could call solve as follows.
%%%
%%%    rlx_depsolver:solve(Graph1, [{app3, "0.3"}]).
%%%
%%% That will give us the completely resolved dependencies including app3
%%% itself. Lets be a little more flexible. Lets ask for a graph that is rooted
%%% in anything greater then or equal to app3 "0.3". We could do that by
%%%
%%%    rlx_depsolver:solve(Graph1, [{app3, "0.3", '>='}]).
%%%
%%% Of course, you can specify any number of goals at the top level.
%%% @end
%%%-------------------------------------------------------------------
-module(rlx_depsolver).

%% Public Api
-export([format_error/1,
         format_roots/1,
         format_culprits/1,
         format_constraint/1,
         format_version/1,
         new_graph/0,
         solve/2,
         add_packages/2,
         add_package/3,
         add_package_version/3,
         add_package_version/4,
         parse_version/1,
         is_valid_constraint/1,
         filter_packages/2]).

%% Internally Exported API. This should *not* be used outside of the rlx_depsolver
%% application. You have been warned.
-export([dep_pkg/1,
         filter_package/2,
         primitive_solve/3]).

-export_type([t/0,
              pkg/0,
              constraint_op/0,
              raw_constraint/0,
              pkg_name/0,
              vsn/0,
              constraint/0,
              dependency_set/0]).

-export_type([dep_graph/0, constraints/0,
              ordered_constraints/0, fail_info/0,
              fail_detail/0]).
%%============================================================================
%% type
%%============================================================================
-ifdef(namespaced_types).
-type dep_graph() :: gb_trees:tree().
-else.
-type dep_graph() :: gb_tree().
-endif.
-opaque t() :: {?MODULE, dep_graph()}.
-type pkg() :: {pkg_name(), vsn()}.
-type pkg_name() :: string() | binary() | atom().
-type raw_vsn() :: ec_semver:any_version().

-type vsn() :: 'NO_VSN'
             | ec_semver:semver().

-type constraint_op() ::
        '=' | gte | '>=' | lte | '<='
      | gt | '>' | lt | '<' | pes | '~>' | between.

-type raw_constraint() :: pkg_name()
                        | {pkg_name(), raw_vsn()}
                        | {pkg_name(), raw_vsn(), constraint_op()}
                        | {pkg_name(), raw_vsn(), vsn(), between}.

-type constraint() :: pkg_name()
                    | {pkg_name(), vsn()}
                    | {pkg_name(), vsn(), constraint_op()}
                    | {pkg_name(), vsn(), vsn(), between}.


-type vsn_constraint() :: {raw_vsn(), [raw_constraint()]}.
-type dependency_set() :: {pkg_name(), [vsn_constraint()]}.

%% Internal Types
-type constraints() :: [constraint()].
-type ordered_constraints() :: [{pkg_name(), constraints()}].
-type fail_info() :: {[pkg()], ordered_constraints()}.
-type fail_detail() :: {fail, [fail_info()]}.
-type version_checker() :: fun((vsn()) -> fail_detail() | vsn()).

%%============================================================================
%% API
%%============================================================================
%% @doc create a new empty dependency graph
-spec new_graph() -> t().
new_graph() ->
    {?MODULE, gb_trees:empty()}.

%% @doc add a complete set of list of packages to the graph. Where the package
%% consists of the name and a list of versions and dependencies.
%%
%% ``` rlx_depsolver:add_packages(Graph,
%%               [{app1, [{"0.1", [{app2, "0.2"},
%%                                 {app3, "0.2", '>='}]},
%%                                 {"0.2", []},
%%                                 {"0.3", []}]},
%%                 {app2, [{"0.1", []},
%%                         {"0.2",[{app3, "0.3"}]},
%%                         {"0.3", []}]},
%%                 {app3, [{"0.1", []},
%%                         {"0.2", []},
%%                         {"0.3", []}]}])
%% '''
-spec add_packages(t(),[dependency_set()]) -> t().
add_packages(Dom0, Info)
  when is_list(Info) ->
    lists:foldl(fun({Pkg, VsnInfo}, Dom1) ->
                        add_package(Dom1, Pkg, VsnInfo)
                end, Dom0, Info).

%% @doc add a single package to the graph, where it consists of a package name
%% and its versions and thier dependencies.
%%  ```rlx_depsolver:add_package(Graph, app1, [{"0.1", [{app2, "0.2"},
%%                                              {app3, "0.2", '>='}]},
%%                                              {"0.2", []},
%%                                              {"0.3", []}]}]).
%% '''
-spec add_package(t(),pkg_name(),[vsn_constraint()]) -> t().
add_package(State, Pkg, Versions)
  when is_list(Versions) ->
    lists:foldl(fun({Vsn, Constraints}, Dom1) ->
                        add_package_version(Dom1, Pkg, Vsn, Constraints);
                   (Version, Dom1) ->
                        add_package_version(Dom1, Pkg, Version, [])
                end, State, Versions).

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

%% @doc add a package and version to the dependency graph with no dependency
%% constraints, dependency constraints can always be added after the fact.
%%
%% ```rlx_depsolver:add_package_version(Graph, app1, "0.1").'''
-spec add_package_version(t(),pkg_name(),raw_vsn()) -> t().
add_package_version(State, Pkg, Vsn) ->
    add_package_version(State, Pkg, Vsn, []).

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
            case primitive_solve(DepGraph1, Goals, no_path) of
                {fail, _} ->
                    [FirstCons | Rest] = Goals,
                    {error, rlx_depsolver_culprit:search(DepGraph1, [FirstCons], Rest)};
                Solution ->
                    {ok, Solution}
            end
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

%% @doc check that a specified constraint is a valid constraint.
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


%% @doc given a list of package name version pairs, and a list of constraints
%% return every member of that list that matches all constraints.
-spec filter_packages([{pkg_name(), raw_vsn()}], [raw_constraint()]) ->
                             {ok, [{pkg_name(), raw_vsn()}]}
                                 | {error, Reason::term()}.
filter_packages(PVPairs, RawConstraints) ->
    Constraints = [fix_con(Constraint) || Constraint <- RawConstraints],
    case check_constraints(Constraints) of
        ok ->
            {ok, [PVPair || PVPair <- PVPairs,
                            filter_pvpair_by_constraint(fix_con(PVPair), Constraints)]};
        Error ->
            Error
    end.

%% @doc Produce a full error message for a given error condition.  This includes
%% details of the paths taken to resolve the dependencies and shows which dependencies
%% could not be satisfied
-spec format_error({error, {unreachable_package, list()} |
                           {invalid_constraints, [constraint()]} |
                           list()}) -> iolist().
format_error(Error) ->
    rlx_depsolver_culprit:format_error(Error).

%% @doc Return a formatted list of roots of the dependency trees which
%% could not be satisified. These may also have versions attached.
%% Example:
%%
%%    ```(foo = 1.2.0), bar```
%%
-spec format_roots([constraints()]) -> iolist().
format_roots(Roots) ->
    rlx_depsolver_culprit:format_roots(Roots).


%% @doc Return a formatted list of the culprit depenedencies which led to
%% the dependencies not being satisfied. Example:
%%
%%     ```(foo = 1.2.0) -> (bar > 2.0.0)```
-spec format_culprits([{[constraint()], [constraint()]}]) -> iolist().
format_culprits(Culprits) ->
    rlx_depsolver_culprit:format_culprits(Culprits).

%% @doc A formatted version tuple
-spec format_version(vsn()) -> iolist().
format_version('NO_VSN') ->
    "";
format_version(Version) ->
    rlx_depsolver_culprit:format_version(Version).

%% @doc A formatted constraint tuple
-spec format_constraint(constraint()) -> iolist().
format_constraint(Constraint) ->
    rlx_depsolver_culprit:format_constraint(Constraint).

%%====================================================================
%% Internal Functions
%%====================================================================
-spec check_constraints(constraints()) ->
                               ok | {error, {invalid_constraints, [term()]}}.
check_constraints(Constraints) ->
    PossibleInvalids =
        lists:foldl(fun(Constraint, InvalidConstraints) ->
                            case is_valid_constraint(Constraint) of
                                true ->
                                    InvalidConstraints;
                                false ->
                                [Constraint | InvalidConstraints]
                            end
                    end, [], Constraints),
    case PossibleInvalids of
        [] ->
            ok;
        _ ->
        {error, {invalid_constraints, PossibleInvalids}}
    end.


-spec filter_pvpair_by_constraint({pkg_name(), vsn()}, [constraint()]) ->
                                         boolean().
filter_pvpair_by_constraint(PVPair, Constraints) ->
    lists:all(fun(Constraint) ->
                      filter_package(PVPair, Constraint)
              end, Constraints).

-spec filter_package({pkg_name(), vsn()}, constraint()) ->
                            boolean().
filter_package({PkgName, Vsn}, C = {PkgName, _}) ->
    is_version_within_constraint(Vsn, C);
filter_package({PkgName, Vsn}, C = {PkgName, _, _}) ->
    is_version_within_constraint(Vsn, C);
filter_package({PkgName, Vsn}, C = {PkgName, _, _, _}) ->
    is_version_within_constraint(Vsn, C);
filter_package(_, _) ->
    %% If its not explicitly excluded its included
    true.

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
fix_con({Pkg, Vsn, CI}) ->
    {fix_pkg(Pkg), parse_version(Vsn), CI};
fix_con({Pkg, Vsn1, Vsn2, CI}) ->
    {fix_pkg(Pkg), parse_version(Vsn1),
     parse_version(Vsn2), CI};
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


%% @doc constraints is an associated list keeping track of all the constraints
%% that have been placed on a package
-spec new_constraints() -> constraints().
new_constraints() ->
    [].

%% @doc Given a dep graph and a set of goals this either solves the problem or
%% fails. This is basically the root solver of the system, the main difference
%% from the exported solve/2 function is the fact that this does not do the
%% culprit search.
-spec primitive_solve(dep_graph(),[constraint()], term()) ->
                             [pkg()] | fail_detail().
primitive_solve(State, PackageList, PathInd)
  when erlang:length(PackageList) > 0 ->
    Constraints = lists:foldl(fun(Info, Acc) ->
                                      add_constraint('_GOAL_', 'NO_VSN', Acc, Info)
                              end, new_constraints(), PackageList),

    Pkgs = lists:map(fun dep_pkg/1, PackageList),
    all_pkgs(State, [], Pkgs, Constraints, PathInd).

%% @doc
%% given a Pkg | {Pkg, Vsn} | {Pkg, Vsn, Constraint} return Pkg
-spec dep_pkg(constraint()) -> pkg_name().
dep_pkg({Pkg, _Vsn}) ->
    Pkg;
dep_pkg({Pkg, _Vsn, _}) ->
    Pkg;
dep_pkg({Pkg, _Vsn1, _Vsn2, _}) ->
    Pkg;
dep_pkg(Pkg) when is_atom(Pkg) orelse is_binary(Pkg) ->
    Pkg.

-spec add_constraint(pkg_name(), vsn(), [constraint()],constraint()) -> ordered_constraints().
add_constraint(SrcPkg, SrcVsn, PkgsConstraints, PkgConstraint) ->
    case is_valid_constraint(PkgConstraint) of
        true -> ok;
        false -> erlang:throw({invalid_constraint, PkgConstraint})
    end,
    PkgName = dep_pkg(PkgConstraint),
    Constraints1 =
        case lists:keysearch(PkgName, 1, PkgsConstraints) of
            false ->
                [];
            {value, {PkgName, Constraints0}} ->
                Constraints0
        end,
    [{PkgName, [{PkgConstraint, {SrcPkg, SrcVsn}} | Constraints1]}
    | lists:keydelete(PkgName, 1, PkgsConstraints)].

%% @doc
%% Extend the currently active constraints correctly for the given constraints.
-spec extend_constraints(pkg_name(), vsn(), constraints(),constraints()) ->
                                [{pkg_name(), constraints()}].
extend_constraints(SrcPkg, SrcVsn, ExistingConstraints0, NewConstraints) ->
    lists:foldl(fun (Constraint, ExistingConstraints1) ->
                        add_constraint(SrcPkg, SrcVsn, ExistingConstraints1, Constraint)
                end,
                ExistingConstraints0, [{SrcPkg, SrcVsn} | NewConstraints]).

-spec is_version_within_constraint(vsn(),constraint()) -> boolean().
is_version_within_constraint(_Vsn, Pkg) when is_atom(Pkg) orelse is_binary(Pkg) ->
    true;
is_version_within_constraint(Vsn, {_Pkg, NVsn}) ->
    ec_semver:eql(Vsn, NVsn);
is_version_within_constraint(Vsn, {_Pkg, NVsn, '='}) ->
    ec_semver:eql(Vsn, NVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, gte})  ->
    ec_semver:gte(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, '>='}) ->
    ec_semver:gte(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, lte}) ->
    ec_semver:lte(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, '<='}) ->
    ec_semver:lte(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, gt}) ->
    ec_semver:gt(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, '>'}) ->
    ec_semver:gt(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, lt}) ->
    ec_semver:lt(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, '<'}) ->
    ec_semver:lt(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, 'pes'}) ->
    ec_semver:pes(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, '~>'}) ->
    ec_semver:pes(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn1, LVsn2, between}) ->
    ec_semver:between(LVsn1, LVsn2, Vsn);
is_version_within_constraint(_Vsn, _Pkg) ->
    false.

%% @doc
%% Get the currently active constraints that relate to the specified package
-spec get_constraints([{pkg_name(), constraints()}],pkg_name()) -> constraints().
get_constraints(PkgsConstraints, PkgName) ->
    case lists:keysearch(PkgName, 1, PkgsConstraints) of
        false ->
            [];
        {value, {PkgName, Constraints}} ->
            Constraints
    end.

%% @doc
%% Given a package name get the list of all versions available for that package.
-spec get_versions(dep_graph(),pkg_name()) -> [vsn()].
get_versions(DepGraph, PkgName) ->
    case gb_trees:lookup(PkgName, DepGraph) of
        none ->
            [];
        {value, AllVsns} ->
            [Vsn || {Vsn, _} <- AllVsns]
    end.

%% @doc
%% make sure a given name/vsn meets all current constraints
-spec valid_version(pkg_name(),vsn(),constraints()) -> boolean().
valid_version(PkgName, Vsn, PkgConstraints) ->
    lists:all(fun ({L, _ConstraintSrc}) ->
                      is_version_within_constraint(Vsn, L)
              end,
              get_constraints(PkgConstraints, PkgName)).

%% @doc
%% Given a Package Name and a set of constraints get a list of package
%% versions that meet all constraints.
-spec constrained_package_versions(dep_graph(),pkg_name(),constraints()) ->
                                          [vsn()].
constrained_package_versions(State, PkgName, PkgConstraints) ->
    Versions = get_versions(State, PkgName),
    [Vsn || Vsn <- Versions, valid_version(PkgName, Vsn, PkgConstraints)].

%% Given a list of constraints filter said list such that only fail (for things
%% that do not match a package and pkg are returned. Since at the end only pkg()
%% we should have a pure list of packages.
-spec filter_package_constraints([constraint()]) -> fail | pkg().
filter_package_constraints([]) ->
    fail;
filter_package_constraints([PkgCon | PkgConstraints]) ->
    case PkgCon of
        {Pkg, _} when is_atom(Pkg); is_binary(Pkg) ->
            filter_package_constraints(PkgConstraints);
        {{_Pkg1, _Vsn} = PV, _} ->
            PV;
        {{_Pkg2, _Vsn, _R}, _} ->
            filter_package_constraints(PkgConstraints);
        {{_Pkg2, _Vsn1, _Vsn2, _R}, _} ->
            filter_package_constraints(PkgConstraints)
    end.

%% @doc all_pkgs is one of the set of mutually recursive functions (all_pkgs and
%% pkgs) that serve to walk the solution space of dependency.
-spec all_pkgs(dep_graph(),[pkg()],[pkg_name()],[{pkg_name(), constraints()}], term()) ->
                      fail_detail() | [pkg()].
all_pkgs(_State, Visited, [], Constraints, _PathInd) ->
    PkgVsns =
        [filter_package_constraints(PkgConstraints)
         || {_, PkgConstraints} <- Constraints],

    %% PkgVsns should be a list of pkg() where all the constraints are correctly
    %% met. If not we fail the solution. If so we return those pkg()s
    case lists:all(fun({Pkg, Vsn}) ->
                           lists:all(fun({Constraint, _}) ->
                                             is_version_within_constraint(Vsn, Constraint)
                                     end,  get_constraints(Constraints, Pkg))
                   end, PkgVsns) of
        true ->
            PkgVsns;
        false ->
            {fail, [{Visited, Constraints}]}
    end;
all_pkgs(State, Visited, [PkgName | PkgNames], Constraints, PathInd)
  when is_atom(PkgName); is_binary(PkgName) ->
    case lists:keymember(PkgName, 1, Visited) of
        true ->
            all_pkgs(State, Visited, PkgNames, Constraints, PathInd);
        false ->
            pkgs(State, Visited, PkgName, Constraints, PkgNames, PathInd)
    end.

%% @doc this is the key graph walker. Set of constraints it walks forward into
%% the solution space searching for a path that solves all dependencies.
-spec pkgs(dep_graph(),[pkg()], pkg_name(), [{pkg_name(), constraints()}],
           [pkg_name()], term()) -> fail_detail() | [pkg()].
pkgs(DepGraph, Visited, Pkg, Constraints, OtherPkgs, PathInd) ->
    F = fun (Vsn) ->
                Deps = get_dep_constraints(DepGraph, Pkg, Vsn),
                UConstraints = extend_constraints(Pkg, Vsn, Constraints, Deps),
                DepPkgs =[dep_pkg(Dep) || Dep <- Deps],
                NewVisited = [{Pkg, Vsn} | Visited],
                Res = all_pkgs(DepGraph, NewVisited, DepPkgs ++ OtherPkgs, UConstraints, PathInd),
                Res
        end,
    case constrained_package_versions(DepGraph, Pkg, Constraints) of
        [] ->
            {fail, [{Visited, Constraints}]};
        Res ->
            lists_some(F, Res, PathInd)
    end.


%% @doc This gathers the dependency constraints for a given package vsn from the
%% dependency graph.
-spec get_dep_constraints(dep_graph(), pkg_name(), vsn()) -> [constraint()].
get_dep_constraints(DepGraph, PkgName, Vsn) ->
    {Vsn, Constraints} = lists:keyfind(Vsn, 1,
                                       gb_trees:get(PkgName, DepGraph)),
    Constraints.


lists_some(F, Res, PathInd) ->
    lists_some(F, Res, [], PathInd).

-spec lists_some(version_checker(), [vsn()], term(), term()) -> vsn() | fail_detail().
%% @doc lists_some is the root of the system the actual backtracing search that
%% makes the dep solver posible. It a takes a function that checks whether the
%% 'problem' has been solved and an fail indicator. As long as the evaluator
%% returns the fail indicator processing continues. If the evaluator returns
%% anything but the fail indicator that indicates success.
lists_some(_, [], FailPaths, _PathInd) ->
    {fail, FailPaths};
lists_some(F, [H | T], FailPaths, PathInd) ->
    case F(H) of
        {fail, FailPath} ->
            case PathInd of
                keep_paths ->
                    lists_some(F, T, [FailPath | FailPaths], PathInd);
                _ ->
                    lists_some(F, T, [], PathInd)
            end;
        Res ->
            Res
    end.

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
