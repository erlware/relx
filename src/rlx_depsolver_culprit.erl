%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sx=4 et
%%
%% @author Eric Merritt <ericbmerritt@gmail.com>
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
%%%-------------------------------------------------------------------
%%% @doc
%%%  This app does the culprit search for a failed solve. It searches
%%%  through the goals provided by the user trying to find the first one
%%%  that fails. It then returns that as the culprit along with the
%%%  unknown apps from the goal, the version constrained apps from the
%%%  goal, and the good apps (those not immediately constrained from
%%%  the goals).
%%% @end
-module(rcl_depsolver_culprit).

-export([search/3,
        format_error/1,
        format_version/1,
        format_constraint/1,
        format_roots/1,
        format_culprits/1]).

%%============================================================================
%% Types
%%============================================================================

%%============================================================================
%% Internal API
%%============================================================================
%% @doc start running the solver, with each run reduce the number of constraints
%% set as goals. At some point the solver should succeed.
-spec search(rcl_depsolver:dep_graph(), [rcl_depsolver:constraint()], [rcl_depsolver:constraint()])
            -> term().
search(State, ActiveCons, []) ->
    case rcl_depsolver:primitive_solve(State, ActiveCons, keep_paths) of
        {fail, FailPaths} ->
            extract_culprit_information0(ActiveCons, lists:flatten(FailPaths));
        _Success ->
            %% This should *never* happen. 'Culprit' above represents the last
            %% possible constraint that could cause things to fail. There for
            %% this should have failed as well.
            inconsistant_graph_state
    end;
search(State, ActiveCons, [NewCon | Constraints]) ->
    case rcl_depsolver:primitive_solve(State, ActiveCons, keep_paths) of
        {fail, FailPaths} ->
            extract_culprit_information0(ActiveCons, lists:flatten(FailPaths));
        _Success ->
            %% Move one constraint from the inactive to the active
            %% constraints and run again
            search(State, [NewCon | ActiveCons], Constraints)
    end.

format_error({error, {unreachable_package, AppName}}) ->
    ["Dependency ", format_constraint(AppName), " is specified as a dependency ",
     "but is not reachable by the system.\n"];
format_error({error, {invalid_constraints, Constraints}}) ->
    ["Invalid constraint ", add_s(Constraints), " specified ",
     lists:foldl(fun(Con, "") ->
                         [io_lib:format("~p", [Con])];
                    (Con, Acc) ->
                         [io_lib:format("~p", [Con]), ", " | Acc]
                 end, "", Constraints)];
format_error({error, Detail}) ->
    format_error(Detail);
format_error(Details) when erlang:is_list(Details) ->
    ["Unable to solve constraints, the following solutions were attempted \n\n",
     [[format_error_path("    ", Detail)] || Detail <- Details]].

-spec format_roots([rcl_depsolver:constraints()]) -> iolist().
format_roots(Roots) ->
    lists:foldl(fun(Root, Acc0) ->
                        lists:foldl(
                          fun(Con, "") ->
                                  [format_constraint(Con)];
                             (Con, Acc1) ->
                                  [format_constraint(Con), ", "  | Acc1]
                          end, Acc0, Root)
                end, [], Roots).

-spec format_culprits([{[rcl_depsolver:constraint()], [rcl_depsolver:constraint()]}]) -> iolist().
format_culprits(FailingDeps) ->
    Deps = sets:to_list(sets:from_list(lists:flatten([[rcl_depsolver:dep_pkg(Con) || Con <- Cons]
                                                      || {_, Cons} <- FailingDeps]))),
    lists:foldl(fun(Con, "") ->
                        [format_constraint(Con)];
                   (Con, Acc1) ->
                        [format_constraint(Con),
                        ", " | Acc1]
                end, [], Deps).

-spec format_version(rcl_depsolver:vsn()) -> iolist().
format_version(Vsn) ->
    ec_semver:format(Vsn).

-spec format_constraint(rcl_depsolver:constraint()) -> list().
format_constraint(Pkg) when is_atom(Pkg) ->
    erlang:atom_to_list(Pkg);
format_constraint(Pkg) when is_binary(Pkg) ->
    erlang:binary_to_list(Pkg);
format_constraint({Pkg, Vsn}) when is_tuple(Vsn) ->
    ["(", format_constraint(Pkg), " = ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, '='}) when is_tuple(Vsn) ->
    ["(", format_constraint(Pkg), " = ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, gte}) ->
    ["(", format_constraint(Pkg), " >= ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, '>='}) ->
    ["(", format_constraint(Pkg), " >= ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, lte}) ->
    ["(", format_constraint(Pkg), " <= ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, '<='}) ->
    ["(", format_constraint(Pkg), " <= ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, gt}) ->
    ["(", format_constraint(Pkg), " > ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, '>'}) ->
    ["(", format_constraint(Pkg), " > ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, lt}) ->
    ["(", format_constraint(Pkg), " < ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, '<'}) ->
    ["(", format_constraint(Pkg), " < ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, pes}) ->
    ["(", format_constraint(Pkg), " ~> ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn, '~>'}) ->
    ["(", format_constraint(Pkg), " ~> ",
     format_version(Vsn), ")"];
format_constraint({Pkg, Vsn1, Vsn2, between}) ->
    ["(", format_constraint(Pkg), " between ",
     format_version(Vsn1), " and ",
     format_version(Vsn2), ")"].


%%============================================================================
%% Internal Functions
%%============================================================================
-spec append_value(term(), term(), proplists:proplist()) -> proplists:proplist().
append_value(Key, Value, PropList) ->
    case proplists:get_value(Key, PropList, undefined) of
        undefined ->
            [{Key, Value} | PropList];
        ExistingValue ->
            [{Key, sets:to_list(sets:from_list([Value | ExistingValue]))} |
             proplists:delete(Key, PropList)]
    end.

-spec strip_goal([[rcl_depsolver:pkg()] | rcl_depsolver:pkg()]) ->
                        [[rcl_depsolver:pkg()] | rcl_depsolver:pkg()].
strip_goal([{'_GOAL_', 'NO_VSN'}, Children]) ->
    Children;
strip_goal(All = [Val | _])
  when erlang:is_list(Val) ->
    [strip_goal(Element) || Element <- All];
strip_goal(Else) ->
    Else.

-spec extract_culprit_information0(rcl_depsolver:constraints(),
                             [rcl_depsolver:fail_info()]) ->
                                           [term()].
extract_culprit_information0(ActiveCons, FailInfo)
  when is_list(FailInfo) ->
    [extract_culprit_information1(ActiveCons, FI) || FI <- FailInfo].


-spec extract_root(rcl_depsolver:constraints(), [rcl_depsolver:pkg()]) ->
                          {[rcl_depsolver:constraint()], [rcl_depsolver:pkg()]}.
extract_root(ActiveCons, TPath = [PRoot | _]) ->
    RootName = rcl_depsolver:dep_pkg(PRoot),
    Roots = lists:filter(fun(El) ->
                                 RootName =:= rcl_depsolver:dep_pkg(El)
                         end, ActiveCons),
    {Roots, TPath}.

-spec extract_culprit_information1(rcl_depsolver:constraints(),
                                  rcl_depsolver:fail_info()) ->
                                         term().
extract_culprit_information1(_ActiveCons, {[], RawConstraints}) ->
    %% In this case where there was no realized versions, the GOAL
    %% constraints actually where unsatisfiable
    Constraints = lists:flatten([Constraints ||
                                    {_, Constraints} <- RawConstraints]),

    Cons = [Pkg || {Pkg, Src} <- Constraints,
                   Src =:= {'_GOAL_', 'NO_VSN'}],
    {[{Cons, Cons}], []};
extract_culprit_information1(ActiveCons, {Path, RawConstraints}) ->
    Constraints = lists:flatten([Constraints ||
                                    {_, Constraints} <- RawConstraints]),
    FailCons =
        lists:foldl(fun(El = {FailedPkg, FailedVsn}, Acc1) ->
                            case get_constraints(FailedPkg, FailedVsn, Path,
                                                 Constraints) of
                                [] ->
                                    Acc1;
                                Cons ->
                                    append_value(El, Cons, Acc1)
                            end
                    end, [], lists:reverse(Path)),
    TreedPath = strip_goal(treeize_path({'_GOAL_', 'NO_VSN'}, Constraints, [])),
    RunListItems = [extract_root(ActiveCons, TPath) || TPath <- TreedPath],
    {RunListItems, FailCons}.

-spec follow_chain(rcl_depsolver:pkg_name(), rcl_depsolver:vsn(),
                   {rcl_depsolver:constraint(), rcl_depsolver:pkg()}) ->
                           false | {ok, rcl_depsolver:constraint()}.
follow_chain(Pkg, Vsn, {{Pkg, Vsn}, {Pkg, Vsn}}) ->
    %% When the package version is the same as the source we dont want to try to follow it at all
    false;
follow_chain(Pkg, Vsn, {Con, {Pkg, Vsn}}) ->
    {ok, Con};
follow_chain(_Pkg, _Vsn, _) ->
    false.

-spec find_chain(rcl_depsolver:pkg_name(), rcl_depsolver:vsn(),
                   [{rcl_depsolver:constraint(), rcl_depsolver:pkg()}]) ->
                          rcl_depsolver:constraints().
find_chain(Pkg, Vsn, Constraints) ->
    lists:foldl(fun(NCon, Acc) ->
                        case follow_chain(Pkg, Vsn, NCon) of
                            {ok, Con} ->
                                [Con | Acc];
                            false ->
                                Acc
                        end
                end, [], Constraints).

-spec get_constraints(rcl_depsolver:pkg_name(), rcl_depsolver:vsn(), [rcl_depsolver:pkg()],
                      [{rcl_depsolver:constraint(), rcl_depsolver:pkg()}]) ->
                             rcl_depsolver:constraints().
get_constraints(FailedPkg, FailedVsn, Path, Constraints) ->
    Chain = find_chain(FailedPkg, FailedVsn, Constraints),
    lists:filter(fun(Con) ->
                         PkgName = rcl_depsolver:dep_pkg(Con),
                         (lists:any(fun(PathEl) ->
                                            not rcl_depsolver:filter_package(PathEl, Con)
                                    end, Path) orelse
                          not lists:keymember(PkgName, 1, Path))
                 end, Chain).

-spec pkg_vsn(rcl_depsolver:constraint(),  [{rcl_depsolver:constraint(),
                                     rcl_depsolver:pkg()}]) ->
                      [rcl_depsolver:pkg()].
pkg_vsn(PkgCon, Constraints) ->
    PkgName = rcl_depsolver:dep_pkg(PkgCon),
    [DepPkg || Con = {DepPkg, _} <- Constraints,
               case Con of
                   {Pkg = {PkgName, PkgVsn}, {PkgName, PkgVsn}} ->
                       rcl_depsolver:filter_package(Pkg, PkgCon);
                   _ ->
                       false
               end].

-spec depends(rcl_depsolver:pkg(), [{rcl_depsolver:constraint(),
                                     rcl_depsolver:pkg()}],
                   [rcl_depsolver:pkg()]) ->
                           [rcl_depsolver:pkg()].
depends(SrcPkg, Constraints, Seen) ->
    lists:flatten([pkg_vsn(Pkg, Constraints) || {Pkg, Source} <- Constraints,
                                                Source =:= SrcPkg andalso
                                                    Pkg =/= SrcPkg andalso
                                                    not lists:member(Pkg, Seen)]).

-spec treeize_path(rcl_depsolver:pkg(), [{rcl_depsolver:constraint(),
                                     rcl_depsolver:pkg()}],
                   [rcl_depsolver:pkg()]) ->
                           [rcl_depsolver:pkg() | [rcl_depsolver:pkg()]].
treeize_path(Pkg, Constraints, Seen0) ->
    Seen1 = [Pkg | Seen0],
    case depends(Pkg, Constraints, Seen1) of
        [] ->
            [Pkg];
        Deps ->
            [Pkg,  [treeize_path(Dep, Constraints, Seen1) ||
                             Dep <- Deps]]

    end.

-spec add_s(list()) -> iolist().
add_s(Roots) ->
     case erlang:length(Roots) of
         Len when Len > 1 ->
             "s";
         _ ->
             ""
     end.

-spec format_path(string(), [rcl_depsolver:pkg()]) -> iolist().
format_path(CurrentIdent, Path) ->
    [CurrentIdent, "    ",
     lists:foldl(fun(Con, "") ->
                         [format_constraint(Con)];
                    (Con, Acc) ->
                         [format_constraint(Con), " -> " | Acc]
                 end, "", Path),
     "\n"].

-spec format_dependency_paths(string(), [[rcl_depsolver:pkg()] | rcl_depsolver:pkg()],
                                   [{rcl_depsolver:pkg(), [rcl_depsolver:constraint()]}], [rcl_depsolver:pkg()]) -> iolist().
format_dependency_paths(CurrentIndent, [SubPath | Rest], FailingDeps, Acc)
  when erlang:is_list(SubPath) ->
    [format_dependency_paths(CurrentIndent, lists:sort(SubPath), FailingDeps, Acc),
     format_dependency_paths(CurrentIndent, Rest, FailingDeps, Acc)];
format_dependency_paths(CurrentIndent, [Dep], FailingDeps, Acc)
  when erlang:is_tuple(Dep) ->
    case proplists:get_value(Dep, FailingDeps, undefined) of
        undefined ->
            format_path(CurrentIndent, [Dep | Acc]);
        Cons ->
            [format_path(CurrentIndent, [Con, Dep | Acc]) || Con <- Cons]
    end;
format_dependency_paths(CurrentIndent, [Dep | Rest], FailingDeps, Acc)
  when erlang:is_tuple(Dep) ->
    case proplists:get_value(Dep, FailingDeps, undefined) of
        undefined ->
            format_dependency_paths(CurrentIndent, Rest, FailingDeps, [Dep | Acc]);
        Cons ->
            [[format_path(CurrentIndent, [Con, Dep | Acc]) || Con <- Cons],
             format_dependency_paths(CurrentIndent, Rest, FailingDeps, [Dep | Acc])]
    end;
format_dependency_paths(CurrentIndent, [Con | Rest], FailingDeps, Acc) ->
    format_dependency_paths(CurrentIndent, Rest, FailingDeps, [Con | Acc]);
format_dependency_paths(_CurrentIndent, [], _FailingDeps, _Acc) ->
    [].

-spec format_error_path(string(), {[{[rcl_depsolver:constraint()], [rcl_depsolver:pkg()]}],
                                   [rcl_depsolver:constraint()]}) -> iolist().
format_error_path(CurrentIndent, {RawPaths, FailingDeps}) ->
    Roots = [RootSet || {RootSet, _} <- RawPaths],
    Paths = [Path || {_, Path} <- RawPaths],
    [CurrentIndent, "Unable to satisfy goal constraint",
     add_s(Roots), " ", format_roots(Roots), " due to constraint", add_s(FailingDeps), " on ",
     format_culprits(FailingDeps), "\n",
     format_dependency_paths(CurrentIndent, lists:sort(Paths), FailingDeps, []), ""].
