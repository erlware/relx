%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sx=4 et
%%-------------------------------------------------------------------
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
%% @doc
%% Additional testing for depsolver
%% @end
%%-------------------------------------------------------------------
-module(rlx_depsolver_tester).

-export([run_data/1, run_log/1]).
-include_lib("eunit/include/eunit.hrl").

-define(ADD_PKG, "^DepSelector\\sinst#\\s(\\d+)\\s-\\s"
        "Adding\\spackage\\sid\\s(\\d+)\\/(\\d+):\\smin\\s=\\s-1,"
        "\\smax\\s=\\s(\\d+),\\scurrent\\sversion\\s0$").
-define(ADD_VC, "^DepSelector\\sinst#\\s(\\d+)\\s-\\sAdding\\sVC\\s"
        "for\\s(\\d+)\\s@\\s(\\d+)\\sdepPkg\\s(\\d+)\\s\\[\\s(\\d+)"
        "\\s(\\d+)\\s\\]$").
-define(ADD_GOAL, "^DepSelector\\sinst#\\s(\\d+)\\s-\\s"
        "Marking\\sPackage\\sRequired\\s(\\d+)$").

%%============================================================================
%% Public Api
%%============================================================================
run_data(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    run_data_file(Device).

run_log(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    run_log_file(Device).

data1_test() ->
    ExpectedResult = versionify([{"app6","0.0.1"},
                                 {"dep_pkg13","0.0.2"},
                                 {"app13","0.0.1"},
                                 {"dep_pkg2","0.0.5"},
                                 {"dep_pkg1","0.0.2"},
                                 {"dep_pkg7","0.1.2"},
                                 {"app9","0.0.1"}]),
    ?assertMatch({ok, ExpectedResult},
                 run_data(fix_rebar_brokenness("data1.txt"))).

data2_test() ->
    ExpectedResult = versionify([{"app18","0.0.1"},
                                 {"app4","0.0.7"},
                                 {"app1","0.0.1"},
                                 {"app6","0.0.1"},
                                 {"dep_pkg13","0.0.2"},
                                 {"app13","0.0.1"},
                                 {"dep_pkg5","0.0.3"},
                                 {"dep_pkg1","0.0.2"},
                                 {"dep_pkg2","0.0.5"},
                                 {"dep_pkg7","0.1.2"},
                                 {"app9","0.0.1"},
                                 {"dep_pkg16","1.0.2"}]),
    ?assertMatch({ok, ExpectedResult},
                 run_data(fix_rebar_brokenness("data2.txt"))).

data3_test() ->
    ExpectedResult = versionify([{"app68","0.0.1"},
                                 {"app58","0.0.1"},
                                 {"app48","0.0.7"},
                                 {"app38","0.0.1"},
                                 {"app28","0.0.1"},
                                 {"app18","0.0.1"},
                                 {"app4","0.0.7"},
                                 {"app1","0.0.1"},
                                 {"app6","0.0.1"},
                                 {"dep_pkg13","0.0.2"},
                                 {"app13","0.0.1"},
                                 {"dep_pkg5","0.0.3"},
                                 {"dep_pkg1","0.0.2"},
                                 {"dep_pkg2","0.0.5"},
                                 {"dep_pkg7","0.1.2"},
                                 {"app9","0.0.1"},
                                 {"dep_pkg16","1.0.2"}]),
    ?assertMatch({ok,ExpectedResult}, run_data(fix_rebar_brokenness("data3.txt"))).

data4_test() ->
    ExpectedResult = versionify([{"dep_pkg20","0.0.2"},
                                 {"app78","0.0.1"},
                                 {"app68","0.0.1"},
                                 {"app58","0.0.1"},
                                 {"app48","0.0.7"},
                                 {"app38","0.0.1"},
                                 {"app28","0.0.1"},
                                 {"app18","0.0.1"},
                                 {"app4","0.0.7"},
                                 {"app1","0.0.1"},
                                 {"app6","0.0.1"},
                                 {"dep_pkg13","0.0.2"},
                                 {"app13","0.0.1"},
                                 {"dep_pkg5","0.0.3"},
                                 {"dep_pkg1","0.0.2"},
                                 {"dep_pkg2","0.0.5"},
                                 {"dep_pkg7","0.1.2"},
                                 {"app9","0.0.1"},
                                 {"dep_pkg16","1.0.2"}]),
    ?assertMatch({ok, ExpectedResult},
                 run_data(fix_rebar_brokenness("data4.txt"))).

data5_test() ->
    ExpectedResult = versionify([{"dep_pkg14","0.0.2"},
                                 {"dep_pkg22","0.0.2"},
                                 {"dep_pkg20","0.0.2"},
                                 {"app78","0.0.1"},
                                 {"app68","0.0.1"},
                                 {"app58","0.0.1"},
                                 {"app48","0.0.7"},
                                 {"app38","0.0.1"},
                                 {"app28","0.0.1"},
                                 {"app18","0.0.1"},
                                 {"app4","0.0.7"},
                                 {"app1","0.0.1"},
                                 {"app6","0.0.1"},
                                 {"dep_pkg13","0.0.2"},
                                 {"app13","0.0.1"},
                                 {"dep_pkg5","0.0.3"},
                                 {"dep_pkg1","0.0.2"},
                                 {"dep_pkg2","0.0.5"},
                                 {"dep_pkg7","0.1.2"},
                                 {"app9","0.0.1"},
                                 {"dep_pkg16","1.0.2"}]),
    ?assertMatch({ok, ExpectedResult},
                 run_data(fix_rebar_brokenness("data5.txt"))).

data6_test() ->
    ExpectedResult = versionify([{"app108","0.0.1"},
                                 {"app98","0.0.1"},
                                 {"app88","0.0.1"},
                                 {"dep_pkg14","0.0.2"},
                                 {"dep_pkg22","0.0.2"},
                                 {"dep_pkg20","0.0.2"},
                                 {"app78","0.0.1"},
                                 {"app68","0.0.1"},
                                 {"app58","0.0.1"},
                                 {"app48","0.0.7"},
                                 {"app38","0.0.1"},
                                 {"app28","0.0.1"},
                                 {"app18","0.0.1"},
                                 {"app4","0.0.7"},
                                 {"app1","0.0.1"},
                                 {"app6","0.0.1"},
                                 {"dep_pkg13","0.0.2"},
                                 {"app13","0.0.1"},
                                 {"dep_pkg5","0.0.3"},
                                 {"dep_pkg1","0.0.2"},
                                 {"dep_pkg2","0.0.5"},
                                 {"dep_pkg7","0.1.2"},
                                 {"app9","0.0.1"},
                                 {"dep_pkg16","1.0.2"}]),
    ?assertMatch({ok, ExpectedResult},
                 run_data(fix_rebar_brokenness("data6.txt"))).

log_07be9e47_test() ->
    Data = run_log(fix_rebar_brokenness("log-07be9e47-6f42-4a5d-b8b5-1d2eae1ad83b.txt")),
    ExpectedResult = versionify([{"0","0"},
                                  {"1","0"},
                                  {"3","0"},
                                  {"4","0"},
                                  {"5","0"},
                                  {"6","0"},
                                  {"7","0"},
                                  {"8","0"},
                                  {"9","0"},
                                  {"10","0"},
                                  {"11","0"},
                                  {"12","0"},
                                  {"13","0"},
                                  {"14","0"},
                                  {"15","0"},
                                  {"16","0"},
                                  {"18","0"},
                                  {"19","0"},
                                  {"21","0"},
                                  {"22","0"},
                                  {"23","0"},
                                  {"24","0"},
                                  {"25","0"}]),
    ?assertMatch({ok, ExpectedResult},
                 Data).

log_183998c1_test() ->
    ?assertMatch({error, {unreachable_package,<<"9">>}},
                 run_log(fix_rebar_brokenness("log-183998c1-2ada-4214-b308-e480345c42f2.txt"))).


log_311a15e7_test() ->
    {ok, Data} = run_log(fix_rebar_brokenness("log-311a15e7-3378-4c5b-beb7-86a1b9cf0ea9.txt")),
    ExpectedResult = lists:sort(versionify([{"45", "22"},
                                            {"40","1"},
                                            {"3","5"},
                                            {"9","0"},
                                            {"8","0"},
                                            {"7","0"},
                                            {"6","2"},
                                            {"1","5"},
                                            {"0","2"},
                                            {"61","1"},
                                            {"60","0"},
                                            {"35","4"},
                                            {"39","0"},
                                            {"38","2"},
                                            {"37","2"},
                                            {"36","3"},
                                            {"32","24"},
                                            {"30","0"},
                                            {"19","1"},
                                            {"18","0"},
                                            {"17","2"},
                                            {"16","0"},
                                            {"15","0"},
                                            {"14","1"},
                                            {"13","0"},
                                            {"12","1"},
                                            {"11","0"},
                                            {"10","1"},
                                            {"59","0"},
                                            {"58","1"},
                                            {"57","0"},
                                            {"56","0"},
                                            {"55","4"},
                                            {"29","2"},
                                            {"27","2"},
                                            {"26","0"},
                                            {"25","5"},
                                            {"24","3"},
                                            {"23","1"},
                                            {"22","3"},
                                            {"21","2"},
                                            {"20","0"}])),
    ?assertMatch(ExpectedResult, lists:sort(Data)).

log_382cfe5b_test() ->
    {ok, Data} =
        run_log(fix_rebar_brokenness("log-382cfe5b-0ac2-48b8-83d1-717cb4620990.txt")),
    ExpectedResult = lists:sort(versionify([{"18","0"},
                                            {"17","0"},
                                            {"15","1"},
                                            {"14","0"},
                                            {"10","0"},
                                            {"7","0"},
                                            {"6","0"},
                                            {"5","0"},
                                            {"4","0"},
                                            {"3","0"},
                                            {"2","1"},
                                            {"1","0"},
                                            {"0","0"}])),
    ?assertMatch(ExpectedResult, lists:sort(Data)).

log_d3564ef6_test() ->
    {ok, Data} = run_log(fix_rebar_brokenness("log-d3564ef6-6437-41e7-90b6-dbdb849551a6_mod.txt")),
    ExpectedResult = lists:sort(versionify([{"57","5"},
                                            {"56","3"},
                                            {"55","4"},
                                            {"54","0"},
                                            {"53","1"},
                                            {"82","0"},
                                            {"81","0"},
                                            {"80","1"},
                                            {"29","0"},
                                            {"28","5"},
                                            {"27","3"},
                                            {"26","1"},
                                            {"25","3"},
                                            {"24","2"},
                                            {"23","0"},
                                            {"22","1"},
                                            {"21","0"},
                                            {"20","2"},
                                            {"75","32"},
                                            {"79","2"},
                                            {"78","4"},
                                            {"74","7"},
                                            {"73","11"},
                                            {"72","0"},
                                            {"70","1"},
                                            {"47","4"},
                                            {"45","1"},
                                            {"44","1"},
                                            {"43","7"},
                                            {"42","1"},
                                            {"41","2"},
                                            {"40","2"},
                                            {"19","0"},
                                            {"18","0"},
                                            {"17","1"},
                                            {"16","0"},
                                            {"15","1"},
                                            {"14","0"},
                                            {"13","1"},
                                            {"12","0"},
                                            {"11","0"},
                                            {"10","0"},
                                            {"9","2"},
                                            {"4","5"},
                                            {"3","2"},
                                            {"0","3"},
                                            {"69","0"},
                                            {"68","1"},
                                            {"67","7"},
                                            {"39","3"},
                                            {"35","24"},
                                            {"33","0"},
                                            {"32","2"},
                                            {"30","2"}])),
    ?assertMatch(ExpectedResult, lists:sort(Data)).

log_ea2d264b_test() ->
    {ok, Data} = run_log(fix_rebar_brokenness("log-ea2d264b-003e-4611-94ed-14efc7732083.txt")),
    ExpectedResult = lists:sort(versionify([{"18","1"},
                                            {"17","0"},
                                            {"16","0"},
                                            {"15","0"},
                                            {"14","0"},
                                            {"13","1"},
                                            {"10","1"},
                                            {"9","1"},
                                            {"8","2"},
                                            {"6","0"},
                                            {"5","0"},
                                            {"4","0"},
                                            {"3","0"},
                                            {"2","0"},
                                            {"1","0"},
                                            {"0","1"}])),
    ?assertMatch(ExpectedResult, lists:sort(Data)).

%%============================================================================
%% Internal Functions
%%============================================================================
versionify(X) when erlang:is_list(X) ->
    lists:map(fun versionify/1, X);
versionify({K, V}) ->
    {erlang:list_to_binary(K), rlx_depsolver:parse_version(V)}.

fix_rebar_brokenness(Filename) ->
    Alt1 = filename:join(["./test", "data", Filename]),
    Alt2 = filename:join(["../test", "data", Filename]),
    case filelib:is_regular(Alt1) of
        true ->
            Alt1;
        false ->
            case filelib:is_regular(Alt2) of
                true ->
                    Alt2;
                false ->
                    erlang:throw(unable_to_find_data_files)
            end
    end.

run_data_file(Device) ->
    Constraints = get_constraints(io:get_line(Device, "")),
    rlx_depsolver:solve(process_packages(read_packages(Device)), Constraints).

goble_lines(_Device, eof, Acc) ->
    lists:reverse(Acc);
goble_lines(_Device, {error, Err}, _Acc) ->
    erlang:throw(Err);
goble_lines(Device, ValidVal, Acc) ->
    goble_lines(Device, io:get_line(Device, ""), [ValidVal | Acc]).

goble_lines(Device) ->
    goble_lines(Device, io:get_line(Device, ""), []).

run_log_file(Device) ->
    State0 = rlx_depsolver:new_graph(),
    {Goals, State2} =
        lists:foldl(fun(Line, Data) ->
                            process_add_goal(Line,
                                             process_add_constraint(Line,
                                                                    process_add_package(Line, Data)))
                    end, {[], State0}, goble_lines(Device)),
    rlx_depsolver:solve(State2, Goals).

read_packages(Device) ->
    process_line(Device, io:get_line(Device, ""), []).

process_line(Device, eof, Acc) ->
    file:close(Device),
    Acc;
process_line(Device, [], Acc) ->
    process_line(Device, io:get_line(Device, ""),
                 Acc);
process_line(Device, "\n", Acc) ->
    process_line(Device, io:get_line(Device, ""),
                 Acc);
process_line(Device, [$\s | Rest], [{Pkg, Vsn, Deps} | Acc]) ->
    [DepPackage, Type,  DepVsn] = rlx_string:lexemes(Rest, " \n"),
    Dep =
        case Type of
            "=" ->
                {DepPackage, DepVsn};
            ">=" ->
                {DepPackage, DepVsn, gte}
        end,
    process_line(Device, io:get_line(Device, ""),
                 [{Pkg, Vsn, [Dep | Deps]} | Acc]);
process_line(Device, Pkg, Acc) ->
    [Package, Vsn] = rlx_string:lexemes(Pkg, " \n"),
    process_line(Device, io:get_line(Device, ""),
                 [{Package, Vsn, []} | Acc]).

process_packages(Pkgs) ->
    lists:foldl(fun({Pkg, Vsn, Constraints}, Dom0) ->
                        rlx_depsolver:add_package_version(Dom0, Pkg, Vsn, Constraints)
                end, rlx_depsolver:new_graph(), Pkgs).

get_constraints(ConLine) ->
    AppVsns = rlx_string:lexemes(ConLine, " \n"),
    lists:map(fun(AppCon) ->
                      parse_app(AppCon, [])
              end, AppVsns).
parse_app([$= | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
parse_app([$>, $= | Rest], Acc) ->
    {lists:reverse(Acc), Rest, gte};
parse_app([Else | Rest], Acc) ->
    parse_app(Rest, [Else | Acc]);
parse_app([], Acc) ->
    lists:reverse(Acc).

process_add_package(Line, {Goals, State0}) ->
    case re:run(Line, ?ADD_PKG, [{capture, all, list}]) of
        {match, [_All, _InstNumber, PkgName, _PkgCount, VersionCount]} ->
            {Goals,
             lists:foldl(fun(PkgVsn, State1) ->
                                 rlx_depsolver:add_package_version(State1,
                                                               PkgName,
                                                               erlang:integer_to_list(PkgVsn),
                                                               [])
                         end, State0, lists:seq(0,
                                                erlang:list_to_integer(VersionCount)))};
        _ ->
            {Goals, State0}
    end.

process_add_constraint(Line, {Goals, State0}) ->
    case re:run(Line, ?ADD_VC, [{capture, all, list}]) of
        {match, [_All, _InstNumber, Pkg, Vsn, Dep, _Ignore, DepVsn]} ->
            {Goals,
             rlx_depsolver:add_package_version(State0, Pkg, Vsn, [{Dep, DepVsn}])};
        _ ->
            {Goals, State0}
    end.

process_add_goal(Line, {Goals, State0}) ->
    case re:run(Line, ?ADD_GOAL, [{capture, all, list}]) of
        {match,[_All, _InstNumber, NewGoal]} ->
            {[NewGoal | Goals], State0};
        _ ->
            {Goals, State0}
    end.
