%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 92 -*-
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
%%%-------------------------------------------------------------------
%%% @author Eric Merrit <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Eric Merrit
-module(rlx_command_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         all/0,
         normal_passing_case/1,
         lib_expansion_case/1,
         lib_fail_case/1,
         spec_parse_fail_case/1,
         config_fail_case/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [normal_passing_case, lib_expansion_case, lib_fail_case, config_fail_case].

normal_passing_case(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Lib1 = filename:join([DataDir, <<"lib1">>]),
    Lib2 = filename:join([DataDir, <<"lib2">>]),
    Outdir = filename:join([DataDir, "outdir"]),
    ok = rlx_util:mkdir_p(Lib1),
    ok = rlx_util:mkdir_p(Lib2),
    Goal1 = "app1<=33.33+build4",
    Goal2 = "app2:btwn:33.22,45.22+build.21",

    LogLevel = "2",
    RelName = "foo-release",
    RelVsn = "33.222",
    CmdLine = ["-V", LogLevel, "-g",Goal1,"-g",Goal2, "-l", Lib1, "-l", Lib2,
               "-n", RelName, "-v", RelVsn, "-o", Outdir, "-a", "lib1:"++binary_to_list(Lib1)],
    {ok, {Opts, Targets}} = getopt:parse(relx:opt_spec_list(), CmdLine),
    {ok, State} = rlx_cmd_args:args2state(Opts, Targets),
    ConfigProvider = rlx_provider:get_provider(config, rlx_state:providers(State)),
    {ok, State1} = rlx_provider:do(ConfigProvider, State),
    Overrides = rlx_state:overrides(State1),
    ?assertMatch([{lib1, Lib1}], Overrides),
    ?assertMatch([Lib1, Lib2],
                 rlx_state:lib_dirs(State1)),
    ?assertMatch(Outdir, rlx_state:base_output_dir(State1)),

    ?assertMatch([{app1,{{33,33},{[],[<<"build4">>]}},lte},
                  {app2,
                   {{33,22},{[],[]}},
                   {{45,22},{[],[<<"build">>,21]}}, between}],
                 rlx_state:goals(State1)).

lib_expansion_case(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Lib1 = filename:join(DataDir, <<"lib1">>),
    Lib2 = filename:join(DataDir, <<"lib2">>),
    ok = rlx_util:mkdir_p(Lib1),
    ok = rlx_util:mkdir_p(Lib2),

    CmdLine = ["-l", filename:join(DataDir, "*")],
    {ok, {Opts, Targets}} = getopt:parse(relx:opt_spec_list(), CmdLine),
    {ok, State} = rlx_cmd_args:args2state(Opts, Targets),
    ConfigProvider = rlx_provider:get_provider(config, rlx_state:providers(State)),
    {ok, State1} = rlx_provider:do(ConfigProvider, State),
    ?assertMatch([Lib1, Lib2],
                 rlx_state:lib_dirs(State1)).

lib_fail_case(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Lib1 = filename:join([DataDir, "lib1"]),
    Lib2 = filename:join([DataDir, "lib3333"]),
    ok = rlx_util:mkdir_p(Lib1),

    CmdLine = ["-l", Lib1, "-l", Lib2],
    {ok, {Opts, Targets}} = getopt:parse(relx:opt_spec_list(), CmdLine),
    ?assertMatch({error, {_, {not_directory, Lib2}}},
                 rlx_cmd_args:args2state(Opts, Targets)).

spec_parse_fail_case(_Config) ->
    Spec = "aaeu:3333:33.22a44",
    CmdLine = ["-g", Spec],
    {ok, {Opts, Targets}} = getopt:parse(relx:opt_spec_list(), CmdLine),
    ?assertMatch({error, {_, {failed_to_parse, _Spec}}},
                 rlx_cmd_args:args2state(Opts, Targets)).

config_fail_case(_Config) ->
    ConfigFile = "does-not-exist",
    CmdLine = ["-c", ConfigFile],
    {ok, {Opts, Targets}} = getopt:parse(relx:opt_spec_list(), CmdLine),
    ?assertMatch({error, {_, {invalid_config_file, ConfigFile}}},
                 rlx_cmd_args:args2state(Opts, Targets)).
