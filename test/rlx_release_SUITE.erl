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
-module(rlx_release_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         make_release/1,
         make_scriptless_release/1,
         make_overridden_release/1,
         make_skip_app_release/1,
         make_rerun_overridden_release/1,
         make_implicit_config_release/1,
         overlay_release/1,
         make_goalless_release/1,
         make_depfree_release/1,
         make_invalid_config_release/1,
         make_relup_release/1,
         make_relup_release2/1,
         make_one_app_top_level_release/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    LibDir1 = filename:join([DataDir, create_random_name("lib_dir1_")]),
    ok = rlx_util:mkdir_p(LibDir1),
    State = rlx_state:new([{lib_dirs, [LibDir1]}], [release]),
    [{lib1, LibDir1},
     {state, State} | Config].

all() ->
    [make_release, make_scriptless_release, make_overridden_release,
     make_skip_app_release,
     make_implicit_config_release, make_rerun_overridden_release,
     overlay_release, make_goalless_release, make_depfree_release,
     make_invalid_config_release, make_relup_release, make_relup_release2,
     make_one_app_top_level_release].

make_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 2,
                              OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_invalid_config_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    ok = ec_file:write(ConfigFile,
                       "{release, {foo, \"0.0.1\"},
                         [goal_app_1,
                          goal_app_2,]}"),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    {error, {rlx_prv_config,
             {consult, _, _}}} = relx:do(undefined, undefined, [], [LibDir1], 2,
                                            OutputDir, ConfigFile).

make_scriptless_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    write_config(ConfigFile,
                 [{generate_start_script, false},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 2,
                              OutputDir, ConfigFile),

    ?assert(not ec_file:exists(filename:join([OutputDir, "bin", "foo"]))),
    ?assert(not ec_file:exists(filename:join([OutputDir, "bin", "foo-0.0.1"]))),

    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).


make_overridden_release(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OverrideDir1 = filename:join([DataDir, create_random_name("override_dir_")]),
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],
    OverrideApp = create_random_name("override_app"),
    OverrideVsn = create_random_vsn(),
    OverrideAppDir = filename:join(OverrideDir1, OverrideApp ++ "-" ++ OverrideVsn),
    OverrideAppName = erlang:list_to_atom(OverrideApp),

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    create_app(OverrideDir1, OverrideApp, OverrideVsn, [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    erlang:list_to_atom(OverrideApp),
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    {ok, Cwd} = file:get_cwd(),
    {ok, State} = relx:do(Cwd, undefined, undefined, [], [LibDir1], 2,
                             OutputDir, [{OverrideAppName, OverrideAppDir}],
                             ConfigFile),
    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({OverrideAppName, OverrideVsn}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),
    {ok, Real} = file:read_link(filename:join([OutputDir, "lib",
                                               OverrideApp ++ "-" ++ OverrideVsn])),
    ?assertMatch(OverrideAppDir, Real).

make_skip_app_release(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    SkipAppDir1 = filename:join([DataDir, create_random_name("skip_app_dir_")]),
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],
    SkipAppApp = create_random_name("skip_app_app"),
    SkipAppVsn = create_random_vsn(),
    SkipAppAppName = erlang:list_to_atom(SkipAppApp),

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    create_empty_app(SkipAppDir1, SkipAppApp, SkipAppVsn, [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {skip_apps, [erlang:list_to_atom(SkipAppApp)]}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    {ok, Cwd} = file:get_cwd(),
    {ok, State} = relx:do(Cwd, undefined, undefined, [], [LibDir1], 2,
                              OutputDir, [],
                             ConfigFile),
    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assertNot(lists:member({SkipAppAppName, SkipAppVsn}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_implicit_config_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    FooRoot = filename:join([LibDir1, "foodir1", "foodir2"]),
    filelib:ensure_dir(filename:join([FooRoot, "tmp"])),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    ok = file:set_cwd(FooRoot),
    {ok, FooRoot} = file:get_cwd(),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 2,
                             OutputDir, undefined),
    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
    ?assert(ec_file:exists(OutputDir)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_rerun_overridden_release(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OverrideDir1 = filename:join([DataDir, create_random_name("override_dir_")]),
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],
    OverrideApp = create_random_name("override_app"),
    OverrideVsn = create_random_vsn(),
    OverrideAppDir = filename:join(OverrideDir1, OverrideApp ++ "-"
                                  ++ OverrideVsn),
    OverrideAppName = erlang:list_to_atom(OverrideApp),

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    create_app(OverrideDir1, OverrideApp, OverrideVsn, [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    erlang:list_to_atom(OverrideApp),
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    {ok, Cwd} = file:get_cwd(),
    {ok, _} = relx:do(Cwd, undefined, undefined, [], [LibDir1], 2,
                             OutputDir, [{OverrideAppName, OverrideAppDir}],
                             ConfigFile),

    %% Now we run it again to see if it fails.
    {ok, State} = relx:do(Cwd,undefined, undefined, [], [LibDir1], 2,
                             OutputDir, [{OverrideAppName, OverrideAppDir}],
                             ConfigFile),

    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({OverrideAppName, OverrideVsn}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),
    {ok, Real} = file:read_link(filename:join([OutputDir, "lib",
                                               OverrideApp ++ "-" ++ OverrideVsn])),
    ?assertMatch(OverrideAppDir, Real).

overlay_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    OverlayVars = filename:join([LibDir1, "vars.config"]),
    Template = filename:join([LibDir1, "test_template"]),
    TestDir = "first_test_dir",
    TestFile = "test_file",
    TestDirFull = filename:join([LibDir1, TestDir]),
    TestFileFull = filename:join(TestDirFull, TestFile),
    SecondTestDir = "second_test_dir",
    write_config(ConfigFile,
                 [{overlay_vars, OverlayVars},
                  {overlay, [{mkdir, "{{target_dir}}/fooo"},
                             {copy, OverlayVars,
                              "{{target_dir}}/{{foo_dir}}/vars.config"},
                             {copy, OverlayVars,
                              "{{target_dir}}/{{yahoo}}/"},
                             {copy, TestDirFull,
                              "{{target_dir}}/"++SecondTestDir++"/"},
                             {template, Template,
                              "{{target_dir}}/test_template_resolved"}]},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),

    VarsFile = filename:join([LibDir1, "vars.config"]),
    write_config(VarsFile, [{yahoo, "yahoo"},
                            {yahoo2, [{foo, "bar"}]},
                            {yahoo3, [{bar, "{{yahoo}}/{{yahoo2.foo}}"}]},
                            {foo_dir, "foodir"}]),

    ok = rlx_util:mkdir_p(TestDirFull),
    ok = file:write_file(TestFileFull, test_template_contents()),

    TemplateFile = filename:join([LibDir1, "test_template"]),
    ok = file:write_file(TemplateFile, test_template_contents()),
    {ok, FileInfo} = file:read_file_info(TemplateFile),
    ok = file:write_file_info(TemplateFile, FileInfo#file_info{mode=8#00777}),

    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),

    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 2,
                              OutputDir, ConfigFile),

    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    ?assert(ec_file:exists(filename:join(OutputDir, "fooo"))),
    ?assert(ec_file:exists(filename:join([OutputDir, "foodir", "vars.config"]))),
    ?assert(ec_file:exists(filename:join([OutputDir, "yahoo", "vars.config"]))),
    io:format("DirFile ~p~n", [filename:join([OutputDir, SecondTestDir, TestDir, TestFile])]),
    ?assert(ec_file:exists(filename:join([OutputDir, SecondTestDir, TestDir, TestFile]))),

    TemplateData = case file:consult(filename:join([OutputDir, "test_template_resolved"])) of
                       {ok, Details} ->
                           Details;
                       Error ->
                           erlang:throw({failed_to_consult, Error})
                   end,
    {ok, ReadFileInfo} = file:read_file_info(filename:join([OutputDir, "test_template_resolved"])),
    ?assertEqual(8#100777, ReadFileInfo#file_info.mode),

    ?assertEqual(erlang:system_info(version),
                 proplists:get_value(erts_vsn, TemplateData)),
    ?assertEqual(erlang:system_info(version),
                 proplists:get_value(release_erts_version, TemplateData)),
    ?assertEqual("0.0.1",
                 proplists:get_value(release_version, TemplateData)),
    ?assertEqual(foo,
                 proplists:get_value(release_name, TemplateData)),
    ?assertEqual([kernel,stdlib,lib_dep_1,non_goal_2,non_goal_1,
                  goal_app_1,goal_app_2],
                 proplists:get_value(release_applications, TemplateData)),
    ?assert(proplists:is_defined(std_version, TemplateData)),
    ?assert(proplists:is_defined(kernel_version, TemplateData)),
    ?assertEqual("0.0.1",
                 proplists:get_value(non_goal_1_version, TemplateData)),
    ?assertEqual("0.0.1",
                 proplists:get_value(non_goal_2_version, TemplateData)),
    ?assertEqual("0.0.1",
                 proplists:get_value(goal_app_1_version, TemplateData)),
    ?assertEqual("0.0.1",
                 proplists:get_value(goal_app_2_version, TemplateData)),
    ?assertEqual("0.0.1",
                 proplists:get_value(lib_dep_1, TemplateData)),
    ?assert(proplists:is_defined(lib_dep_1_dir, TemplateData)),
    ?assertEqual([stdlib,kernel],
                 proplists:get_value(lib_dep_1_active, TemplateData)),
    ?assertEqual([],
                 proplists:get_value(lib_dep_1_library, TemplateData)),
    ?assertEqual("false",
                 proplists:get_value(lib_dep_1_link, TemplateData)),
    ?assertEqual("(2:debug)",
                 proplists:get_value(log, TemplateData)),
    ?assertEqual(OutputDir,
                 proplists:get_value(output_dir, TemplateData)),
    ?assertEqual(OutputDir,
                 proplists:get_value(target_dir, TemplateData)),
    ?assertEqual([],
                 proplists:get_value(overridden, TemplateData)),
    ?assertEqual([""],
                 proplists:get_value(goals, TemplateData)),
    ?assert(proplists:is_defined(lib_dirs, TemplateData)),
    ?assert(proplists:is_defined(config_file, TemplateData)),
    ?assertEqual([""],
                 proplists:get_value(goals, TemplateData)),
    ?assertEqual([],
                 proplists:get_value(sys_config, TemplateData)),
    ?assert(proplists:is_defined(root_dir, TemplateData)),
    ?assertEqual(foo,
                 proplists:get_value(default_release_name, TemplateData)),
    ?assertEqual("0.0.1",
                 proplists:get_value(default_release_version, TemplateData)),
    ?assertEqual("foo-0.0.1",
                 proplists:get_value(default_release, TemplateData)),
    ?assertEqual("yahoo",
                 proplists:get_value(yahoo, TemplateData)),
    ?assertEqual("bar",
                 proplists:get_value(yahoo2_foo, TemplateData)),
    ?assertEqual("foodir",
                 proplists:get_value(foo_dir, TemplateData)),
    ?assertEqual("yahoo/bar",
                 proplists:get_value(yahoo3, TemplateData)).

make_goalless_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   []}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    ?assertMatch({error,{rlx_prv_release,no_goals_specified}},
                 relx:do(undefined, undefined, [], [LibDir1], 2,
                            OutputDir, ConfigFile)).

make_depfree_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],

    create_app(LibDir1, "goal_app_1", "0.0.1", [kernel,stdlib], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [kernel,stdlib], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [kernel,stdlib], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [kernel,stdlib], []),
    create_app(LibDir1, "non_goal_2", "0.0.1", [kernel,stdlib], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1]}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 2,
                             OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)).

make_relup_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "goal_app_1", "0.0.2", [stdlib,kernel,non_goal_1], []),
    {ok, GA1} = create_app(LibDir1, "goal_app_1", "0.0.3", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "goal_app_2", "0.0.2", [stdlib,kernel,goal_app_1,non_goal_2], []),
    {ok, GA2} = create_app(LibDir1, "goal_app_2", "0.0.3", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    write_appup_file(GA1, "0.0.2"),
    write_appup_file(GA2, "0.0.2"),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [sasl,
                    {goal_app_1, "0.0.1"},
                    {goal_app_2, "0.0.1"}]},
                 {release, {foo, "0.0.2"},
                   [sasl,
                    {goal_app_1, "0.0.2"},
                    {goal_app_2, "0.0.2"}]},
                 {release, {foo, "0.0.3"},
                   [sasl,
                    {goal_app_1, "0.0.3"},
                    {goal_app_2, "0.0.3"}]}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    {ok, _} = relx:do(foo, "0.0.1", [], [LibDir1], 2,
                             OutputDir, ConfigFile),
    {ok, _} = relx:do(foo, "0.0.2", [], [LibDir1], 2,
                         OutputDir, ConfigFile),
    {ok, State} = relx:do([{relname, foo},
                              {relvsn, "0.0.3"},
                              {goals, []},
                              {lib_dirs, [LibDir1]},
                              {log_level, 2},
                              {output_dir, OutputDir},
                              {config, ConfigFile}], ["release", "relup"]),

    %% we should have one 'resolved' release and three discovered realized_releases.
    ?assertMatch([{foo, "0.0.1"},
                  {foo, "0.0.2"},
                  {foo, "0.0.3"}],
                 lists:sort(ec_dictionary:keys(rlx_state:realized_releases(State)))),
    Release = ec_dictionary:get({foo, "0.0.3"}, rlx_state:realized_releases(State)),
    ?assert(rlx_release:realized(Release)),
    ?assert(not rlx_release:realized(ec_dictionary:get({foo, "0.0.2"},
                                                       rlx_state:realized_releases(State)))),
    ?assert(not rlx_release:realized(ec_dictionary:get({foo, "0.0.1"},
                                                      rlx_state:realized_releases(State)))),

    ?assertMatch({ok, [{"0.0.3",
                        [{"0.0.2",[],[point_of_no_return]}],
                        [{"0.0.2",[],[point_of_no_return]}]}]},
                 file:consult(filename:join(filename:dirname(rlx_release:relfile(Release)), "relup"))),

    ?assertMatch(foo, rlx_release:name(Release)),
    ?assertMatch("0.0.3", rlx_release:vsn(Release)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.3"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.3"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).


make_relup_release2(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "goal_app_1", "0.0.2", [stdlib,kernel,non_goal_1], []),
    {ok, GA1} = create_app(LibDir1, "goal_app_1", "0.0.3", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "goal_app_2", "0.0.2", [stdlib,kernel,goal_app_1,non_goal_2], []),
    {ok, GA2} = create_app(LibDir1, "goal_app_2", "0.0.3", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    write_appup_file(GA1, "0.0.1"),
    write_appup_file(GA2, "0.0.1"),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [sasl,
                    {goal_app_1, "0.0.1"},
                    {goal_app_2, "0.0.1"}]},
                 {release, {foo, "0.0.2"},
                   [sasl,
                    {goal_app_1, "0.0.2"},
                    {goal_app_2, "0.0.2"}]},
                 {release, {foo, "0.0.3"},
                   [sasl,
                    {goal_app_1, "0.0.3"},
                    {goal_app_2, "0.0.3"}]}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relx-output")]),
    {ok, _} = relx:do(foo, "0.0.1", [], [LibDir1], 2,
                             OutputDir, ConfigFile),
    {ok, _} = relx:do(foo, "0.0.2", [], [LibDir1], 2,
                         OutputDir, ConfigFile),
    {ok, State} = relx:do([{relname, foo},
                              {relvsn, "0.0.3"},
                              {upfrom, "0.0.1"},
                              {goals, []},
                              {lib_dirs, [LibDir1]},
                              {log_level, 2},
                              {output_dir, OutputDir},
                              {config, ConfigFile}], ["release", "relup"]),

    %% we should have one 'resolved' release and three discovered realized_releases.
    ?assertMatch([{foo, "0.0.1"},
                  {foo, "0.0.2"},
                  {foo, "0.0.3"}],
                 lists:sort(ec_dictionary:keys(rlx_state:realized_releases(State)))),
    Release = ec_dictionary:get({foo, "0.0.3"}, rlx_state:realized_releases(State)),
    ?assert(rlx_release:realized(Release)),
    ?assert(not rlx_release:realized(ec_dictionary:get({foo, "0.0.2"},
                                                       rlx_state:realized_releases(State)))),
    ?assert(not rlx_release:realized(ec_dictionary:get({foo, "0.0.1"},
                                                      rlx_state:realized_releases(State)))),

    ?assertMatch({ok, [{"0.0.3",
                        [{"0.0.1",[],[point_of_no_return]}],
                        [{"0.0.1",[],[point_of_no_return]}]}]},
                 file:consult(filename:join(filename:dirname(rlx_release:relfile(Release)), "relup"))),

    ?assertMatch(foo, rlx_release:name(Release)),
    ?assertMatch("0.0.3", rlx_release:vsn(Release)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.3"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.3"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).


make_one_app_top_level_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    {ok, AppInfo} = create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel], []),
    AppDir = rlx_app_info:dir(AppInfo),
    ConfigFile = filename:join([AppDir, "relx.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [{goal_app_1, "0.0.1"}]}]),

    OutputDir = filename:join([AppDir,
                               create_random_name("relx-output")]),

    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd(AppDir),
    {ok, State} = relx:do(undefined, undefined, [], [], 2,
                              OutputDir, ConfigFile),
    ok = file:set_cwd(Cwd),
    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)).


%%%===================================================================
%%% Helper Functions
%%%===================================================================

create_app(Dir, Name, Vsn, Deps, LibDeps) ->
    AppDir = filename:join([Dir, Name ++ "-" ++ Vsn]),
    write_app_file(AppDir, Name, Vsn, Deps, LibDeps),
    write_beam_file(AppDir, Name),
    rlx_app_info:new(erlang:list_to_atom(Name), Vsn, AppDir,
                     Deps, []).

create_empty_app(Dir, Name, Vsn, Deps, LibDeps) ->
    AppDir = filename:join([Dir, Name ++ "-" ++ Vsn]),
    write_app_file(AppDir, Name, Vsn, Deps, LibDeps),
    rlx_app_info:new(erlang:list_to_atom(Name), Vsn, AppDir,
                     Deps, []).

write_beam_file(Dir, Name) ->
    Beam = filename:join([Dir, "ebin", "not_a_real_beam" ++ Name ++ ".beam"]),
    ok = filelib:ensure_dir(Beam),
    ok = ec_file:write_term(Beam, testing_purposes_only).

write_appup_file(AppInfo, DownVsn) ->
    Dir = rlx_app_info:dir(AppInfo),
    Name = rlx_util:to_string(rlx_app_info:name(AppInfo)),
    Vsn = rlx_app_info:vsn_as_string(AppInfo),
    Filename = filename:join([Dir, "ebin", Name ++ ".appup"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, {Vsn, [{DownVsn, []}], [{DownVsn, []}]}).

write_app_file(Dir, Name, Version, Deps, LibDeps) ->
    Filename = filename:join([Dir, "ebin", Name ++ ".app"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(Name, Version, Deps, LibDeps)).

get_app_metadata(Name, Vsn, Deps, LibDeps) ->
    {application, erlang:list_to_atom(Name),
     [{description, ""},
      {vsn, Vsn},
      {modules, []},
      {included_applications, LibDeps},
      {registered, []},
      {applications, Deps}]}.

create_random_name(Name) ->
    random:seed(erlang:now()),
    Name ++ erlang:integer_to_list(random:uniform(1000000)).

create_random_vsn() ->
    random:seed(erlang:now()),
    lists:flatten([erlang:integer_to_list(random:uniform(100)),
                   ".", erlang:integer_to_list(random:uniform(100)),
                   ".", erlang:integer_to_list(random:uniform(100))]).

write_config(Filename, Values) ->
    ok = ec_file:write(Filename,
                       [io_lib:format("~p.\n", [Val]) || Val <- Values]).

test_template_contents() ->
    "{erts_vsn, \"{{erts_vsn}}\"}.\n"
     "{release_erts_version, \"{{release_erts_version}}\"}.\n"
        "{release_name, {{release_name}}}.\n"
        "{rel_vsn, \"{{release_version}}\"}.\n"
        "{release_version, \"{{release_version}}\"}.\n"
        "{release_applications, [{{ release_applications|join:\", \" }}]}.\n"
        "{std_version, \"{{release.stdlib.version}}\"}.\n"
        "{kernel_version, \"{{release.kernel.version}}\"}.\n"
        "{non_goal_1_version, \"{{release.non_goal_1.version}}\"}.\n"
        "{non_goal_2_version, \"{{release.non_goal_2.version}}\"}.\n"
        "{goal_app_1_version, \"{{release.goal_app_1.version}}\"}.\n"
        "{goal_app_2_version, \"{{release.goal_app_2.version}}\"}.\n"
        "{lib_dep_1, \"{{release.lib_dep_1.version}}\"}.\n"
        "{lib_dep_1_dir, \"{{release.lib_dep_1.dir}}\"}.\n"
        "{lib_dep_1_active, [{{ release.lib_dep_1.active_dependencies|join:\", \" }}]}.\n"
        "{lib_dep_1_library, [{{ release.lib_dep_1.library_dependencies|join:\", \" }}]}.\n"
        "{lib_dep_1_link, \"{{release.lib_dep_1.link}}\"}.\n"
        "{log, \"{{log}}\"}.\n"
        "{output_dir, \"{{output_dir}}\"}.\n"
        "{target_dir, \"{{target_dir}}\"}.\n"
        "{overridden, [{{ overridden|join:\", \" }}]}.\n"
        "{goals, [\"{{ goals|join:\", \" }}\"]}.\n"
        "{lib_dirs, [\"{{ lib_dirs|join:\", \" }}\"]}.\n"
        "{config_file, \"{{ config_file }}\"}.\n"
        "{providers, [{{ providers|join:\", \" }}]}.\n"
        "{sys_config, \"{{sys_config}}\"}.\n"
        "{root_dir, \"{{root_dir}}\"}.\n"
        "{default_release_name, {{default_release_name}}}.\n"
        "{default_release_version, \"{{default_release_version}}\"}.\n"
        "{default_release, \"{{default_release}}\"}.\n"
        "{yahoo, \"{{yahoo}}\"}.\n"
        "{yahoo2_foo, \"{{yahoo2.foo}}\"}.\n"
        "{foo_dir, \"{{foo_dir}}\"}.\n"
        "{yahoo3, \"{{yahoo3.bar}}\"}.\n".
