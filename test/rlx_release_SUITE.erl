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

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    DataDir = filename:join(proplists:get_value(priv_dir, Config), ?MODULE),
    LibDir1 = filename:join([DataDir, rlx_test_utils:create_random_name("lib_dir1_")]),
    ok = rlx_util:mkdir_p(LibDir1),
    [{lib1, LibDir1} | Config].

all() ->
    [make_config_release, make_dev_mode_template_release, overlay_release].

%% all() ->
%%     [make_release, make_extend_release, make_extend_config_release,
%%      make_extend_release_versioned,
%%      make_scriptless_release, make_overridden_release, make_auto_skip_empty_app_release,
%%      make_skip_app_release, make_exclude_app_release, make_app_type_none_release,
%%      make_implicit_config_release, make_rerun_overridden_release, overlay_release,
%%      make_goalless_release, make_depfree_release, make_invalid_config_release,
%%      make_relup_release, make_relup_release2, make_one_app_top_level_release,
%%      make_dev_mode_release, make_dev_mode_template_release, make_config_script_release,
%%      make_release_twice, make_release_twice_dev_mode, make_erts_release,
%%      make_erts_config_release, make_included_nodetool_release,
%%      make_not_included_nodetool_release, make_src_release, make_excluded_src_release,
%%      make_exclude_modules_release, make_release_with_sys_config_vm_args_src].

make_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    [(fun({Name, Vsn}) ->
              rlx_test_utils:create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
    ||
        App <-
            [{rlx_test_utils:create_random_name("lib_app1_"), rlx_test_utils:create_random_vsn()}
            || _ <- lists:seq(1, 100)]],

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {release, {foo, "0.0.2"},
                     [goal_app_1,
                      goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                              OutputDir, ConfigFile),
    [{{foo, "0.0.2"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_config_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.2", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2],
                   [{some, config1}]},
                  {release, {foo, "0.0.2"},
                   [{goal_app_1, "0.0.2"},
                    goal_app_2],
                   [{some, config2}]},
                  {lib_dirs, [LibDir1]},
                  {default_release, {foo, "0.0.2"}}],

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    %% TODO: this needs to go
    Dirs = [list_to_binary(Dir) || Dir <- rlx_util:wildcard_paths([LibDir1 | code:get_path()])],
    Apps = rlx_test_utils:resolve_app_metadata(Dirs),

    {ok, State} = relx:build_release(#{}, Apps, [{root_dir, LibDir1}, {output_dir, OutputDir} | RelxConfig]),


    [{{foo, "0.0.2"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.2"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),
    ?assertEqual([{some, config2}], rlx_release:config(Release)).

make_extend_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {release, {foo_test, "0.0.1", {extend, foo}},
                  [goal_app_2]},
                  {lib_dirs, [filename:join(LibDir1, "*")]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    ?assertMatch({error, {rlx_prv_release, {multiple_release_names,foo,foo_test}}},
                 catch relx:do(undefined, undefined, [], [LibDir1], 3, OutputDir, ConfigFile)),

    {ok, State} = relx:do(foo_test, undefined, [], [LibDir1], 3,
                              OutputDir, ConfigFile),
    [{{foo_test, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_extend_release_versioned(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.2"},
                   [goal_app_1,
                    goal_app_2]},
                  {release, {foo_test, "0.0.3", {extend, {foo, "0.0.2"}}},
                  [goal_app_2]},
                  {lib_dirs, [filename:join(LibDir1, "*")]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    ?assertMatch({error, {rlx_prv_release, {multiple_release_names,foo,foo_test}}},
                 catch relx:do(undefined, undefined, [], [LibDir1], 3, OutputDir, ConfigFile)),

    {ok, State} = relx:do(foo_test, undefined, [], [LibDir1], 3,
                              OutputDir, ConfigFile),
    [{{foo_test, "0.0.3"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_extend_config_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {release, {foo_test, "0.0.1", {extend, foo}},
                    [goal_app_2],
                    [{some, config}]},
                  {lib_dirs, [filename:join(LibDir1, "*")]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    ?assertMatch({error, {rlx_prv_release, {multiple_release_names,foo,foo_test}}},
                 catch relx:do(undefined, undefined, [], [LibDir1], 3, OutputDir, ConfigFile)),

    {ok, State} = relx:do(foo_test, undefined, [], [LibDir1], 3,
                              OutputDir, ConfigFile),
    [{{foo_test, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),
    ?assertEqual([{some, config}], rlx_release:config(Release)).

make_invalid_config_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    ok = ec_file:write(ConfigFile,
                       "{release, {foo, \"0.0.1\"},
                         [goal_app_1,
                          goal_app_2,]}"),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {error, {rlx_config,
             {consult, _, _}}} = relx:do(undefined, undefined, [], [LibDir1], 3,
                                            OutputDir, ConfigFile).

make_scriptless_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{generate_start_script, false},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                              OutputDir, ConfigFile),

    ?assert(not ec_file:exists(filename:join([OutputDir, "bin", "foo"]))),
    ?assert(not ec_file:exists(filename:join([OutputDir, "bin", "foo-0.0.1"]))),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_overridden_release(Config) ->
    DataDir = proplists:get_value(priv_dir, Config),
    OverrideDir1 = filename:join([DataDir, rlx_test_utils:create_random_name("override_dir_")]),
    LibDir1 = proplists:get_value(lib1, Config),

    OverrideApp = rlx_test_utils:create_random_name("override_app"),
    OverrideVsn = rlx_test_utils:create_random_vsn(),
    OverrideAppDir = filename:join(OverrideDir1, OverrideApp ++ "-" ++ OverrideVsn),
    OverrideAppName = erlang:list_to_atom(OverrideApp),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    rlx_test_utils:create_app(OverrideDir1, OverrideApp, OverrideVsn, [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    erlang:list_to_atom(OverrideApp),
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, Cwd} = file:get_cwd(),
    {ok, State} = relx:do(Cwd, undefined, undefined, [], [LibDir1], 3,
                             OutputDir, [{OverrideAppName, OverrideAppDir}],
                             ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({OverrideAppName, OverrideVsn}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),
    case os:type() of
        {win32, _} ->
            filelib:is_file(filename:join([OutputDir, "foo", "lib",
                                               OverrideApp ++ "-" ++ OverrideVsn]));
        _ ->
            {ok, Real} = file:read_link(filename:join([OutputDir, "foo", "lib",
                                                       OverrideApp ++ "-" ++ OverrideVsn])),
            ?assertMatch(OverrideAppDir, Real)
    end.

make_skip_app_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1]},
                  {skip_apps, [goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, Cwd} = file:get_cwd(),
    {ok, State} = relx:do(Cwd, undefined, undefined, [], [LibDir1], 3,
                              OutputDir, [],
                             ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assertNot(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assertNot(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

%% Test to ensure that an excluded app and its deps are not included in a release
make_exclude_app_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel, non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel, non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1]},
                  {exclude_apps, [non_goal_1]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, Cwd} = file:get_cwd(),
    {ok, State} = relx:do(Cwd, undefined, undefined, [], [LibDir1], 3,
                          OutputDir, [],
                          ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assertNot(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assertNot(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)).

make_auto_skip_empty_app_release(Config) ->
    DataDir = proplists:get_value(priv_dir, Config),
    EmptyAppDir1 = filename:join([DataDir, rlx_test_utils:create_random_name("skip_app_dir_")]),
    LibDir1 = proplists:get_value(lib1, Config),

    EmptyAppApp = rlx_test_utils:create_random_name("empty_app_app"),
    EmptyAppVsn = rlx_test_utils:create_random_vsn(),
    EmptyAppAppName = erlang:list_to_atom(EmptyAppApp),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    rlx_test_utils:create_empty_app(EmptyAppDir1, EmptyAppApp, EmptyAppVsn, [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, Cwd} = file:get_cwd(),
    {ok, State} = relx:do(Cwd, undefined, undefined, [], [LibDir1], 3,
                              OutputDir, [],
                             ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assertNot(lists:member({EmptyAppAppName, EmptyAppVsn}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_app_type_none_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    {goal_app_2, none}]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, Cwd} = file:get_cwd(),
    {ok, State} = relx:do(Cwd, undefined, undefined, [], [LibDir1], 3,
                              OutputDir, [],
                             ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1", none}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_implicit_config_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    FooRoot = filename:join([LibDir1, "foodir1", "foodir2"]),
    filelib:ensure_dir(filename:join([FooRoot, "tmp"])),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    ok = file:set_cwd(FooRoot),
    {ok, FooRoot} = file:get_cwd(),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                             OutputDir, undefined),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
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
    DataDir = proplists:get_value(priv_dir, Config),
    OverrideDir1 = filename:join([DataDir, rlx_test_utils:create_random_name("override_dir_")]),
    LibDir1 = proplists:get_value(lib1, Config),

    OverrideApp = rlx_test_utils:create_random_name("override_app"),
    OverrideVsn = rlx_test_utils:create_random_vsn(),
    OverrideAppDir = filename:join(OverrideDir1, OverrideApp ++ "-"
                                  ++ OverrideVsn),
    OverrideAppName = erlang:list_to_atom(OverrideApp),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    rlx_test_utils:create_app(OverrideDir1, OverrideApp, OverrideVsn, [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    OverlayVars = filename:join([LibDir1, "vars1.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    erlang:list_to_atom(OverrideApp),
                    goal_app_2]},
                  {overlay_vars, [OverlayVars]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, Cwd} = file:get_cwd(),
    {ok, _} = relx:do(Cwd, undefined, undefined, [], [LibDir1], 3,
                             OutputDir, [{OverrideAppName, OverrideAppDir}],
                             ConfigFile),

    %% Now we run it again to see if it fails.
    {ok, State} = relx:do(Cwd,undefined, undefined, [], [LibDir1], 3,
                             OutputDir, [{OverrideAppName, OverrideAppDir}],
                             ConfigFile),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({OverrideAppName, OverrideVsn}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),
    case os:type() of
        {win32, _} ->
            filelib:is_file(filename:join([OutputDir, "foo", "lib",
                                               OverrideApp ++ "-" ++ OverrideVsn]));
        _ ->
            {ok, Real} = file:read_link(filename:join([OutputDir, "foo", "lib",
                                                       OverrideApp ++ "-" ++ OverrideVsn])),
            ?assertMatch(OverrideAppDir, Real)
    end.

overlay_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    OverlayVars1 = filename:join([LibDir1, "vars1.config"]),
    OverlayVars2 = filename:join([LibDir1, "vars2.config"]),
    OverlayVars3 = filename:join([LibDir1, "vars3.config"]),
    OverlayVars4 = filename:join([LibDir1, "vars4.config"]),
    Template = filename:join([LibDir1, "test_template"]),
    TestDir = "first_test_dir",
    TestFile = "test_file",
    TestDirFull = filename:join([LibDir1, TestDir]),
    TestFileFull = filename:join(TestDirFull, TestFile),
    SecondTestDir = "second_test_dir",
    RelxConfig = [{overlay_vars, [{var_list_dir, "non-file-variable-list"},
                                  OverlayVars1, OverlayVars2, OverlayVars4]},
                  {overlay, [{mkdir, "{{target_dir}}/fooo"},
                             {mkdir, "{{target_dir}}/{{var_list_dir}}"},
                             {copy, OverlayVars1,
                              "{{target_dir}}/{{foo_dir}}/vars1.config"},
                             {copy, filename:join([LibDir1, "vars1*.config"]),
                              "{{target_dir}}/{{yahoo}}/"},
                             {link, OverlayVars4,
                              "{{target_dir}}/{{yahoo}}/vars4.config"},
                             {copy, TestDirFull,
                              "{{target_dir}}/"++SecondTestDir++"/"},
                             {template, Template,
                              "{{target_dir}}/test_template_resolved"},
                             {template, Template,
                              "bin/{{default_release_name}}-{{default_release_version}}"},
                             {copy, "{{erts_dir}}/bin/erl", "bin/copy.erl"}]},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}],

    VarsFile1 = filename:join([LibDir1, "vars1.config"]),
    %% `tpl_var' is defined in vars1, but redifined in vars2 using template.
    %% `api_caller_var' is to be injected as an API caller overlay var.
    rlx_test_utils:write_config(VarsFile1, [{yahoo, "yahoo"},
                                            {yahoo2, [{foo, "bar"}]},
                                            {foo_yahoo, "foo_{{yahoo}}"},
                                            {foo_dir, "foodir"},
                                            {tpl_var, "defined in vars1"},
                                            {api_caller_var, "{{api_caller_var}}"}]),

    VarsFile2 = filename:join([LibDir1, "vars2.config"]),
    rlx_test_utils:write_config(VarsFile2, [{google, "yahoo"},
                                            {yahoo2, "foo"},
                                            {tpl_arg, "a template value"},
                                            {tpl_var, "Redefined in vars2 with {{tpl_arg}}"},
                                            OverlayVars3]),

    VarsFile3 = filename:join([LibDir1, "vars3.config"]),
    rlx_test_utils:write_config(VarsFile3, [{google, "yahoo"},
                                            {yahoo4, "{{yahoo}}/{{yahoo2}}4"}]),

    VarsFile4 = filename:join([LibDir1, "vars4.config"]),
    rlx_test_utils:write_config(VarsFile4, [{prop1, "val1"},
                                            {prop2, 2}]),

    ok = rlx_util:mkdir_p(TestDirFull),
    ok = file:write_file(TestFileFull, rlx_test_utils:test_template_contents()),

    TemplateFile = filename:join([LibDir1, "test_template"]),
    ok = file:write_file(TemplateFile, rlx_test_utils:test_template_contents()),
    {ok, FileInfo} = file:read_file_info(TemplateFile),
    ok = file:write_file_info(TemplateFile, FileInfo#file_info{mode=8#00777}),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    ApiCallerVarValue = "api-caller-var",
    ApiCallerReleaseNameValue = "release-var-conflict",
    ApiCallerConfigFileValue = "state-var-conflict",
    ApiCallerYahooValue = "overlays-file-conflict",
    ApiCallerOverlays =
        [{api_caller_var, ApiCallerVarValue},
         {release_name, ApiCallerReleaseNameValue},
         {yahoo, ApiCallerYahooValue}],

    Dirs = [list_to_binary(Dir) || Dir <- rlx_util:wildcard_paths([LibDir1 | code:get_path()])],
    Apps = rlx_test_utils:resolve_app_metadata(Dirs),

    {ok, State} = relx:build_release(#{}, Apps, [{root_dir, LibDir1},
                                                 {overlay_vars_values, ApiCallerOverlays},
                                                 {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "non-file-variable-list"]))),
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "fooo"]))),
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "foodir", "vars1.config"]))),
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "yahoo", "vars1.config"]))),
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", SecondTestDir, TestDir, TestFile]))),
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "bin", "copy.erl"]))),

    TemplateData = case file:consult(filename:join([OutputDir, "foo", "test_template_resolved"])) of
                       {ok, Details} ->
                           Details;
                       Error ->
                           erlang:throw({failed_to_consult, Error})
                   end,

    {ok, ReadFileInfo} = file:read_file_info(filename:join([OutputDir, "foo", "test_template_resolved"])),
    ?assertEqual(8#100777, ReadFileInfo#file_info.mode),

    ?assertEqual(erlang:system_info(version),
                 proplists:get_value(erts_vsn, TemplateData)),
    ?assertEqual(erlang:system_info(version),
                 proplists:get_value(release_erts_version, TemplateData)),
    ?assertEqual("0.0.1",
                 proplists:get_value(release_version, TemplateData)),
    ?assertEqual(foo,
                 proplists:get_value(release_name, TemplateData)),
    ?assertEqual("yahoo/foo4",
                 proplists:get_value(yahoo4, TemplateData)),
    ?assertEqual("foo_yahoo",
                 proplists:get_value(foo_yahoo, TemplateData)),
    ?assertEqual("yahoo",
                 proplists:get_value(google, TemplateData)),
    ?assertEqual("val1",
                 proplists:get_value(prop1, TemplateData)),
    ?assertEqual(2,
                 proplists:get_value(prop2, TemplateData)),
    %% This should be rendered correctly based on VarsFile2 file, regardless
    %% of tpl_var defined in VarsFile1 or not.
    ?assertEqual("Redefined in vars2 with a template value",
                 proplists:get_value(tpl_var, TemplateData)),

    ?assertEqual(ApiCallerVarValue,
                 proplists:get_value(api_caller_var, TemplateData)),
    % The following complements a few of the assertions above
    % by ensuring that the values of the overwritten API caller
    % overlays were distinct from their internal values.
    ?assertNotEqual(ApiCallerReleaseNameValue,
                    proplists:get_value(release_name, TemplateData)),
    ?assertNotEqual(ApiCallerConfigFileValue,
                    proplists:get_value(config_file, TemplateData)),
    ?assertNotEqual(ApiCallerYahooValue,
                    proplists:get_value(yahoo, TemplateData)).

make_goalless_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   []}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    ?assertMatch({error,{rlx_prv_release,no_goals_specified}},
                 relx:do(undefined, undefined, [], [LibDir1], 3,
                            OutputDir, ConfigFile)).

make_depfree_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [kernel,stdlib], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                             OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)).

make_one_app_top_level_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    {ok, AppInfo} = rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel], []),
    AppDir = rlx_app_info:dir(AppInfo),
    ConfigFile = filename:join([AppDir, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [{goal_app_1, "0.0.1"}]}]),

    OutputDir = filename:join([AppDir,
                               rlx_test_utils:create_random_name("relx-output")]),

    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd(AppDir),
    {ok, State} = relx:do(undefined, undefined, [], [], 3,
                              OutputDir, ConfigFile),
    ok = file:set_cwd(Cwd),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)).

make_dev_mode_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    SysConfig = filename:join([LibDir1, "config", "sys.config"]),
    rlx_test_utils:write_config(SysConfig, [{this_is_a_test, "yup it is"}]),

    VmArgs = filename:join([LibDir1, "config", "vm.args"]),
    ec_file:write(VmArgs, ""),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{dev_mode, true},
                  {sys_config, SysConfig},
                  {vm_args, VmArgs},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                          OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, _Release}] = maps:to_list(rlx_state:realized_releases(State)),


    case os:type() of
        {unix, _} ->
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_2-0.0.1"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_1-0.0.1"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_2-0.0.1"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "lib_dep_1-0.0.1"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                      "sys.config"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                      "vm.args"])));
        {win32, _} ->
            ?assert(filelib:is_dir(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]))),
            ?assert(filelib:is_dir(filename:join([OutputDir, "foo", "lib", "non_goal_2-0.0.1"]))),
            ?assert(filelib:is_dir(filename:join([OutputDir, "foo", "lib", "goal_app_1-0.0.1"]))),
            ?assert(filelib:is_dir(filename:join([OutputDir, "foo", "lib", "goal_app_2-0.0.1"]))),
            ?assert(filelib:is_dir(filename:join([OutputDir, "foo", "lib", "lib_dep_1-0.0.1"]))),
            ?assert(filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                      "sys.config"]))),
            ?assert(filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                      "vm.args"])))
    end.

make_dev_mode_template_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1",
                              [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1",
                              [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1",
                              [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1",
                              [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1",
                              [stdlib,kernel], []),

    SysConfig = filename:join([LibDir1, "config", "sys.config"]),
    SysConfigTerm = [{this_is_a_test, "yup it is"},
                     {this_is_an_overlay_var, "{{var1}}"}],
    rlx_test_utils:write_config(SysConfig, SysConfigTerm),

    VmArgs = filename:join([LibDir1, "config", "vm.args"]),
    ec_file:write(VmArgs, "-sname {{nodename}}"),

    VarsFile1 = filename:join([LibDir1, "config", "vars1.config"]),
    rlx_test_utils:write_config(VarsFile1, [{var1, "indeed it is"},
                                            {nodename, "testnode"}]),

    RelxConfig = [{dev_mode, true},
                  {sys_config, SysConfig},
                  {vm_args, VmArgs},
                  {overlay_vars, [VarsFile1]},
                  {overlay, [
                             {template, "config/sys.config",
                              "releases/{{release_version}}/sys.config"},
                             {template, "config/vm.args",
                              "releases/{{release_version}}/vm.args"}]},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}],
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    %% TODO: this needs to go
    Dirs = [list_to_binary(Dir) || Dir <- rlx_util:wildcard_paths([LibDir1 | code:get_path()])],
    Apps = rlx_test_utils:resolve_app_metadata(Dirs),

    {ok, State} = relx:build_release(#{}, Apps, [{root_dir, LibDir1}, {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, _Release}] = maps:to_list(rlx_state:realized_releases(State)),

    ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]))),
    ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_2-0.0.1"]))),
    ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_1-0.0.1"]))),
    ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_2-0.0.1"]))),
    ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "lib_dep_1-0.0.1"]))),
    ?assert(not ec_file:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                  "sys.config"]))),
    ?assert(not ec_file:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                  "vm.args"]))),
    %% ensure that the original sys.config didn't get overwritten
    ?assertMatch({ok, SysConfigTerm}, file:consult(SysConfig)),
    %% ensure that the original vm.args didn't get overwritten
    ?assertMatch({ok, <<"-sname {{nodename}}">>}, ec_file:read(VmArgs)).

make_config_script_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    FooRoot = filename:join([LibDir1, "foodir1", "foodir2"]),
    filelib:ensure_dir(filename:join([FooRoot, "tmp"])),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    ConfigScriptFile = filename:join([LibDir1, "relx.config.script"]),
    ok = file:write_file(ConfigScriptFile,
            "case os:getenv(\"RELX_TEST\") of\n"
            "   \"true\" ->\n"
            "       {release, {RelName, Version}, Apps} = lists:keyfind(release, 1, CONFIG),\n"
            "       lists:keyreplace(release, 1, CONFIG, {release, {RelName, \"0.0.2\"}, Apps});\n"
            "    _ -> CONFIG % env var not defined or anything other than true\n"
            "end.\n"),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    ok = file:set_cwd(FooRoot),
    {ok, FooRoot} = file:get_cwd(),

    % set the env var that will cause relx.config to be altered by the config script
    os:putenv("RELX_TEST", "true"),

    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                             OutputDir, undefined),
    [{{foo, "0.0.2"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    ?assert(ec_file:exists(OutputDir)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_release_twice(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    [(fun({Name, Vsn}) ->
              rlx_test_utils:create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
    ||
        App <-
            [{rlx_test_utils:create_random_name("lib_app1_"), rlx_test_utils:create_random_vsn()}
            || _ <- lists:seq(1, 100)]],

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                          OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    ec_file:remove(filename:join(LibDir1, "non_goal_1-0.0.1"), [recursive]),
    ec_file:remove(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]), [recursive]),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.3", [stdlib,kernel], [lib_dep_1]),

    {ok, State1} = relx:do(undefined, undefined, [], [LibDir1], 3,
                              OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release1}] = maps:to_list(rlx_state:realized_releases(State1)),
    AppSpecs1 = rlx_release:applications(Release1),
    ?assert(lists:keymember(stdlib, 1, AppSpecs1)),
    ?assert(lists:keymember(kernel, 1, AppSpecs1)),
    ?assert(lists:member({non_goal_1, "0.0.3"}, AppSpecs1)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs1)).

make_release_twice_dev_mode(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    [(fun({Name, Vsn}) ->
              rlx_test_utils:create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
    ||
        App <-
            [{rlx_test_utils:create_random_name("lib_app1_"), rlx_test_utils:create_random_vsn()}
            || _ <- lists:seq(1, 100)]],

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}, {dev_mode, true}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                          OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    ec_file:remove(filename:join(LibDir1, "non_goal_1-0.0.1"), [recursive]),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.3", [stdlib,kernel], [lib_dep_1]),

    {ok, State1} = relx:do(undefined, undefined, [], [LibDir1], 3,
                              OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release1}] = maps:to_list(rlx_state:realized_releases(State1)),
    AppSpecs1 = rlx_release:applications(Release1),
    ?assert(lists:keymember(stdlib, 1, AppSpecs1)),
    ?assert(lists:keymember(kernel, 1, AppSpecs1)),
    ?assert(lists:member({non_goal_1, "0.0.3"}, AppSpecs1)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs1)).

make_erts_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [kernel,stdlib], []),

    ErtsVsn = erlang:system_info(version),
    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"}, {erts, ErtsVsn},
                   [goal_app_1]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                             OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assertEqual(ErtsVsn, rlx_release:erts(Release)).

make_erts_config_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [kernel,stdlib], []),

    ErtsVsn = erlang:system_info(version),
    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"}, {erts, ErtsVsn},
                   [goal_app_1],
                   [{some, config}]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                             OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assertEqual(ErtsVsn, rlx_release:erts(Release)),
    ?assertEqual([{some, config}], rlx_release:config(Release)).

make_included_nodetool_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [kernel,stdlib], []),

    ErtsVsn = erlang:system_info(version),
    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"}, {erts, ErtsVsn},
                   [goal_app_1]},
                  {extended_start_script, true},
                  {include_nodetool, true}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                             OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "bin", "nodetool"]))),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assertEqual(ErtsVsn, rlx_release:erts(Release)).

make_not_included_nodetool_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [kernel,stdlib], []),

    ErtsVsn = erlang:system_info(version),
    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"}, {erts, ErtsVsn},
                   [goal_app_1]},
                  {extended_start_script, true},
                  {include_nodetool, false}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                             OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    %% extended start script needs nodetool to work, so the
    %% {include_nodetool, false} option is simply ignored
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "bin", "nodetool"]))),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assertEqual(ErtsVsn, rlx_release:erts(Release)).

make_src_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [kernel,stdlib], []),

    ErtsVsn = erlang:system_info(version),
    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1]},
                  {extended_start_script, true},
                  {include_erts, true},
                  {include_src, true}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                             OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "erts-"++ErtsVsn, "src"]))),
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "lib",
                                          "goal_app_1-0.0.1", "src"]))).

make_excluded_src_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [kernel,stdlib], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [kernel,stdlib], []),

    ErtsVsn = erlang:system_info(version),
    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1]},
                  {extended_start_script, true},
                  {include_erts, true},
                  {include_src, false}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                             OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(not ec_file:exists(filename:join([OutputDir, "foo", "erts-"++ErtsVsn, "src"]))),
    ?assert(not ec_file:exists(filename:join([OutputDir, "foo", "lib",
                                              "goal_app_1-0.0.1", "src"]))).

%% Test to ensure that excluded modules don't end up in the release 
make_exclude_modules_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel, non_goal_1], []),
    {ok, NonGoalAppInfo} = rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], []),

    NonGoalAppDir = rlx_app_info:dir(NonGoalAppInfo),
    FooFilename = filename:join([NonGoalAppDir, "priv/subdir/foo.txt"]),
    rlx_test_utils:write_config(FooFilename, []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1]},
                  {exclude_modules, [{non_goal_1, [a_real_beamnon_goal_1]}]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, Cwd} = file:get_cwd(),
    {ok, State} = relx:do(Cwd, undefined, undefined, [], [LibDir1], 3,
                          OutputDir, [],
                          ConfigFile),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    %% ensure that the excluded module beam file didn't get copied
    ?assert(not ec_file:exists(filename:join([OutputDir, "foo", "lib",
                                              "non_goal_1-0.0.1", "ebin",
                                              "a_real_beamnon_goal_1.beam"]))),

    ?assertMatch({ok, [{application,non_goal_1,
                        [{description,[]},
                         {vsn,"0.0.1"},
                         {modules,[]},
                         {included_applications,[]},
                         {registered,[]},
                         {applications,[stdlib,kernel]}]}]},
                 file:consult(filename:join([OutputDir, "foo", "lib",
                                             "non_goal_1-0.0.1", "ebin",
                                             "non_goal_1.app"]))).

make_release_with_sys_config_vm_args_src(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    %% the .src versions should take precedence and the others are not copied
    SysConfig = filename:join([LibDir1, "config", "sys.config"]),
    rlx_test_utils:write_config(SysConfig, [{this_is_a_test, "yup it is"}]),

    SysConfigSrc = filename:join([LibDir1, "config", "sys.config.src"]),
    rlx_test_utils:write_config(SysConfigSrc, [{this_is_a_test, "yup it is"}]),

    VmArgs = filename:join([LibDir1, "config", "vm.args"]),
    ec_file:write(VmArgs, ""),

    VmArgsSrc = filename:join([LibDir1, "config", "vm.args.src"]),
    ec_file:write(VmArgsSrc, ""),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{dev_mode, true},
                  {sys_config_src, SysConfigSrc},
                  {vm_args_src, VmArgsSrc},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do(undefined, undefined, [], [LibDir1], 3,
                          OutputDir, ConfigFile),
    [{{foo, "0.0.1"}, _Release}] = maps:to_list(rlx_state:realized_releases(State)),


    case os:type() of
        {unix, _} ->
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_2-0.0.1"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_1-0.0.1"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_2-0.0.1"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "lib", "lib_dep_1-0.0.1"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                      "sys.config.src"]))),
            ?assert(ec_file:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                      "vm.args.src"]))),
            ?assert(not ec_file:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                          "sys.config"]))),
            ?assert(not ec_file:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                          "sys.config"])));
        {win32, _} ->
            ?assert(filelib:is_dir(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]))),
            ?assert(filelib:is_dir(filename:join([OutputDir, "foo", "lib", "non_goal_2-0.0.1"]))),
            ?assert(filelib:is_dir(filename:join([OutputDir, "foo", "lib", "goal_app_1-0.0.1"]))),
            ?assert(filelib:is_dir(filename:join([OutputDir, "foo", "lib", "goal_app_2-0.0.1"]))),
            ?assert(filelib:is_dir(filename:join([OutputDir, "foo", "lib", "lib_dep_1-0.0.1"]))),
            ?assert(filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                      "sys.config.src"]))),
            ?assert(filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                   "vm.args.src"]))),
            ?assert(not filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                       "sys.config"]))),
            ?assert(not filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                       "vm.args"])))
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================
