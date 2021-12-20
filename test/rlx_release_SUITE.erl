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
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [{group, regular_mode},
     {group, dev_mode},
     {group, prod_mode},
     {group, minimal_mode}].

%% Tests using dev_mode can't run in parallel because they write to the same directories
groups() ->
    [{regular_mode, [shuffle, parallel],
      [make_release, make_config_release, make_release_semver, overlay_release,
       make_extend_release, make_extend_release_versioned, make_extend_config_release,
       make_scriptless_release, make_app_type_none_release, include_powershell,
       include_unix_powershell_win32, make_release_with_default_sys_config_vm_args_src,
       make_release_with_default_sys_config_vm_args,
       make_not_included_nodetool_release, make_src_release, make_excluded_src_release,
       make_exclude_modules_release, make_release_with_sys_config_vm_args_src,
       make_exclude_app_release, make_overridden_release, make_goalless_release,
       make_one_app_top_level_release, make_release_twice, make_erts_release,
       make_erts_config_release, make_included_nodetool_release, make_release_goal_reorder,
       make_release_keep_goal_order]},
     {dev_mode, [shuffle], [make_dev_mode_template_release,
                            make_dev_mode_release,
                            make_release_twice_dev_mode]},
     {prod_mode, [shuffle], [make_prod_mode_release]},
     {minimal_mode, [shuffle], [make_minimal_mode_release]}].

init_per_suite(Config) ->
    DataDir = filename:join(?config(data_dir, Config), ?MODULE),
    LibDir = filename:join([DataDir, rlx_test_utils:create_random_name("release_lib_dir_")]),
    ok = rlx_file_utils:mkdir_p(LibDir),

    rlx_test_utils:create_app(LibDir, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    [{lib_dir, LibDir} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DirName = rlx_test_utils:create_random_name("relx-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    [{out_dir, OutputDir} | Config].

end_per_testcase(_, _) ->
    ok.

make_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {release, {foo, "0.0.2"},
                   [goal_app_1,
                    goal_app_2]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.2"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_release_goal_reorder(Config) ->
    LibDir = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {reordered_foo, "0.0.1"},
                   [goal_app_2, goal_app_1]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(reordered_foo, [{root_dir, LibDir}, {lib_dirs, [LibDir]},
                                                     {output_dir, OutputDir} | RelxConfig]),

    [{{reordered_foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),

    %% goal_app_2 depends on goal_app_1, so the two are reordered when building the .rel file
    ?assertMatch([{kernel,_},
                  {stdlib,_},
                  {lib_dep_1,"0.0.1",load},
                  {non_goal_1,"0.0.1"},
                  {goal_app_1,"0.0.1"},
                  {non_goal_2,"0.0.1"},
                  {goal_app_2,"0.0.1"}], AppSpecs).

make_release_keep_goal_order(Config) ->
    LibDir = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {ordered_foo, "0.0.1"},
                   [non_goal_2, lib_dep_1]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(ordered_foo, [{root_dir, LibDir}, {lib_dirs, [LibDir]},
                                                   {output_dir, OutputDir} | RelxConfig]),

    [{{ordered_foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),

    %% goal_app_2 depends on goal_app_1, so the two are reordered when building the .rel file
    ?assertMatch([{kernel,_},
                  {stdlib,_},
                  {non_goal_2,"0.0.1"},
                  {lib_dep_1,"0.0.1"}], AppSpecs).

make_config_release(Config) ->
    DataDir = ?config(priv_dir, Config),
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    OtherAppsDir = filename:join([DataDir, rlx_test_utils:create_random_name("other_apps_")]),

    %% create goal_app_1-0.0.2
    rlx_test_utils:create_app(OtherAppsDir, "goal_app_1", "0.0.2", [stdlib,kernel,non_goal_1], []),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2],
                   [{some, config1}]},
                  {release, {foo, "0.0.2"},
                   [{goal_app_1, "0.0.2"},
                    goal_app_2],
                   [{some, config2}]},
                  {check_for_undefined_functions, false},
                  {lib_dirs, [LibDir1]}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [OtherAppsDir, LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.2"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.2"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),
    ?assertEqual([{some, config2}], rlx_release:config(Release)).

make_release_semver(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {foo, semver},
                   [goal_app_1,
                    goal_app_2]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, Vsn}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    {match, _} = re:run(Vsn, "[0-9\\.]+(\\+build\\.\\d+\\.ref[a-d0-9]+)?"),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_extend_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {release, {foo_test, "0.0.1", {extend, foo}},
                   [goal_app_2]},
                  {check_for_undefined_functions, false},
                  {lib_dirs, [filename:join(LibDir1, "*")]}],

    {ok, State} = relx:build_release(foo_test, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                                {output_dir, OutputDir} | RelxConfig]),

    [{{foo_test, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_extend_release_versioned(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {foo, "0.0.2"},
                   [goal_app_1,
                    goal_app_2]},
                  {release, {foo_test, "0.0.3", {extend, {foo, "0.0.2"}}},
                   [goal_app_2]},
                  {check_for_undefined_functions, false},
                  {lib_dirs, [filename:join(LibDir1, "*")]}],

    {ok, State} = relx:build_release(foo_test, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                                {output_dir, OutputDir} | RelxConfig]),

    [{{foo_test, "0.0.3"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

make_extend_config_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {release, {foo_test, "0.0.1", {extend, foo}},
                   [goal_app_2],
                   [{some, config}]},
                  {check_for_undefined_functions, false},
                  {lib_dirs, [filename:join(LibDir1, "*")]}],

    {ok, State} = relx:build_release(foo_test, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                                {output_dir, OutputDir} | RelxConfig]),

    [{{foo_test, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),
    ?assertEqual([{some, config}], rlx_release:config(Release)).

make_scriptless_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    RelxConfig = [{generate_start_script, false},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  %% the generate_start_script should take precedence
                  {include_start_scripts_for, [unix]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    ?assert(not rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo"]))),
    ?assert(not rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo-0.0.1"]))),
    ?assert(not rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "psutil.ps1"]))),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

include_powershell(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {include_start_scripts_for, [powershell]},
                  {check_for_undefined_functions, false}],

    {ok, _} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                       {output_dir, OutputDir} | RelxConfig]),

    ?assert(not rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo"]))),
    ?assert(not rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo-0.0.1"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo.ps1"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo-0.0.1.ps1"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "psutil.ps1"]))).

include_unix_powershell_win32(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {include_start_scripts_for, [powershell, unix, win32]},
                  {check_for_undefined_functions, false}],

    {ok, _} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                       {output_dir, OutputDir} | RelxConfig]),

    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo-0.0.1"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo.cmd"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo-0.0.1.cmd"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo.ps1"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "foo-0.0.1.ps1"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "psutil.ps1"]))).

make_overridden_release(Config) ->
    DataDir = ?config(priv_dir, Config),
    OverrideDir1 = filename:join([DataDir, rlx_test_utils:create_random_name("override_dir_")]),
    LibDir = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    OverrideApp = rlx_test_utils:create_random_name("override_app"),
    OverrideVsn = rlx_test_utils:create_random_vsn(),
    OverrideAppDir = filename:join(OverrideDir1, OverrideApp ++ "-" ++ OverrideVsn),

    rlx_test_utils:create_app(OverrideDir1, OverrideApp, OverrideVsn, [stdlib,kernel], []),
    OverrideAppName = erlang:list_to_atom(OverrideApp),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    OverrideAppName,
                    goal_app_2]},
                  {dev_mode, true},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir}, {lib_dirs, [OverrideDir1, LibDir]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
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
            ?assertEqual(filename:absname(OverrideAppDir), Real)
    end.

%% Test to ensure that an excluded app and its deps are not included in a release
make_exclude_app_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1]},
                  {exclude_apps, [non_goal_1]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(not lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)).

make_app_type_none_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    {goal_app_2, none}]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),
    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1", none}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

overlay_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

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
                                  OverlayVars1, OverlayVars2]},
                  {overlay, [{mkdir, "{{target_dir}}/fooo"},
                             {mkdir, "{{target_dir}}/{{var_list_dir}}"},
                             {copy, "{{goal_app_1}}/priv/subdir/foo.txt",
                              "{{target_dir}}/{{foo_dir}}/test_app_overlay"},
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
                              "bin/{{release_name}}-{{release_version}}"},
                             {copy, "{{erts_dir}}/bin/erl", "bin/copy.erl"}]},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {check_for_undefined_functions, false}],

    VarsFile1 = filename:join([LibDir1, "vars1.config"]),
    %% `tpl_var' is defined in vars1, but redifined in vars2 using template.
    %% `api_caller_var' is to be injected as an API caller overlay var.
    rlx_test_utils:write_config(VarsFile1, [{yahoo, "yahoo"},
                                            {yahoo2, [{foo, "bar"}]},
                                            {foo_yahoo, "foo_{{yahoo}}"},
                                            {foo_dir, "foodir"},
                                            {prop2, 1},
                                            {tpl_var, "defined in vars1"},
                                            {api_caller_var, "{{api_caller_var}}"}]),

    %% regression test for #827
    %% OverlayVars4 must take precedence over OverlayVars3, meaning prop2 should equal 2 not 3
    VarsFile2 = filename:join([LibDir1, "vars2.config"]),
    rlx_test_utils:write_config(VarsFile2, [{google, "yahoo"},
                                            {yahoo2, "foo"},
                                            {tpl_arg, "a template value"},
                                            {tpl_var, "Redefined in vars2 with {{tpl_arg}}"},
                                            OverlayVars3,
                                            OverlayVars4]),

    VarsFile3 = filename:join([LibDir1, "vars3.config"]),
    rlx_test_utils:write_config(VarsFile3, [{google, "yahoo"},
                                            {prop2, 3},
                                            {yahoo4, "{{yahoo}}/{{yahoo2}}4"}]),

    VarsFile4 = filename:join([LibDir1, "vars4.config"]),
    rlx_test_utils:write_config(VarsFile4, [{prop1, "val1"},
                                            {prop2, 2}]),

    ok = rlx_file_utils:mkdir_p(TestDirFull),
    ok = file:write_file(TestFileFull, rlx_test_utils:test_template_contents()),

    TemplateFile = filename:join([LibDir1, "test_template"]),
    ok = file:write_file(TemplateFile, rlx_test_utils:test_template_contents()),
    {ok, FileInfo} = file:read_file_info(TemplateFile),
    ok = file:write_file_info(TemplateFile, FileInfo#file_info{mode=8#00777}),

    ApiCallerVarValue = "api-caller-var",
    ApiCallerReleaseNameValue = "release-var-conflict",
    ApiCallerConfigFileValue = "state-var-conflict",
    ApiCallerYahooValue = "overlays-file-conflict",
    ApiCallerOverlays =
        [{api_caller_var, ApiCallerVarValue},
         {release_name, ApiCallerReleaseNameValue},
         {yahoo, ApiCallerYahooValue}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {overlay_vars_values, ApiCallerOverlays},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "non-file-variable-list"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "fooo"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "foodir", "vars1.config"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "foodir", "test_app_overlay"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "yahoo", "vars1.config"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", SecondTestDir, TestDir, TestFile]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "copy.erl"]))),

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
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    RelxConfig = [{release, {foo, "0.0.1"},
                   []},
                  {check_for_undefined_functions, false}],

    ?assertError({error, {rlx_resolve, {no_goals_specified, {foo, "0.0.1"}}}},
                 relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                          {output_dir, OutputDir} | RelxConfig])),

    ok.

make_one_app_top_level_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    %% Use non_goal_2 here because this is to test when a top level app
    %% has no dependencies of its own
    RelxConfig = [{release, {foo, "0.0.1"},
                   [{non_goal_2, "0.0.1"}]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)).

make_dev_mode_release(Config) ->
    LibDir1 = proplists:get_value(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    SysConfig = filename:join([LibDir1, "config", "sys.config"]),
    rlx_test_utils:write_config(SysConfig, [{this_is_a_test, "yup it is"}]),

    VmArgs = filename:join([LibDir1, "config", "vm.args"]),
    rlx_file_utils:write(VmArgs, ""),

    RelxConfig = [{dev_mode, true},
                  {sys_config, SysConfig},
                  {vm_args, VmArgs},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, _Release}] = maps:to_list(rlx_state:realized_releases(State)),

    case os:type() of
        {unix, _} ->
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_2-0.0.1"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_1-0.0.1"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_2-0.0.1"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "lib_dep_1-0.0.1"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                             "sys.config"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
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
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    SysConfig = filename:join([LibDir1, "config", "sys.config"]),
    SysConfigTerm = [{this_is_a_test, "yup it is"},
                     {this_is_an_overlay_var, "{{var1}}"}],
    rlx_test_utils:write_config(SysConfig, SysConfigTerm),

    VmArgs = filename:join([LibDir1, "config", "vm.args"]),
    rlx_file_utils:write(VmArgs, "-sname {{nodename}}"),

    VarsFile1 = filename:join([LibDir1, "config", "vars1.config"]),
    rlx_test_utils:write_config(VarsFile1, [{var1, "indeed it is"},
                                            {nodename, "testnode"}]),

    RelxConfig = [{dev_mode, true},
                  {mode, dev},
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
                    goal_app_2]},
                  {check_for_undefined_functions, false}],


    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, _Release}] = maps:to_list(rlx_state:realized_releases(State)),

    ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]))),
    ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_2-0.0.1"]))),
    ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_1-0.0.1"]))),
    ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_2-0.0.1"]))),
    ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "lib_dep_1-0.0.1"]))),
    ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                         "sys.config"]))),
    ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                         "vm.args"]))),
    %% ensure that the original sys.config didn't get overwritten
    ?assertMatch({ok, SysConfigTerm}, file:consult(SysConfig)),
    %% ensure that the original vm.args didn't get overwritten
    ?assertMatch({ok, <<"-sname {{nodename}}">>}, file:read_file(VmArgs)).

%% verify that creating a new app with the same name after creating a release results in the
%% newest version being used in the new release
make_release_twice(Config) ->
    PrivDir = ?config(priv_dir, Config),
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    OtherAppsDir = filename:join([PrivDir, rlx_test_utils:create_random_name("other_apps_")]),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {check_for_undefined_functions, false}],

    {ok, State1} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [OtherAppsDir, LibDir1]},
                                            {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State1)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),


    rlx_test_utils:create_app(OtherAppsDir, "non_goal_1", "0.0.3", [stdlib,kernel], [lib_dep_1]),
    {ok, State2} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [OtherAppsDir, LibDir1]},
                                            {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release1}] = maps:to_list(rlx_state:realized_releases(State2)),
    AppSpecs1 = rlx_release:app_specs(Release1),
    ?assert(lists:keymember(stdlib, 1, AppSpecs1)),
    ?assert(lists:keymember(kernel, 1, AppSpecs1)),
    ?assert(lists:member({non_goal_1, "0.0.3"}, AppSpecs1)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs1)).

%% verify that creating a new app with the same name after creating a release results in the
%% newest version being used in the new release
make_release_twice_dev_mode(Config) ->
    PrivDir = ?config(priv_dir, Config),
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    OtherAppsDir = filename:join([PrivDir, rlx_test_utils:create_random_name("other_apps_")]),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {dev_mode, true},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [OtherAppsDir, LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    rlx_test_utils:create_app(OtherAppsDir, "non_goal_1", "0.0.3", [stdlib,kernel], [lib_dep_1]),
    {ok, State2} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [OtherAppsDir, LibDir1]},
                                            {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release1}] = maps:to_list(rlx_state:realized_releases(State2)),
    AppSpecs1 = rlx_release:app_specs(Release1),
    ?assert(lists:keymember(stdlib, 1, AppSpecs1)),
    ?assert(lists:keymember(kernel, 1, AppSpecs1)),
    ?assert(lists:member({non_goal_1, "0.0.3"}, AppSpecs1)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs1)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs1)).

make_erts_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    ErtsVsn = erlang:system_info(version),
    RelxConfig = [{release, {foo, "0.0.1"}, {erts, ErtsVsn},
                   [goal_app_1]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assertEqual(ErtsVsn, rlx_release:erts(Release)).

make_erts_config_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    ErtsVsn = erlang:system_info(version),
    RelxConfig = [{release, {foo, "0.0.1"}, {erts, ErtsVsn},
                   [goal_app_1],
                   [{some, config}]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assertEqual(ErtsVsn, rlx_release:erts(Release)),
    ?assertEqual([{some, config}], rlx_release:config(Release)).

make_included_nodetool_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    ErtsVsn = erlang:system_info(version),
    RelxConfig = [{release, {foo, "0.0.1"}, {erts, ErtsVsn},
                   [goal_app_1]},
                  {extended_start_script, true},
                  {include_nodetool, true},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "nodetool"]))),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assertEqual(ErtsVsn, rlx_release:erts(Release)).

make_not_included_nodetool_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    ErtsVsn = erlang:system_info(version),
    RelxConfig = [{release, {foo, "0.0.1"}, {erts, ErtsVsn},
                   [goal_app_1]},
                  {extended_start_script, true},
                  {include_nodetool, false},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    %% extended start script needs nodetool to work, so the
    %% {include_nodetool, false} option is simply ignored
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "bin", "nodetool"]))),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assertEqual(ErtsVsn, rlx_release:erts(Release)).

make_src_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    ErtsVsn = erlang:system_info(version),
    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1]},
                  {extended_start_script, true},
                  {include_erts, true},
                  {include_src, true},
                  {check_for_undefined_functions, false}],


    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "erts-"++ErtsVsn, "bin"]))),
    ?assert(rlx_file_utils:exists(filename:join([OutputDir, "foo", "lib",
                                                 "goal_app_1-0.0.1", "src"]))).

make_excluded_src_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    ErtsVsn = erlang:system_info(version),
    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1]},
                  {extended_start_script, true},
                  {include_erts, true},
                  {include_src, false},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(not rlx_file_utils:exists(filename:join([OutputDir, "foo", "erts-"++ErtsVsn, "src"]))),
    ?assert(not rlx_file_utils:exists(filename:join([OutputDir, "foo", "lib",
                                                     "goal_app_1-0.0.1", "src"]))).

%% Test to ensure that excluded modules don't end up in the release 
make_exclude_modules_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1]},
                  {exclude_modules, [{non_goal_1, [a_real_beamnon_goal_1]}]}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    %% ensure that the excluded module beam file didn't get copied
    ?assert(not rlx_file_utils:exists(filename:join([OutputDir, "foo", "lib",
                                                     "non_goal_1-0.0.1", "ebin",
                                                     "a_real_beamnon_goal_1.beam"]))),

    ?assertMatch({ok, [{application,non_goal_1,
                        [{description,_},
                         {vsn,"0.0.1"},
                         {modules,[]},
                         {included_applications,_},
                         {registered,_},
                         {applications,_}]}]},
                 file:consult(filename:join([OutputDir, "foo", "lib",
                                             "non_goal_1-0.0.1", "ebin",
                                             "non_goal_1.app"]))).

make_release_with_sys_config_vm_args_src(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    %% the .src versions should take precedence and the others are not copied
    SysConfig = filename:join([LibDir1, "config", "sys.config"]),
    rlx_test_utils:write_config(SysConfig, [{this_is_a_test, "yup it is"}]),

    SysConfigSrc = filename:join([LibDir1, "config", "sys.config.src"]),
    rlx_test_utils:write_config(SysConfigSrc, [{this_is_a_test, "yup it is"}]),

    VmArgs = filename:join([LibDir1, "config", "vm.args"]),
    rlx_file_utils:write(VmArgs, ""),

    VmArgsSrc = filename:join([LibDir1, "config", "vm.args.src"]),
    rlx_file_utils:write(VmArgsSrc, ""),

    RelxConfig = [{dev_mode, true},
                  {sys_config_src, SysConfigSrc},
                  {vm_args_src, VmArgsSrc},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, _Release}] = maps:to_list(rlx_state:realized_releases(State)),

    case os:type() of
        {unix, _} ->
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_2-0.0.1"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_1-0.0.1"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_2-0.0.1"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "lib_dep_1-0.0.1"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                             "sys.config.src"]))),
            ?assert(rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                             "vm.args.src"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                                 "sys.config"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
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

make_release_with_default_sys_config_vm_args_src(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    RootDir = filename:join(LibDir1, "make_release_with_default_sys_config_vm_args_src"),

    %% the .src versions should take precedence and the others are not copied
    SysConfig = filename:join([RootDir, "config", "sys.config"]),
    rlx_test_utils:write_config(SysConfig, [{this_is_a_test, "yup it is"}]),

    SysConfigSrc = filename:join([RootDir, "config", "sys.config.src"]),
    rlx_test_utils:write_config(SysConfigSrc, [{this_is_a_test, "yup it is"}]),

    VmArgs = filename:join([RootDir, "config", "vm.args"]),
    rlx_file_utils:write(VmArgs, ""),

    VmArgsSrc = filename:join([RootDir, "config", "vm.args.src"]),
    rlx_file_utils:write(VmArgsSrc, ""),

    RelxConfig = [{dev_mode, false},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, RootDir}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, _Release}] = maps:to_list(rlx_state:realized_releases(State)),

    ?assert(filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                           "sys.config.src"]))),
    ?assert(filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                           "vm.args.src"]))),

    %% check that sys.config and vm.args don't exist because .src was used instead
    ?assert(not filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                               "sys.config"]))),
    ?assert(not filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                               "vm.args"]))),
    ok.

make_release_with_default_sys_config_vm_args(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),
    RootDir = filename:join(LibDir1, "make_release_with_default_sys_config_vm_args"),

    %% no .src versions of the files means these get used
    SysConfig = filename:join([RootDir, "config", "sys.config"]),
    rlx_test_utils:write_config(SysConfig, [{this_is_a_test, "yup it is"}]),

    VmArgs = filename:join([RootDir, "config", "vm.args"]),
    rlx_file_utils:write(VmArgs, ""),

    RelxConfig = [{mode, prod},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, RootDir}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, _Release}] = maps:to_list(rlx_state:realized_releases(State)),

    ?assert(filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                           "sys.config"]))),
    ?assert(filelib:is_file(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                           "vm.args"]))),
    ok.

make_prod_mode_release(Config) ->
    LibDir1 = proplists:get_value(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    SysConfig = filename:join([LibDir1, "config", "sys.config"]),
    rlx_test_utils:write_config(SysConfig, [{this_is_a_test, "yup it is"}]),

    VmArgs = filename:join([LibDir1, "config", "vm.args"]),
    rlx_file_utils:write(VmArgs, ""),

    RelxConfig = [{mode, prod},
                  %% osx test fails if debug_info is strip
                  {debug_info, keep},
                  {sys_config, SysConfig},
                  {vm_args, VmArgs},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, _Release}] = maps:to_list(rlx_state:realized_releases(State)),

    case os:type() of
        {unix, _} ->
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_2-0.0.1"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_1-0.0.1"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_2-0.0.1"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "lib_dep_1-0.0.1"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                                 "sys.config"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
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

make_minimal_mode_release(Config) ->
    LibDir1 = proplists:get_value(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    SysConfig = filename:join([LibDir1, "config", "sys.config"]),
    rlx_test_utils:write_config(SysConfig, [{this_is_a_test, "yup it is"}]),

    VmArgs = filename:join([LibDir1, "config", "vm.args"]),
    rlx_file_utils:write(VmArgs, ""),

    RelxConfig = [{mode, minimal},
                  %% osx test fails if debug_info is strip
                  {debug_info, keep},
                  {sys_config, SysConfig},
                  {vm_args, VmArgs},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {check_for_undefined_functions, false}],

    {ok, State} = relx:build_release(foo, [{root_dir, LibDir1}, {lib_dirs, [LibDir1]},
                                           {output_dir, OutputDir} | RelxConfig]),

    [{{foo, "0.0.1"}, _Release}] = maps:to_list(rlx_state:realized_releases(State)),

    case os:type() of
        {unix, _} ->
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_1-0.0.1"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "non_goal_2-0.0.1"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_1-0.0.1"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "goal_app_2-0.0.1"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "lib", "lib_dep_1-0.0.1"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                                                 "sys.config"]))),
            ?assert(not rlx_file_utils:is_symlink(filename:join([OutputDir, "foo", "releases", "0.0.1",
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

%%%===================================================================
%%% Helper Functions
%%%===================================================================
