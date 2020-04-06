%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2015, Tristan Sloughter
-module(rlx_tar_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [basic_tar, exclude_erts, exclude_src, include_src, overlay_archive].

init_per_suite(Config) ->
    DataDir = filename:join(proplists:get_value(data_dir, Config), ?MODULE),
    LibDir = filename:join([DataDir, rlx_test_utils:create_random_name("tar_lib_dir1_")]),
    ok = rlx_file_utils:mkdir_p(LibDir),

    rlx_test_utils:create_app(LibDir, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    Apps = rlx_test_utils:all_apps([LibDir]),

    [{lib_dir, LibDir},
     {apps, Apps} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DirName = rlx_test_utils:create_random_name("tar-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    [{out_dir, OutputDir} | Config].

end_per_testcase(_, _) ->
    ok.

basic_tar(Config) ->
    LibDir1 = ?config(lib_dir, Config),
    Apps = ?config(apps, Config),
    OutputDir = ?config(out_dir, Config),

    SysConfigSrc = filename:join([LibDir1, "config", "sys.config.src"]),
    rlx_test_utils:write_config(SysConfigSrc, [{this_is_a_test, "yup it is"}]),

    VmArgsSrc = filename:join([LibDir1, "config", "vm.args.src"]),
    rlx_file_utils:write(VmArgsSrc, ""),

    RelxConfig = [{release, {foo, "0.0.4"},
                   [goal_app_1,
                    goal_app_2]},
                  {sys_config_src, SysConfigSrc},
                  {vm_args_src, VmArgsSrc}],

    {ok, Release} = relx:build_tar(foo, Apps, [{root_dir, LibDir1},
                                               {output_dir, OutputDir} | RelxConfig]),

    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    TarFile = filename:join([OutputDir, "foo", "foo-0.0.4.tar.gz"]),
    {ok, Files} = erl_tar:table(TarFile, [compressed]),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/stdlib-.*/ebin/.*") =/= nomatch end, Files)),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/kernel-.*/ebin/.*") =/= nomatch end, Files)),

    %% only works in otp-21 and above
    case list_to_integer(erlang:system_info(otp_release)) >= 21 of
        true ->
            ?assert(lists:member("releases/0.0.4/vm.args.src", Files)),
            ?assert(lists:member("releases/0.0.4/sys.config.src", Files));
        _ ->
            ok
    end,

    ?assert(filelib:is_regular(TarFile)).

exclude_erts(Config) ->
    LibDir1 = proplists:get_value(lib_dir, Config),
    Apps = ?config(apps, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {foo, "0.0.3"},
                   [goal_app_1,
                    goal_app_2]},
                  {include_erts, false}
                 ],
    {ok, Release} = relx:build_tar(foo, Apps, [{root_dir, LibDir1},
                                               {output_dir, OutputDir} | RelxConfig]),

    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    TarFile = filename:join([OutputDir, "foo", "foo-0.0.3.tar.gz"]),
    {ok, Files} = erl_tar:table(TarFile, [compressed]),
    ?assert(lists:all(fun(X) -> re:run(X, "lib/stdlib-.*/ebin/.*") =:= nomatch end, Files)),
    ?assert(lists:all(fun(X) -> re:run(X, "lib/kernel-.*/ebin/.*") =:= nomatch end, Files)),
    ?assert(filelib:is_regular(TarFile)).

exclude_src(Config) ->
    LibDir1 = proplists:get_value(lib_dir, Config),
    Apps = ?config(apps, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]},
                  {include_src, false}],

    {ok, Release} = relx:build_tar(foo, Apps, [{root_dir, LibDir1},
                                               {output_dir, OutputDir} | RelxConfig]),

    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    TarFile = filename:join([OutputDir, "foo", "foo-0.0.1.tar.gz"]),
    {ok, Files} = erl_tar:table(TarFile, [compressed]),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/stdlib-.*/src/.*") =:= nomatch end, Files)),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/kernel-.*/src/.*") =:= nomatch end, Files)),
    ?assert(filelib:is_regular(TarFile)).

include_src(Config) ->
    LibDir1 = proplists:get_value(lib_dir, Config),
    Apps = ?config(apps, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {foo, "0.0.2"},
                   [goal_app_1,
                    goal_app_2]}],

    {ok, Release} = relx:build_tar(foo, Apps, [{root_dir, LibDir1},
                                               {output_dir, OutputDir} | RelxConfig]),

    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    TarFile = filename:join([OutputDir, "foo", "foo-0.0.2.tar.gz"]),
    {ok, Files} = erl_tar:table(TarFile, [compressed]),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/stdlib-.*/src/.*") =/= nomatch end, Files)),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/kernel-.*/src/.*") =/= nomatch end, Files)),
    ?assert(filelib:is_regular(TarFile)).

overlay_archive(Config) ->
    LibDir1 = proplists:get_value(lib_dir, Config),
    Apps = ?config(apps, Config),
    OutputDir = ?config(out_dir, Config),

    OverlayVars1 = filename:join([LibDir1, "vars1.config"]),
    OverlayVars2 = filename:join([LibDir1, "vars2.config"]),
    OverlayVars3 = filename:join([LibDir1, "vars3.config"]),
    Template = filename:join([LibDir1, "test_template"]),
    TestDir = "first_test_dir",
    TestFile = "test_file",
    TestDirFull = filename:join([LibDir1, TestDir]),
    TestFileFull = filename:join(TestDirFull, TestFile),
    SecondTestDir = "second_test_dir",
    TestScript = "test_script",
    TestScript2 = "test_script2",

    RelxConfig = [{overlay_vars, [OverlayVars1, OverlayVars2]},
                  {overlay, [{mkdir, "{{target_dir}}/fooo"},
                             {copy, OverlayVars1,
                              "{{target_dir}}/{{foo_dir}}/vars1.config"},
                             {copy, OverlayVars1,
                              "{{target_dir}}/{{yahoo}}/"},
                             {link, OverlayVars1,
                              "{{target_dir}}/{{yahoo}}/vars.link.config"},
                             {copy, TestDirFull,
                              "{{target_dir}}/"++SecondTestDir++"/"},
                             {copy, TestScript,
                              "{{target_dir}}/"++SecondTestDir++"/"++TestScript},
                             {chmod, 8#00700,
                              "{{target_dir}}/"++SecondTestDir++"/"++TestScript},
                             {copy, TestScript2,
                              "{{target_dir}}/"++SecondTestDir++"/"++TestScript2},
                             {chmod, "{{test_script_perm}}",
                              "{{target_dir}}/"++SecondTestDir++"/"++TestScript2},
                             {template, Template,
                              "{{target_dir}}/test_template_resolved"},
                             {template, Template,
                              "bin/{{release_name}}-{{release_version}}"}]},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}],

    VarsFile1 = filename:join([LibDir1, "vars1.config"]),
    rlx_test_utils:write_config(VarsFile1, [{yahoo, "yahoo"},
                                            {yahoo2, [{foo, "bar"}]},
                                            {foo_yahoo, "foo_{{yahoo}}"},
                                            {foo_dir, "foodir"},
                                            {test_script_perm,8#00770}]),

    VarsFile2 = filename:join([LibDir1, "vars2.config"]),
    rlx_test_utils:write_config(VarsFile2, [{google, "yahoo"},
                                            {yahoo2, "foo"},
                                            OverlayVars3]),

    VarsFile3 = filename:join([LibDir1, "vars3.config"]),
    rlx_test_utils:write_config(VarsFile3, [{google, "yahoo"},
                                            {yahoo4, "{{yahoo}}/{{yahoo2}}4"}]),

    TestScriptFile = filename:join([LibDir1,TestScript]),
    ok = file:write_file(TestScriptFile, <<"#!/bin/sh\necho \"hello world\"">>),
    TestScriptFile2 = filename:join([LibDir1,TestScript2]),
    ok = file:write_file(TestScriptFile2, <<"#!/bin/sh\necho \"hello world 2\"">>),

    ok = rlx_file_utils:mkdir_p(TestDirFull),
    ok = file:write_file(TestFileFull, rlx_test_utils:test_template_contents()),

    TemplateFile = filename:join([LibDir1, "test_template"]),
    ok = file:write_file(TemplateFile, rlx_test_utils:test_template_contents()),
    {ok, FileInfo} = file:read_file_info(TemplateFile),
    ok = file:write_file_info(TemplateFile, FileInfo#file_info{mode=8#00777}),

    {ok, Release} = relx:build_tar(foo, Apps, [{root_dir, LibDir1},
                                               {output_dir, OutputDir} | RelxConfig]),

    AppSpecs = rlx_release:app_specs(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    %% check that the chmod of our file worked
    ChmodedFile = filename:join([OutputDir,"foo",SecondTestDir,TestScript]),
    {ok, ChmodedInfo} = file:read_file_info (ChmodedFile),
    %% mode from file_info is a bitmask which might have other bits set, but
    %% if we mask those we care about and check we should get true, see details
    %% here http://stackoverflow.com/questions/13183838/how-to-use-erlang-fileread-file-info-permissions-mode-info
    ?assert(ChmodedInfo#file_info.mode band 8#00700 =:= 8#00700),

    %% check that the templated chmod of our file worked
    ChmodedFile2 = filename:join([OutputDir,"foo",SecondTestDir,TestScript2]),
    {ok, ChmodedInfo2} = file:read_file_info (ChmodedFile2),
    ?assert(ChmodedInfo2#file_info.mode band 8#00770 =:= 8#00770),

    TarFile = filename:join([OutputDir, "foo", "foo-0.0.1.tar.gz"]),
    {ok, Files} = erl_tar:table(TarFile, [compressed]),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/stdlib-.*/src/.*") =/= nomatch end, Files)),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/kernel-.*/src/.*") =/= nomatch end, Files)),
    ?assert(filelib:is_regular(TarFile)).
