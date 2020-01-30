%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2015, Tristan Sloughter
-module(rlx_archive_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         basic_tar/1,
         exclude_erts/1,
         exclude_src/1,
         include_src/1,
         overlay_archive/1]).

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
    State = rlx_state:new([], [{lib_dirs, [LibDir1]}], [release]),
    {ok, State1} = rlx_config:do(State),
    [{lib1, LibDir1},
     {state, State1} | Config].

all() ->
    [basic_tar, exclude_erts, exclude_src, include_src,
     overlay_archive].

basic_tar(Config) ->
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

    SysConfigSrc = filename:join([LibDir1, "config", "sys.config.src"]),
    rlx_test_utils:write_config(SysConfigSrc, [{this_is_a_test, "yup it is"}]),

    VmArgsSrc = filename:join([LibDir1, "config", "vm.args.src"]),
    ec_file:write(VmArgsSrc, ""),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                                [{release, {foo, "0.0.1"},
                                  [goal_app_1,
                                   goal_app_2]},
                                 {sys_config_src, SysConfigSrc},
                                 {vm_args_src, VmArgsSrc}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do([{relname, foo},
                           {relvsn, "0.0.1"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release", "tar"]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    TarFile = filename:join([OutputDir, "foo", "foo-0.0.1.tar.gz"]),
    {ok, Files} = erl_tar:table(TarFile, [compressed]),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/stdlib-.*/ebin/.*") =/= nomatch end, Files)),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/kernel-.*/ebin/.*") =/= nomatch end, Files)),

    %% only works in otp-21 and above
    case erlang:system_info(otp_release) of
        R when R =:= "21" orelse R =:= "22" ->
            ?assert(lists:member("releases/0.0.1/vm.args.src", Files)),
            ?assert(lists:member("releases/0.0.1/sys.config.src", Files));
        _ ->
            ok
    end,

    ?assert(filelib:is_regular(TarFile)).

exclude_erts(Config) ->
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
    {ok, State} = relx:do([{relname, foo},
                           {relvsn, "0.0.1"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile},
                           {include_erts, false},
                           {system_libs, false}], ["release", "tar"]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    TarFile = filename:join([OutputDir, "foo", "foo-0.0.1.tar.gz"]),
    {ok, Files} = erl_tar:table(TarFile, [compressed]),
    ?assert(lists:all(fun(X) -> re:run(X, "lib/stdlib-.*/ebin/.*") =:= nomatch end, Files)),
    ?assert(lists:all(fun(X) -> re:run(X, "lib/kernel-.*/ebin/.*") =:= nomatch end, Files)),
    ?assert(filelib:is_regular(TarFile)).

exclude_src(Config) ->
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
                                 {include_src, false}]),
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, State} = relx:do([{relname, foo},
                           {relvsn, "0.0.1"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release", "tar"]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
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
    {ok, State} = relx:do([{relname, foo},
                           {relvsn, "0.0.1"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release", "tar"]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    TarFile = filename:join([OutputDir, "foo", "foo-0.0.1.tar.gz"]),
    {ok, Files} = erl_tar:table(TarFile, [compressed]),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/stdlib-.*/src/.*") =/= nomatch end, Files)),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/kernel-.*/src/.*") =/= nomatch end, Files)),
    ?assert(filelib:is_regular(TarFile)).

overlay_archive(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
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
    rlx_test_utils:write_config(ConfigFile,
                 [{overlay_vars, [OverlayVars1, OverlayVars2]},
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
                              "bin/{{default_release_name}}-{{default_release_version}}"}]},
                  {release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),

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

    ok = rlx_util:mkdir_p(TestDirFull),
    ok = file:write_file(TestFileFull, rlx_test_utils:test_template_contents()),

    TemplateFile = filename:join([LibDir1, "test_template"]),
    ok = file:write_file(TemplateFile, rlx_test_utils:test_template_contents()),
    {ok, FileInfo} = file:read_file_info(TemplateFile),
    ok = file:write_file_info(TemplateFile, FileInfo#file_info{mode=8#00777}),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    {ok, State} = relx:do([{relname, foo},
                           {relvsn, "0.0.1"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release", "tar"]),

    [{{foo, "0.0.1"}, Release}] = maps:to_list(rlx_state:realized_releases(State)),
    AppSpecs = rlx_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)),

    % check that the chmod of our file worked
    ChmodedFile = filename:join([OutputDir,"foo",SecondTestDir,TestScript]),
    {ok, ChmodedInfo} = file:read_file_info (ChmodedFile),
    % mode from file_info is a bitmask which might have other bits set, but
    % if we mask those we care about and check we should get true, see details
    % here http://stackoverflow.com/questions/13183838/how-to-use-erlang-fileread-file-info-permissions-mode-info
    ?assert(ChmodedInfo#file_info.mode band 8#00700 =:= 8#00700),

    % check that the templated chmod of our file worked
    ChmodedFile2 = filename:join([OutputDir,"foo",SecondTestDir,TestScript2]),
    {ok, ChmodedInfo2} = file:read_file_info (ChmodedFile2),
    ?assert(ChmodedInfo2#file_info.mode band 8#00770 =:= 8#00770),

    TarFile = filename:join([OutputDir, "foo", "foo-0.0.1.tar.gz"]),
    {ok, Files} = erl_tar:table(TarFile, [compressed]),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/stdlib-.*/src/.*") =/= nomatch end, Files)),
    ?assert(lists:any(fun(X) -> re:run(X, "lib/kernel-.*/src/.*") =/= nomatch end, Files)),
    ?assert(filelib:is_regular(TarFile)).
