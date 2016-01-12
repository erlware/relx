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
         include_src/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

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
    [basic_tar, exclude_erts, exclude_src, include_src].

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

    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
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

    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
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

    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
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

    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),
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
