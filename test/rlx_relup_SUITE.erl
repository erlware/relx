-module(rlx_relup_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    [{lib_dir, LibDir1} | Config].

all() ->
    [make_relup_release, make_relup_release2].

make_relup_release(Config) ->
    LibDir1 = ?config(lib_dir, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.2", [stdlib,kernel,non_goal_1], []),
    {ok, GA1} = rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.3", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.2", [stdlib,kernel,goal_app_1,non_goal_2], []),
    {ok, GA2} = rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.3", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    rlx_test_utils:write_appup_file(GA1, "0.0.2"),
    rlx_test_utils:write_appup_file(GA2, "0.0.2"),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
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
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, _} = relx:do(foo, "0.0.1", [], [LibDir1], 3,
                      OutputDir, ConfigFile),

    {ok, _} = relx:do(foo, "0.0.2", [], [LibDir1], 3,
                      OutputDir, ConfigFile),

    %% Goal apps are removed to simulate a users dev environment where the apps
    %% being used in an appup/relup are likely only under _rel/<release>/lib/
    ec_file:remove(filename:join(LibDir1, "goal_app_1-0.0.1"), [recursive]),
    ec_file:remove(filename:join(LibDir1, "goal_app_1-0.0.2"), [recursive]),

    {ok, State} = relx:do([{relname, foo},
                           {relvsn, "0.0.3"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release", "relup"]),

    %% we should have one 'resolved' release and three discovered realized_releases.
    ?assertMatch([{foo, "0.0.1"},
                  {foo, "0.0.2"},
                  {foo, "0.0.3"}],
                 lists:sort(maps:keys(rlx_state:realized_releases(State)))),
    Release = maps:get({foo, "0.0.3"}, rlx_state:realized_releases(State)),
    ?assert(rlx_release:realized(Release)),
    ?assert(not rlx_release:realized(maps:get({foo, "0.0.2"},
                                                       rlx_state:realized_releases(State)))),
    ?assert(not rlx_release:realized(maps:get({foo, "0.0.1"},
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
    LibDir1 = ?config(lib_dir, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.2", [stdlib,kernel,non_goal_1], []),
    {ok, GA1} = rlx_test_utils:create_app(LibDir1, "goal_app_1", "0.0.3", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.2", [stdlib,kernel,goal_app_1,non_goal_2], []),
    {ok, GA2} = rlx_test_utils:create_app(LibDir1, "goal_app_2", "0.0.3", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    rlx_test_utils:write_appup_file(GA1, "0.0.1"),
    rlx_test_utils:write_appup_file(GA2, "0.0.1"),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
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
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, _} = relx:do(foo, "0.0.1", [], [LibDir1], 3,
                             OutputDir, ConfigFile),
    {ok, _} = relx:do(foo, "0.0.2", [], [LibDir1], 3,
                         OutputDir, ConfigFile),
    {ok, State} = relx:do([{relname, foo},
                           {relvsn, "0.0.3"},
                           {upfrom, "0.0.1"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release", "relup"]),

    %% we should have one 'resolved' release and three discovered realized_releases.
    ?assertMatch([{foo, "0.0.1"},
                  {foo, "0.0.2"},
                  {foo, "0.0.3"}],
                 lists:sort(maps:keys(rlx_state:realized_releases(State)))),
    Release = maps:get({foo, "0.0.3"}, rlx_state:realized_releases(State)),
    ?assert(rlx_release:realized(Release)),
    ?assert(not rlx_release:realized(maps:get({foo, "0.0.2"},
                                                       rlx_state:realized_releases(State)))),
    ?assert(not rlx_release:realized(maps:get({foo, "0.0.1"},
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
