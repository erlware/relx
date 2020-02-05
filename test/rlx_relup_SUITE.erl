-module(rlx_relup_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [make_relup_release, make_relup_release2, no_upfrom_release].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    DataDir = filename:join(proplists:get_value(priv_dir, Config), ?MODULE),
    LibDir1 = filename:join([DataDir, rlx_test_utils:create_random_name("lib_dir1_")]),
    ok = rlx_file_utils:mkdir_p(LibDir1),
    [{lib_dir, LibDir1} | Config].

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

    RelxConfig = [{release, {foo, "0.0.1"},
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
                    {goal_app_2, "0.0.3"}]}],
    OutputDir = filename:join([?config(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    Apps = rlx_test_utils:all_apps([LibDir1]),
    {ok, _State} = relx:build_release({foo, "0.0.1"}, Apps, [{root_dir, LibDir1},
                                                             {output_dir, OutputDir} | RelxConfig]),

    {ok, _State1} = relx:build_release({foo, "0.0.2"}, Apps, [{root_dir, LibDir1},
                                                              {output_dir, OutputDir} | RelxConfig]),

    %% Goal apps are removed to simulate a users dev environment where the apps
    %% being used in an appup/relup are likely only under _rel/<release>/lib/
    ec_file:remove(filename:join(LibDir1, "goal_app_1-0.0.1"), [recursive]),
    ec_file:remove(filename:join(LibDir1, "goal_app_1-0.0.2"), [recursive]),

    {ok, State2} = relx:build_release({foo, "0.0.3"}, Apps, [{root_dir, LibDir1},
                                                              {output_dir, OutputDir} | RelxConfig]),

    %% `undefined' means automatically find last release version
    %% in this case `0.0.2'
    {ok, _State3} = relx:build_relup(foo, "0.0.3", undefined,
                                     [{root_dir, LibDir1},
                                            {output_dir, OutputDir} | RelxConfig]),

    ?assertMatch({ok, [{"0.0.3",
                        [{"0.0.2",[],[point_of_no_return]}],
                        [{"0.0.2",[],[point_of_no_return]}]}]},
                 file:consult(filename:join([OutputDir, foo, "releases", "0.0.3", "relup"]))),

    [{{foo, "0.0.3"}, Release}] = maps:to_list(rlx_state:realized_releases(State2)),

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

    RelxConfig = [{release, {foo, "0.0.1"},
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
                    {goal_app_2, "0.0.3"}]}],
    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    Apps = rlx_test_utils:all_apps([LibDir1]),
    {ok, _} = relx:build_release({foo, "0.0.1"}, Apps, [{root_dir, LibDir1},
                                                        {output_dir, OutputDir} | RelxConfig]),

    {ok, _} = relx:build_release({foo, "0.0.2"}, Apps, [{root_dir, LibDir1},
                                                              {output_dir, OutputDir} | RelxConfig]),

    {ok, State2} = relx:build_release({foo, "0.0.3"}, Apps, [{root_dir, LibDir1},
                                                             {output_dir, OutputDir} | RelxConfig]),

    {ok, _State3} = relx:build_relup(foo, "0.0.3", "0.0.1",
                                     [{root_dir, LibDir1},
                                      {output_dir, OutputDir} | RelxConfig]),

    ?assertMatch({ok, [{"0.0.3",
                        [{"0.0.1",[],[point_of_no_return]}],
                        [{"0.0.1",[],[point_of_no_return]}]}]},
                 file:consult(filename:join([OutputDir, foo, "releases", "0.0.3", "relup"]))),

    [{{foo, "0.0.3"}, Release}] = maps:to_list(rlx_state:realized_releases(State2)),
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

no_upfrom_release(Config) ->
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

    RelxConfig = [{release, {foo, "0.0.1"},
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
                    {goal_app_2, "0.0.3"}]}],
    OutputDir = filename:join([?config(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    Apps = rlx_test_utils:all_apps([LibDir1]),
    {ok, _} = relx:build_release({foo, "0.0.3"}, Apps, [{root_dir, LibDir1},
                                                        {output_dir, OutputDir} | RelxConfig]),

    %% no release to build relup from leads to an error
    ?assertError({error, {rlx_relup, {no_upfrom_release_found, "0.0.3"}}},
                 relx:build_relup(foo, "0.0.3", undefined,
                                  [{root_dir, LibDir1},
                                   {output_dir, OutputDir} | RelxConfig])).
