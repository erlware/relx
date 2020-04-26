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
-module(rlx_extended_bin_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(SLEEP_TIME, 2500).
-define(LONG_SLEEP_TIME, 2500).

all() ->
    case erlang:system_info(otp_release) of
        %% make them never run for now
        V when V =:= "22" ; V =:= "23" ->
            [{group, start_fail},
             {group, custom_setup}];
        _ ->
            []
    end.

groups() ->
    [{start_fail, [], [start_fail_when_no_name, start_fail_when_multiple_names,
                       start_fail_when_circular_argsfiles,
                       start_fail_when_missing_argsfile, start_fail_when_relative_argsfile]},
     {custom_setup, [], [start_sname_in_other_argsfile, start_preserves_arguments,
                         start_nodetool_with_data_from_argsfile, start_upgrade_escript_with_argsfile_data,
                         builtin_status_script, custom_status_script]}].

init_per_suite(Config) ->
    DataDir = filename:join(proplists:get_value(data_dir, Config), ?MODULE),
    LibDir = filename:join([DataDir, rlx_test_utils:create_random_name("extended_lib_dir1_")]),
    ok = rlx_file_utils:mkdir_p(LibDir),

    rlx_test_utils:create_app(LibDir, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    rlx_test_utils:create_app(LibDir, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    rlx_test_utils:create_app(LibDir, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    rlx_test_utils:create_app(LibDir, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    rlx_test_utils:create_app(LibDir, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    [{lib_dir, LibDir} | Config].

end_per_suite(_Config) ->
    ok.

init_per_group(start_fail, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DirName = rlx_test_utils:create_random_name("longname-extended-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),
    [{out_dir, OutputDir} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(start_fail, _Config) ->
    ok;
end_per_group(Group, Config) when Group =:= longname ;
                                  Group =:= shortname ->
    OutputDir = ?config(out_dir, Config),

    sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    %% a ping should fail after stopping a node
    ?assertMatch({error, 1, _}, sh(filename:join([OutputDir, "foo", "bin", "foo ping"]))),
    ok;
end_per_group(_, Config) ->
    OutputDir = ?config(out_dir, Config),
    sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

escript(Config) ->
    OutputDir = ?config(out_dir, Config),

    ok = rlx_file_utils:write(filename:join([OutputDir, "foo", "script.erl"]),
                              [rlx_test_utils:escript_contents()]),

    %% now start/stop the release to make sure the extended script is working
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo daemon"])),
    timer:sleep(?SLEEP_TIME),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    [ExpectedOutput] = io_lib:format("~s",
                                     [filename:join([OutputDir, "foo"])]),
    {ok, Output} = sh(filename:join([OutputDir, "foo", "bin", "foo escript script.erl"])),
    ?assertEqual(ExpectedOutput, Output).

os_var_timeouts(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                                [{release, {foo, "0.0.1"},
                                  [goal_app]},
                                 {lib_dirs, [filename:join(LibDir1, "*")]},
                                 {generate_start_script, true},
                                 {extended_start_script, true}
                                ]),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    {ok, _State} = relx:do([{relname, foo},
                            {relvsn, "0.0.1"},

                            {lib_dirs, [LibDir1]},
                            {log_level, 3},
                            {output_dir, OutputDir},
                            {config, ConfigFile}], ["release"]),

    ok = rlx_file_utils:write(filename:join([OutputDir, "foo", "script.erl"]),
                              [rlx_test_utils:escript_contents()]),

    %% now start/stop the release to make sure the extended script is working
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo daemon"])),
    timer:sleep(?SLEEP_TIME),
    ?assertEqual({ok, "ok"}, sh(filename:join([OutputDir, "foo", "bin",
                                               "foo rpcterms timer sleep 2000."]))),
    ?assertEqual({ok, "ok"}, sh(filename:join([OutputDir, "foo", "bin",
                                               "foo rpcterms  timer sleep 2000."]),
                                [{"NODETOOL_TIMEOUT", "5asdnkajef"}])),
    {error,1,"RPC to " ++ _Rest} = sh(filename:join([OutputDir, "foo", "bin",
                                                     "foo rpcterms timer sleep 2000."]),
                                      [{"NODETOOL_TIMEOUT", "500"}]).

builtin_wait_for_process_start_script_hook(Config) ->
    PrivDir = ?config(priv_dir, Config),
    LibDir1 = ?config(lib_dir, Config),

    DirName = rlx_test_utils:create_random_name("start-fail-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),

    rlx_test_utils:create_full_app(OutputDir, "goal_app", "0.0.1",
                                   [stdlib,kernel], []),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {generate_start_script, true},
                  {extended_start_script, true},
                  {extended_start_script_hooks, [
                                                 {post_start, [wait_for_vm_start,
                                                               {wait_for_process, goal_app_srv_signal}]}
                                                ]}
                 ],

    {ok, _State} = relx:build_release(foo, [{root_dir, OutputDir}, {lib_dirs, [OutputDir, LibDir1]},
                                            {output_dir, OutputDir} | RelxConfig]),

    %% now start/stop the release to make sure the script hooks are really getting
    %% executed
    %% get the current time, we'll measure how long it took for the node to
    %% start, it must be at least 3 seconds which is the time it takes the
    %% goal_app_srv to register the signal
    T1 = os:timestamp(),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo daemon"])),
    T2 = timer:now_diff(os:timestamp(), T1),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    ?assert((T2 div 1000) > 3000),
    ok.

start_sname_in_other_argsfile(Config) ->
    PrivDir = ?config(priv_dir, Config),
    LibDir1 = ?config(lib_dir, Config),

    DirName = rlx_test_utils:create_random_name("start-fail-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),

    rlx_test_utils:create_full_app(OutputDir, "goal_app", "0.0.1", [stdlib,kernel], []),

    VmArgs = filename:join([OutputDir, "vm.args"]),
    VmArgs2 = VmArgs ++ ".2",

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {vm_args, VmArgs},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ],

    rlx_file_utils:write(VmArgs, "-args_file " ++ VmArgs2 ++ "\n\n"
                         "-setcookie cookie\n"),

    rlx_file_utils:write(VmArgs2, "-sname foo@localhost\n"),


    {ok, _State} = relx:build_release(foo, [{root_dir, OutputDir}, {lib_dirs, [OutputDir, LibDir1]},
                                            {output_dir, OutputDir} | RelxConfig]),
    %% now start/stop the release to make sure the extended script is working
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo daemon"])),
    timer:sleep(?SLEEP_TIME),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    %% a ping should fail after stopping a node
    {error, 1, _} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])).

start_preserves_arguments(Config) ->
    PrivDir = ?config(priv_dir, Config),
    LibDir1 = ?config(lib_dir, Config),

    DirName = rlx_test_utils:create_random_name("start-fail-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),

    rlx_test_utils:create_full_app(OutputDir, "goal_app", "0.0.1", [stdlib,kernel], []),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ],

    {ok, _State} = relx:build_release(foo, [{root_dir, OutputDir}, {lib_dirs, [OutputDir, LibDir1]},
                                            {output_dir, OutputDir} | RelxConfig]),

    %% now start/stop the release to make sure the extended script is working
    %% and preserving the "tricky" argument that contains a string with a space
    %% in it
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo daemon -goal_app baz '\"bat zing\"'"])),
    timer:sleep(?SLEEP_TIME),
    BinFile = filename:join([PrivDir, "goal_app.bin"]),
    Eval = io_lib:format("{ok,Env}=application:get_env(goal_app,baz),file:write_file(\"~s\",term_to_binary(Env)).",
                         [BinFile]),
    Cmd = lists:flatten(io_lib:format("foo eval '~s'", [Eval])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", Cmd])),
    {ok, Bin} = file:read_file(BinFile),
    "bat zing" = binary_to_term(Bin),
    file:delete(BinFile),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    %% a ping should fail after stopping a node
    {error, 1, _} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])).

start_nodetool_with_data_from_argsfile(Config) ->
    PrivDir = ?config(priv_dir, Config),
    LibDir1 = ?config(lib_dir, Config),

    DirName = rlx_test_utils:create_random_name("start-fail-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),

    rlx_test_utils:create_full_app(OutputDir, "goal_app", "0.0.1", [stdlib,kernel], []),

    VmArgs = filename:join([OutputDir, "vm.args"]),
    rlx_file_utils:write(VmArgs, "-setcookie cookie\n"
                         "-sname foo@localhost\n\n"
                         "-proto_dist inet_tcp\n\n"),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {vm_args, VmArgs},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ],

    {ok, _State} = relx:build_release(foo, [{root_dir, OutputDir}, {lib_dirs, [OutputDir, LibDir1]},
                                            {output_dir, OutputDir} | RelxConfig]),

    %% now start/stop the release to make sure the extended script is working
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo daemon"])),
    timer:sleep(?SLEEP_TIME),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    %% a ping should fail after stopping a node
    {error, 1, _} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])).

start_upgrade_escript_with_argsfile_data(Config) ->
    PrivDir = ?config(priv_dir, Config),
    LibDir1 = ?config(lib_dir, Config),

    DirName = rlx_test_utils:create_random_name("start-fail-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),

    rlx_test_utils:create_full_app(OutputDir, "goal_app", "0.0.1", [stdlib,kernel], []),

    VmArgs = filename:join([OutputDir, "vm.args"]),
    rlx_file_utils:write(VmArgs, "-setcookie cookie\n"
                         "-sname foo@localhost\n\n"
                         "-proto_dist inet_tcp\n\n"),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1, sasl]},
                  {vm_args, VmArgs},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ],

    {ok, _State} = relx:build_release(foo, [{root_dir, OutputDir}, {lib_dirs, [OutputDir, LibDir1]},
                                            {output_dir, OutputDir} | RelxConfig]),

    %% now start/stop the release to make sure the extended script is working
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo daemon"])),
    timer:sleep(?SLEEP_TIME),
    {ok, _Ver} = sh(filename:join([OutputDir, "foo", "bin", "foo versions"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    %% a ping should fail after stopping a node
    {error, 1, _} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])).

start_fail_when_no_name(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DirName = rlx_test_utils:create_random_name("start-fail-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),

    VmArgs = filename:join([OutputDir, "vm-1.args"]),
    rlx_file_utils:write(VmArgs, "-setcookie cookie\n"),
    start_fail_with_vmargs(Config, VmArgs, 1).

start_fail_when_multiple_names(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DirName = rlx_test_utils:create_random_name("start-fail-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),

    VmArgs = filename:join([OutputDir, "vm-2.args"]),
    rlx_file_utils:write(VmArgs, "-name foo\n\n"
                         "-name bar\n\n"
                         "-setcookie cookie\n"),
    start_fail_with_vmargs([{out_dir, OutputDir} | Config], VmArgs, 2).

start_fail_when_missing_argsfile(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DirName = rlx_test_utils:create_random_name("start-fail-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),

    VmArgs = filename:join([OutputDir, "vm-3.args"]),
    rlx_file_utils:write(VmArgs, "-name foo\n\n"
                         "-args_file " ++ VmArgs ++ ".nonexistent\n\n"
                         "-setcookie cookie\n"),
    start_fail_with_vmargs([{out_dir, OutputDir} | Config], VmArgs, 3).

start_fail_when_relative_argsfile(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DirName = rlx_test_utils:create_random_name("start-fail-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),

    VmArgs = filename:join([OutputDir, "vm-5.args"]),
    rlx_file_utils:write(VmArgs, "-name foo\n\n"
                         "-args_file vm.args.relative\n\n"
                         "-setcookie cookie\n"),
    start_fail_with_vmargs([{out_dir, OutputDir} | Config], VmArgs, 4).

start_fail_when_circular_argsfiles(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DirName = rlx_test_utils:create_random_name("start-fail-testcase-output"),
    OutputDir = filename:join(PrivDir, DirName),
    ok = rlx_file_utils:mkdir_p(OutputDir),

    VmArgs = filename:join([OutputDir, "vm-6.args"]),
    VmArgs2 = VmArgs ++ ".2",
    VmArgs3 = VmArgs ++ ".3",
    rlx_file_utils:write(VmArgs, "-name foo\n\n"
                         "-args_file " ++ VmArgs2 ++ "\n\n"
                         "-setcookie cookie\n"),
    rlx_file_utils:write(VmArgs2, "-args_file " ++ VmArgs3 ++ "\n"),
    rlx_file_utils:write(VmArgs3, "-args_file " ++ VmArgs2 ++ "\n"),
    start_fail_with_vmargs([{out_dir, OutputDir} | Config], VmArgs, 5).

%%-------------------------------------------------------------------
%% Helper Function for start_fail_when_* tests
%%-------------------------------------------------------------------
start_fail_with_vmargs(Config, VmArgs, ExpectedCode) ->
    LibDir = ?config(lib_dir, Config),
    OutputDir = ?config(out_dir, Config),

    RelxConfig = [{release, {foo, "0.0.1"},
                   [goal_app_1]},

                  {vm_args, VmArgs},
                  {dev_mode, true},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ],

    {ok, _State} = relx:build_release(foo, [{root_dir, LibDir}, {lib_dirs, [LibDir]},
                                            {output_dir, OutputDir} | RelxConfig]),


    %% now start/stop the release to make sure the extended script is working
    ?assertMatch({error, ExpectedCode, _}, sh(filename:join([OutputDir, "foo", "bin", "foo daemon"]))).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

sh(Command) ->
    sh(Command, [{"RELX_REPLACE_OS_VARS", "1"},
                 {"NODENAME", "node1"},
                 {"COOKIE", "cookie1"},
                 {"VAR1", "v1"}]).

sh(Command, Env) ->
    sh(Command, Env, get_cwd()).

sh(Command, Env, Dir) ->
    Port = open_port({spawn, lists:flatten(Command)},
                     [{cd, Dir},
                      {env, Env},
                      exit_status,
                      {line, 16384},
                      use_stdio, stderr_to_stdout]),
    case sh_loop(Port) of
        {ok, Ret} ->
            {ok, Ret};
        {error, Rc, Msg} ->
            {error, Rc, Msg}
    end.

sh_loop(Port) ->
    sh_loop(Port, "").

sh_loop(Port, Acc) ->
    receive
        {Port, {data, {_, Line}}} ->
            sh_loop(Port, Acc ++ Line);
        {Port, {exit_status, 0}} ->
            {ok, Acc};
        {Port, {exit_status, Rc}} ->
            {error, Rc, Acc}
    end.

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.
