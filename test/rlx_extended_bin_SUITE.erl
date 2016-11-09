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

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         ping/1,
         attach/1,
         pid/1,
         restart/1,
         reboot/1,
         escript/1,
         remote_console/1,
         replace_os_vars/1,
         replace_os_vars_custom_location/1,
         replace_os_vars_dev_mode/1,
         replace_os_vars_twice/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

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
    [ping, attach, pid, restart, reboot, escript,
     remote_console,
     replace_os_vars, replace_os_vars_custom_location,
     replace_os_vars_dev_mode, replace_os_vars_twice].

ping(Config) ->
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
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    %% now start/stop the release to make sure the extended script is working
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    %% a ping should fail after stopping a node
    {error, 1, _} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])).

attach(Config) ->
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
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    %% now start/stop the release to make sure the extended script is working
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo attach", "&"])),
    timer:sleep(2000),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    {error, 1, _} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])).

pid(Config) ->
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
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    %% check for a valid pid
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    {ok, _Pid} = sh(filename:join([OutputDir, "foo", "bin", "foo pid"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    {error, 1, _} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])).

restart(Config) ->
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
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    %% a restart is a gracious operation that does not involve the
    %% death of the VM, so the pid should be the same
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    {ok, Pid1} = sh(filename:join([OutputDir, "foo", "bin", "foo pid"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo restart"])),
    timer:sleep(2000),
    {ok, Pid2} = sh(filename:join([OutputDir, "foo", "bin", "foo pid"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    {error, 1, _} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    ?assertEqual(Pid1, Pid2).

reboot(Config) ->
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
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    %% a reboot involves stopping the emulator, it needs to be restarted
    %% though
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    {ok, Pid1} = sh(filename:join([OutputDir, "foo", "bin", "foo pid"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo reboot"])),
    timer:sleep(2000),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, Pid2} = sh(filename:join([OutputDir, "foo", "bin", "foo pid"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    {error, 1, _} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    ?assertNotEqual(Pid1, Pid2).

escript(Config) ->
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
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    ok = ec_file:write(filename:join([OutputDir, "foo", "script.erl"]),
                       [rlx_test_utils:escript_contents()]),

    %% now start/stop the release to make sure the extended script is working
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    [ExpectedOutput] = io_lib:format("~s",
                            [filename:join([OutputDir, "foo"])]),
    {ok, Output} = sh(filename:join([OutputDir, "foo", "bin", "foo escript script.erl"])),
    ?assertEqual(ExpectedOutput, Output).

remote_console(Config) ->
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
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    %% now start/stop the release to make sure the extended script is working
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo remote_console &"])),
    timer:sleep(2000),
    {ok, NodesStr} = sh(filename:join([OutputDir, "foo", "bin", "foo eval 'nodes(connected).'"])),
    Nodes = rlx_test_utils:list_to_term(NodesStr),
    ?assertEqual(1, length(Nodes)),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])).

replace_os_vars(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    SysConfig = filename:join([LibDir1, "sys.config"]),
    VmArgs = filename:join([LibDir1, "vm.args"]),

    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {sys_config, SysConfig},
                  {vm_args, VmArgs},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ]),

    rlx_test_utils:write_config(SysConfig,
                                [[{goal_app, [{var1, "${VAR1}"}]}]]),
    ec_file:write(VmArgs, "-sname ${NODENAME}\n\n"
                          "-setcookie ${COOKIE}\n"),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    {ok, _State} = relx:do([{relname, foo},
                           {relvsn, "0.0.1"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"]),
                 [{"RELX_REPLACE_OS_VARS", "1"},
                  {"NODENAME", "node1"},
                  {"COOKIE", "cookie1"},
                  {"VAR1", "v1"}]),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"]),
                      [{"RELX_REPLACE_OS_VARS", "1"},
                         {"NODENAME", "node1"},
                         {"COOKIE", "cookie1"}]),
    {ok, "\"v1\""} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval '{ok, V} = application:get_env(goal_app, var1), V.'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"NODENAME", "node1"},
                         {"COOKIE", "cookie1"}]),
    {ok, "\"node1\""} = sh(filename:join([OutputDir, "foo", "bin",
                                     "foo eval '[Node,_] = re:split(atom_to_list(node()), \"@\"),binary_to_list(Node).'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"NODENAME", "node1"},
                         {"COOKIE", "cookie1"}]),
    {ok, "cookie1"} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'erlang:get_cookie().'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    {ok, Node1} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys." ++
                                          rlx_test_utils:unescape_string(Node1) ++
                                          ".config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                [{"RELX_REPLACE_OS_VARS", "1"},
                 {"NODENAME", "node1"},
                 {"COOKIE", "cookie1"}]),

    %% start the node again but this time with different env variables to replace
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"]),
                 [{"RELX_REPLACE_OS_VARS", "1"},
                  {"NODENAME", "node2"},
                  {"COOKIE", "cookie2"},
                  {"VAR1", "v2"}]),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"]),
                      [{"RELX_REPLACE_OS_VARS", "1"},
                         {"NODENAME", "node2"},
                         {"COOKIE", "cookie2"}]),
    {ok, "\"v2\""} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval '{ok, V} = application:get_env(goal_app, var1), V.'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"NODENAME", "node2"},
                         {"COOKIE", "cookie2"}]),
    {ok, "\"node2\""} = sh(filename:join([OutputDir, "foo", "bin",
                                     "foo eval '[Node,_] = re:split(atom_to_list(node()), \"@\"),binary_to_list(Node).'"]),
                          [{"RELX_REPLACE_OS_VARS", "1"},
                           {"NODENAME", "node2"},
                           {"COOKIE", "cookie2"}]),
    {ok, "cookie2"} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'erlang:get_cookie().'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    {ok, Node2} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys." ++
                                          rlx_test_utils:unescape_string(Node2) ++
                                          ".config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    ok.

replace_os_vars_custom_location(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    SysConfig = filename:join([LibDir1, "sys.config"]),
    VmArgs = filename:join([LibDir1, "vm.args"]),

    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {sys_config, SysConfig},
                  {vm_args, VmArgs},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ]),

    rlx_test_utils:write_config(SysConfig,
                                [[{goal_app, [{var1, "${VAR1}"}]}]]),
    ec_file:write(VmArgs, "-sname ${NODENAME}\n\n"
                          "-setcookie ${COOKIE}\n"),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    {ok, _State} = relx:do([{relname, foo},
                           {relvsn, "0.0.1"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"]),
                 [{"RELX_REPLACE_OS_VARS", "1"},
                  {"RELX_OUT_FILE_PATH", "/tmp"},
                  {"NODENAME", "node1"},
                  {"COOKIE", "cookie1"},
                  {"VAR1", "v1"}]),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"]),
                      [{"RELX_REPLACE_OS_VARS", "1"},
                       {"RELX_OUT_FILE_PATH", "/tmp"},
                       {"NODENAME", "node1"},
                       {"COOKIE", "cookie1"}]),
    {ok, "\"v1\""} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval '{ok, V} = application:get_env(goal_app, var1), V.'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"RELX_OUT_FILE_PATH", "/tmp"},
                         {"NODENAME", "node1"},
                         {"COOKIE", "cookie1"}]),
    {ok, "\"node1\""} = sh(filename:join([OutputDir, "foo", "bin",
                                     "foo eval '[Node,_] = re:split(atom_to_list(node()), \"@\"),binary_to_list(Node).'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"RELX_OUT_FILE_PATH", "/tmp"},
                         {"NODENAME", "node1"},
                         {"COOKIE", "cookie1"}]),
    {ok, "cookie1"} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'erlang:get_cookie().'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_OUT_FILE_PATH", "/tmp"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    {ok, Node1} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_OUT_FILE_PATH", "/tmp"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join(["/", "tmp",
                                          "sys." ++
                                          rlx_test_utils:unescape_string(Node1) ++
                                          ".config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                [{"RELX_REPLACE_OS_VARS", "1"},
                 {"RELX_OUT_FILE_PATH", "/tmp"},
                 {"NODENAME", "node1"},
                 {"COOKIE", "cookie1"}]),

    %% start the node again but this time with different env variables to replace
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"]),
                 [{"RELX_REPLACE_OS_VARS", "1"},
                  {"RELX_OUT_FILE_PATH", "/tmp"},
                  {"NODENAME", "node2"},
                  {"COOKIE", "cookie2"},
                  {"VAR1", "v2"}]),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"]),
                      [{"RELX_REPLACE_OS_VARS", "1"},
                       {"RELX_OUT_FILE_PATH", "/tmp"},
                       {"NODENAME", "node2"},
                       {"COOKIE", "cookie2"}]),
    {ok, "\"v2\""} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval '{ok, V} = application:get_env(goal_app, var1), V.'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"RELX_OUT_FILE_PATH", "/tmp"},
                         {"NODENAME", "node2"},
                         {"COOKIE", "cookie2"}]),
    {ok, "\"node2\""} = sh(filename:join([OutputDir, "foo", "bin",
                                     "foo eval '[Node,_] = re:split(atom_to_list(node()), \"@\"),binary_to_list(Node).'"]),
                          [{"RELX_REPLACE_OS_VARS", "1"},
                           {"RELX_OUT_FILE_PATH", "/tmp"},
                           {"NODENAME", "node2"},
                           {"COOKIE", "cookie2"}]),
    {ok, "cookie2"} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'erlang:get_cookie().'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_OUT_FILE_PATH", "/tmp"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    {ok, Node2} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_OUT_FILE_PATH", "/tmp"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join(["/", "tmp",
                                          "sys." ++
                                          rlx_test_utils:unescape_string(Node2) ++
                                          ".config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_OUT_FILE_PATH", "/tmp"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    ok.

replace_os_vars_twice(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    SysConfig = filename:join([LibDir1, "sys.config"]),
    VmArgs = filename:join([LibDir1, "vm.args"]),

    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {sys_config, SysConfig},
                  {vm_args, VmArgs},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ]),

    rlx_test_utils:write_config(SysConfig,
                                [[{goal_app, [{var1, "${VAR1}"}]}]]),
    ec_file:write(VmArgs, "-sname node\n\n"
                          "-setcookie cookie\n"),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    {ok, _State} = relx:do([{relname, foo},
                            {relvsn, "0.0.1"},
                            {goals, []},
                            {lib_dirs, [LibDir1]},
                            {log_level, 3},
                            {output_dir, OutputDir},
                            {config, ConfigFile}], ["release"]),

    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"]),
                 [{"RELX_REPLACE_OS_VARS", "1"},
                  {"VAR1", "v1"}]),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"]),
                      [{"RELX_REPLACE_OS_VARS", "1"}]),
    {ok, "\"v1\""} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval '{ok, V} = application:get_env(goal_app, var1), V.'"]),
                       [{"RELX_REPLACE_OS_VARS", "1"}]),
    {ok, "\"node\""} = sh(filename:join([OutputDir, "foo", "bin",
                                     "foo eval '[Node,_] = re:split(atom_to_list(node()), \"@\"),binary_to_list(Node).'"]),
                          [{"RELX_REPLACE_OS_VARS", "1"}]),
    {ok, "cookie"} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'erlang:get_cookie().'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"}]),
    {ok, Node1} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys." ++
                                          rlx_test_utils:unescape_string(Node1) ++
                                          ".config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                 [{"RELX_REPLACE_OS_VARS", "1"}]),

    %% start the node again but this time don't replace env variables
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    {ok, "\"${VAR1}\""} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval '{ok, V} = application:get_env(goal_app, var1), V.'"])),
    {ok, "\"node\""} = sh(filename:join([OutputDir, "foo", "bin",
                                     "foo eval '[Node,_] = re:split(atom_to_list(node()), \"@\"),binary_to_list(Node).'"])),
    {ok, "cookie"} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'erlang:get_cookie().'"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    ok.

replace_os_vars_dev_mode(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    SysConfig = filename:join([LibDir1, "sys.config"]),
    VmArgs = filename:join([LibDir1, "vm.args"]),

    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {sys_config, SysConfig},
                  {vm_args, VmArgs},
                  {dev_mode, true},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ]),

    rlx_test_utils:write_config(SysConfig,
                                [[{goal_app, [{var1, "${VAR1}"}]}]]),
    ec_file:write(VmArgs, "-sname ${NODENAME}\n\n"
                          "-setcookie ${COOKIE}\n"),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    {ok, _State} = relx:do([{relname, foo},
                           {relvsn, "0.0.1"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"]),
                 [{"RELX_REPLACE_OS_VARS", "1"},
                  {"NODENAME", "node1"},
                  {"COOKIE", "cookie1"},
                  {"VAR1", "v1"}]),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"]),
                      [{"RELX_REPLACE_OS_VARS", "1"},
                         {"NODENAME", "node1"},
                         {"COOKIE", "cookie1"}]),
    {ok, "\"v1\""} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval '{ok, V} = application:get_env(goal_app, var1), V.'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"NODENAME", "node1"},
                         {"COOKIE", "cookie1"}]),
    {ok, "\"node1\""} = sh(filename:join([OutputDir, "foo", "bin",
                                     "foo eval '[Node,_] = re:split(atom_to_list(node()), \"@\"),binary_to_list(Node).'"]),
                          [{"RELX_REPLACE_OS_VARS", "1"},
                           {"NODENAME", "node1"},
                           {"COOKIE", "cookie1"}]),
    {ok, "cookie1"} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'erlang:get_cookie().'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    {ok, Node1} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys." ++
                                          rlx_test_utils:unescape_string(Node1) ++
                                          ".config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                 [{"RELX_REPLACE_OS_VARS", "1"},
                  {"NODENAME", "node1"},
                  {"COOKIE", "cookie1"}]),

    %% start the node again but this time with different env variables to replace
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"]),
                 [{"RELX_REPLACE_OS_VARS", "1"},
                  {"NODENAME", "node2"},
                  {"COOKIE", "cookie2"},
                  {"VAR1", "v2"}]),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"]),
                      [{"RELX_REPLACE_OS_VARS", "1"},
                       {"NODENAME", "node2"},
                       {"COOKIE", "cookie2"}]),
    {ok, "\"v2\""} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval '{ok, V} = application:get_env(goal_app, var1), V.'"]),
                       [{"RELX_REPLACE_OS_VARS", "1"},
                        {"NODENAME", "node2"},
                        {"COOKIE", "cookie2"}]),
    {ok, "\"node2\""} = sh(filename:join([OutputDir, "foo", "bin",
                                     "foo eval '[Node,_] = re:split(atom_to_list(node()), \"@\"),binary_to_list(Node).'"]),
                          [{"RELX_REPLACE_OS_VARS", "1"},
                           {"NODENAME", "node2"},
                           {"COOKIE", "cookie2"}]),
    {ok, "cookie2"} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'erlang:get_cookie().'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"NODENAME", "node2"},
                         {"COOKIE", "cookie2"}]),
    {ok, Node2} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys." ++
                                          rlx_test_utils:unescape_string(Node2) ++
                                          ".config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                [{"RELX_REPLACE_OS_VARS", "1"},
                 {"NODENAME", "node2"},
                 {"COOKIE", "cookie2"}]),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

sh(Command) ->
    sh(Command, []).

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
