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
         start_sname_in_other_argsfile/1,
         start_preserves_arguments/1,
         start_fail_when_no_name/1,
         start_fail_when_multiple_names/1,
         start_fail_when_missing_argsfile/1,
         start_fail_when_nonreadable_argsfile/1,
         start_fail_when_relative_argsfile/1,
         start_fail_when_circular_argsfiles/1,
         ping/1,
         shortname_ping/1,
         longname_ping/1,
         attach/1,
         pid/1,
         restart/1,
         reboot/1,
         escript/1,
         remote_console/1,
         replace_os_vars/1,
         replace_os_vars_multi_node/1,
         replace_os_vars_included_config/1,
         replace_os_vars_custom_location/1,
         replace_os_vars_dev_mode/1,
         replace_os_vars_twice/1,
         custom_start_script_hooks/1,
         builtin_wait_for_vm_start_script_hook/1,
         builtin_pid_start_script_hook/1,
         builtin_wait_for_process_start_script_hook/1,
         mixed_custom_and_builtin_start_script_hooks/1,
         builtin_status_script/1, custom_status_script/1,
         extension_script/1]).

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
    [start_sname_in_other_argsfile, start_preserves_arguments,
     start_fail_when_no_name, start_fail_when_multiple_names,
     start_fail_when_missing_argsfile, start_fail_when_nonreadable_argsfile,
     start_fail_when_relative_argsfile, start_fail_when_circular_argsfiles,
     ping, shortname_ping, longname_ping, attach, pid, restart, reboot, escript,
     remote_console, replace_os_vars, replace_os_vars_multi_node, replace_os_vars_included_config,
     replace_os_vars_custom_location, replace_os_vars_dev_mode, replace_os_vars_twice, custom_start_script_hooks,
     builtin_wait_for_vm_start_script_hook, builtin_pid_start_script_hook,
     builtin_wait_for_process_start_script_hook, mixed_custom_and_builtin_start_script_hooks,
     builtin_status_script, custom_status_script, extension_script].

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

shortname_ping(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    VmArgs = filename:join([LibDir1, "vm.args"]),

    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {vm_args, VmArgs},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ]),
    
    ec_file:write(VmArgs, "-sname foo\n\n"
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

    %% now start/stop the release to make sure the extended script is working
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    %% a ping should fail after stopping a node
    {error, 1, _} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])).

longname_ping(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    VmArgs = filename:join([LibDir1, "vm.args"]),

    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {vm_args, VmArgs},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ]),
    
    ec_file:write(VmArgs, "-name foo\n\n"
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
                                [[{goal_app,
                                   [{var1, "${VAR1}"},
                                    {var2, "${VAR2}"}]}]]),
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
    {ok, _Node1} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys.config"]))),
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
    {ok, _Node2} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys.config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    ok.

replace_os_vars_multi_node(Config) ->
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
                  {"RELX_MULTI_NODE", "1"},
                  {"NODENAME", "node1"},
                  {"COOKIE", "cookie1"},
                  {"VAR1", "v1"}]),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"]),
                      [{"RELX_REPLACE_OS_VARS", "1"},
                       {"RELX_MULTI_NODE", "1"},
                       {"NODENAME", "node1"},
                       {"COOKIE", "cookie1"}]),
    {ok, "\"v1\""} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval '{ok, V} = application:get_env(goal_app, var1), V.'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"RELX_MULTI_NODE", "1"},
                         {"NODENAME", "node1"},
                         {"COOKIE", "cookie1"}]),
    {ok, "\"node1\""} = sh(filename:join([OutputDir, "foo", "bin",
                                     "foo eval '[Node,_] = re:split(atom_to_list(node()), \"@\"),binary_to_list(Node).'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"RELX_MULTI_NODE", "1"},
                         {"NODENAME", "node1"},
                         {"COOKIE", "cookie1"}]),
    {ok, "cookie1"} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'erlang:get_cookie().'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_MULTI_NODE", "1"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    {ok, Node1} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_MULTI_NODE", "1"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys." ++
                                          rlx_test_utils:unescape_string(Node1) ++
                                          ".config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                [{"RELX_REPLACE_OS_VARS", "1"},
                 {"RELX_MULTI_NODE", "1"},
                 {"NODENAME", "node1"},
                 {"COOKIE", "cookie1"}]),

    %% start the node again but this time with different env variables to replace
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"]),
                 [{"RELX_REPLACE_OS_VARS", "1"},
                  {"RELX_MULTI_NODE", "1"},
                  {"NODENAME", "node2"},
                  {"COOKIE", "cookie2"},
                  {"VAR1", "v2"}]),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"]),
                      [{"RELX_REPLACE_OS_VARS", "1"},
                       {"RELX_MULTI_NODE", "1"},
                       {"NODENAME", "node2"},
                       {"COOKIE", "cookie2"}]),
    {ok, "\"v2\""} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval '{ok, V} = application:get_env(goal_app, var1), V.'"]),
                        [{"RELX_REPLACE_OS_VARS", "1"},
                         {"RELX_MULTI_NODE", "1"},
                         {"NODENAME", "node2"},
                         {"COOKIE", "cookie2"}]),
    {ok, "\"node2\""} = sh(filename:join([OutputDir, "foo", "bin",
                                     "foo eval '[Node,_] = re:split(atom_to_list(node()), \"@\"),binary_to_list(Node).'"]),
                          [{"RELX_REPLACE_OS_VARS", "1"},
                           {"RELX_MULTI_NODE", "1"},
                           {"NODENAME", "node2"},
                           {"COOKIE", "cookie2"}]),
    {ok, "cookie2"} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'erlang:get_cookie().'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_MULTI_NODE", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    {ok, Node2} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_MULTI_NODE", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys." ++
                                          rlx_test_utils:unescape_string(Node2) ++
                                          ".config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_MULTI_NODE", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    ok.

replace_os_vars_included_config(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    SysConfig = filename:join([LibDir1, "sys.config"]),
    IncludedConfig = filename:join([LibDir1, "config", "included.config"]),
    VmArgs = filename:join([LibDir1, "vm.args"]),

    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {sys_config, SysConfig},
                  {vm_args, VmArgs},
                  {generate_start_script, true},
                  {extended_start_script, true},
                  {overlay, [
                    {mkdir, "releases/{{release_version}}/config"},
                    {template, "config/included.config", "releases/{{release_version}}/config/included.config"}
                  ]}
                 ]),

    rlx_test_utils:write_config(IncludedConfig,
                                [[{goal_app, [
                                              {var1_included, "${VAR1}"}]
                                  }]
                                ]),
    rlx_test_utils:write_config(SysConfig,
                                [[{goal_app, [
                                              {var1, "${VAR1}"}]
                                  },
                                 "releases/0.0.1/config/included.config"]
                                ]),
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
    {ok, _Node1} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys.config"]))),
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
    {ok, _Node2} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys.config"]))),
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
    {ok, _Node1} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_OUT_FILE_PATH", "/tmp"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join(["/", "tmp",
                                          "sys.config"]))),
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
    {ok, _Node2} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"RELX_OUT_FILE_PATH", "/tmp"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join(["/", "tmp",
                                          "sys.config"]))),
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
    {ok, _Node1} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys.config"]))),
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
    {ok, _Node1} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node1"},
                          {"COOKIE", "cookie1"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys.config"]))),
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
    {ok, _Node2} = sh(filename:join([OutputDir, "foo", "bin",
                                       "foo eval 'atom_to_list(node()).'"]),
                         [{"RELX_REPLACE_OS_VARS", "1"},
                          {"NODENAME", "node2"},
                          {"COOKIE", "cookie2"}]),
    %% check that the replaced files have been created in the right place
    ?assert(ec_file:exists(filename:join([OutputDir, "foo", "releases", "0.0.1",
                                          "sys.config"]))),
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo stop"]),
                [{"RELX_REPLACE_OS_VARS", "1"},
                 {"NODENAME", "node2"},
                 {"COOKIE", "cookie2"}]),
    ok.

custom_start_script_hooks(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {generate_start_script, true},
                  {extended_start_script, true},
                  {extended_start_script_hooks, [
                        {pre_start, [
                          {custom, "hooks/pre_start"}
                        ]},
                        {post_start, [
                          {custom, "hooks/post_start"}
                        ]},
                        {pre_stop, [
                          {custom, "hooks/pre_stop"}
                        ]},
                        {post_stop, [
                          {custom, "hooks/post_stop"}
                        ]}
                    ]},
                  {mkdir, "scripts"},
                  {overlay, [{copy, "./pre_start", "bin/hooks/pre_start"},
                             {copy, "./post_start", "bin/hooks/post_start"},
                             {copy, "./pre_stop", "bin/hooks/pre_stop"},
                             {copy, "./post_stop", "bin/hooks/post_stop"}]}
                 ]),

    %% write the hook scripts, each of them will write an erlang term to a file
    %% that will later be consulted
    ok = file:write_file(filename:join([LibDir1, "./pre_start"]),
                         "#!/bin/bash\n# $*\necho \\{pre_start, $REL_NAME, \\'$NAME\\', $COOKIE\\}. >> test"),
    ok = file:write_file(filename:join([LibDir1, "./post_start"]),
                         "#!/bin/bash\n# $*\necho \\{post_start, $REL_NAME, \\'$NAME\\', $COOKIE\\}. >> test"),
    ok = file:write_file(filename:join([LibDir1, "./pre_stop"]),
                         "#!/bin/bash\n# $*\necho \\{pre_stop, $REL_NAME, \\'$NAME\\', $COOKIE\\}. >> test"),
    ok = file:write_file(filename:join([LibDir1, "./post_stop"]),
                         "#!/bin/bash\n# $*\necho \\{post_stop, $REL_NAME, \\'$NAME\\', $COOKIE\\}. >> test"),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, _State} = relx:do(foo, undefined, [], [LibDir1], 3,
                           OutputDir, ConfigFile),
    %% now start/stop the release to make sure the script hooks are really getting
    %% executed
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    %% now check that the output file contains the expected format
    {ok,[{pre_start, foo, _, foo},
         {post_start, foo, _, foo},
         {pre_stop, foo, _, foo},
         {post_stop, foo, _, foo}]} = file:consult(filename:join([OutputDir, "foo", "test"])).

builtin_pid_start_script_hook(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {generate_start_script, true},
                  {extended_start_script, true},
                  {extended_start_script_hooks, [
                      {post_start, [
                        {pid, filename:join([OutputDir, "foo.pid"])}
                      ]}
                  ]}
                 ]),

    {ok, _State} = relx:do(foo, undefined, [], [LibDir1], 3,
                           OutputDir, ConfigFile),
    %% now start/stop the release to make sure the script hooks are really getting
    %% executed
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo start"])),
    %% check that the pid file really was created
    ?assert(ec_file:exists(filename:join([OutputDir, "foo.pid"]))),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    ok.

builtin_wait_for_vm_start_script_hook(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {generate_start_script, true},
                  {extended_start_script, true},
                  {extended_start_script_hooks, [
                        {post_start, [wait_for_vm_start]}
                    ]}
                 ]),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, _State} = relx:do(foo, undefined, [], [LibDir1], 3,
                           OutputDir, ConfigFile),
    %% now start/stop the release to make sure the script hooks are really getting
    %% executed
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo start"])),
    % this run doesn't need the sleep because the wait_for_vm_start
    % start script makes it unnecessary
    %timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    ok.

builtin_wait_for_process_start_script_hook(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_full_app(LibDir1, "goal_app", "0.0.1",
                                   [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {generate_start_script, true},
                  {extended_start_script, true},
                  {extended_start_script_hooks, [
                        {post_start, [wait_for_vm_start,
                                     {wait_for_process, goal_app_srv_signal}]}
                    ]}
                 ]),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),
    {ok, _State} = relx:do(foo, undefined, [], [LibDir1], 3,
                           OutputDir, ConfigFile),
    %% now start/stop the release to make sure the script hooks are really getting
    %% executed
    %% get the current time, we'll measure how long it took for the node to
    %% start, it must be at least 3 seconds which is the time it takes the
    %% goal_app_srv to register the signal
    T1 = os:timestamp(),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo start"])),
    T2 = timer:now_diff(os:timestamp(), T1),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    ?assert((T2 div 1000) > 3000),
    ok.

mixed_custom_and_builtin_start_script_hooks(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_full_app(LibDir1, "goal_app", "0.0.1",
                                   [stdlib,kernel], []),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {generate_start_script, true},
                  {extended_start_script, true},
                  {extended_start_script_hooks, [
                        {pre_start, [
                          {custom, "hooks/pre_start"}
                        ]},
                        {post_start, [
                          wait_for_vm_start,
                          {pid, filename:join([OutputDir, "foo.pid"])},
                          {wait_for_process, goal_app_srv_signal},
                          {custom, "hooks/post_start"}
                        ]},
                        {pre_stop, [
                          {custom, "hooks/pre_stop"}
                        ]},
                        {post_stop, [
                          {custom, "hooks/post_stop"}
                        ]}
                    ]},
                  {mkdir, "scripts"},
                  {overlay, [{copy, "./pre_start", "bin/hooks/pre_start"},
                             {copy, "./post_start", "bin/hooks/post_start"},
                             {copy, "./pre_stop", "bin/hooks/pre_stop"},
                             {copy, "./post_stop", "bin/hooks/post_stop"}]}
                 ]),

    %% write the hook scripts, each of them will write an erlang term to a file
    %% that will later be consulted
    ok = file:write_file(filename:join([LibDir1, "./pre_start"]),
                         "#!/bin/bash\n# $*\necho \\{pre_start, $REL_NAME, \\'$NAME\\', $COOKIE\\}. >> test"),
    ok = file:write_file(filename:join([LibDir1, "./post_start"]),
                         "#!/bin/bash\n# $*\necho \\{post_start, $REL_NAME, \\'$NAME\\', $COOKIE\\}. >> test"),
    ok = file:write_file(filename:join([LibDir1, "./pre_stop"]),
                         "#!/bin/bash\n# $*\necho \\{pre_stop, $REL_NAME, \\'$NAME\\', $COOKIE\\}. >> test"),
    ok = file:write_file(filename:join([LibDir1, "./post_stop"]),
                         "#!/bin/bash\n# $*\necho \\{post_stop, $REL_NAME, \\'$NAME\\', $COOKIE\\}. >> test"),

    {ok, _State} = relx:do(foo, undefined, [], [LibDir1], 3,
                           OutputDir, ConfigFile),
    %% now start/stop the release to make sure the script hooks are really getting
    %% executed
    %% get the current time, we'll measure how long it took for the node to
    %% start, it must be at least 3 seconds which is the time it takes the
    %% goal_app_srv to register the signal
    T1 = os:timestamp(),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo start"])),
    % this run doesn't need the sleep because the wait_for_vm_start
    % start script makes it unnecessary
    T2 = timer:now_diff(os:timestamp(), T1),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    ?assert((T2 div 1000) > 3000),
    %% check that the pid file really was created
    ?assert(ec_file:exists(filename:join([OutputDir, "foo.pid"]))),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    %% now check that the output file contains the expected format
    {ok,[{pre_start, foo, _, foo},
         {post_start, foo, _, foo},
         {pre_stop, foo, _, foo},
         {post_stop, foo, _, foo}]} = file:consult(filename:join([OutputDir, "foo", "test"])).

builtin_status_script(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_full_app(LibDir1, "goal_app", "0.0.1",
                                   [stdlib,kernel], []),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ]),

    {ok, _State} = relx:do(foo, undefined, [], [LibDir1], 3,
                           OutputDir, ConfigFile),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    %% write the status to a file
    {ok, ""} = sh(filename:join([OutputDir, "foo", "bin", "foo status"])).

custom_status_script(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_full_app(LibDir1, "goal_app", "0.0.1",
                                   [stdlib,kernel], []),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {generate_start_script, true},
                  {extended_start_script, true},
                  {extended_start_script_hooks, [
                        {status, [
                          {custom, "hooks/status"}
                        ]}
                    ]},
                  {overlay, [
                    {copy, "./status", "bin/hooks/status"}]}
                 ]),

    %% write the hook status script
    ok = file:write_file(filename:join([LibDir1, "./status"]),
                         "#!/bin/bash\n# $*\necho \\{status, $REL_NAME, \\'$NAME\\', $COOKIE\\}."),

    {ok, _State} = relx:do(foo, undefined, [], [LibDir1], 3,
                           OutputDir, ConfigFile),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    %% write the status to a file
    {ok, StatusStr} = sh(filename:join([OutputDir, "foo", "bin", "foo status"])),
    ec_file:write(filename:join([OutputDir, "status.txt"]), StatusStr),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    {ok, [Status]} = file:consult(filename:join([OutputDir, "status.txt"])),
    {ok, {status, foo, _, foo} = Status}.

start_sname_in_other_argsfile(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    VmArgs = filename:join([LibDir1, "vm.args"]),
    VmArgs2 = VmArgs ++ ".2",

    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {vm_args, VmArgs},
                  {generate_start_script, true},
                  {extended_start_script, true}
                 ]),

    ec_file:write(VmArgs, "-args_file " ++ VmArgs2 ++ "\n\n"
                          "-setcookie cookie\n"),

    ec_file:write(VmArgs2, "-sname foo\n"),

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

start_preserves_arguments(Config) ->
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

    PrivDir = proplists:get_value(priv_dir, Config),
    OutputDir = filename:join([PrivDir, rlx_test_utils:create_random_name("relx-output")]),

    {ok, _State} = relx:do([{relname, foo},
                           {relvsn, "0.0.1"},
                           {goals, []},
                           {lib_dirs, [LibDir1]},
                           {log_level, 3},
                           {output_dir, OutputDir},
                           {config, ConfigFile}], ["release"]),

    %% now start/stop the release to make sure the extended script is working
    %% and preserving the "tricky" argument that contains a string with a space
    %% in it
    {ok, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start -goal_app baz '\"bat zing\"'"])),
    timer:sleep(2000),
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

start_fail_when_no_name(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    VmArgs = filename:join([LibDir1, "vm.args"]),
    ec_file:write(VmArgs, "-setcookie cookie\n"),
    start_fail_with_vmargs(Config, VmArgs, 1).

start_fail_when_multiple_names(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    VmArgs = filename:join([LibDir1, "vm.args"]),
    ec_file:write(VmArgs, "-name foo\n\n"
                          "-name bar\n\n"
                          "-setcookie cookie\n"),
    start_fail_with_vmargs(Config, VmArgs, 2).

start_fail_when_missing_argsfile(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    VmArgs = filename:join([LibDir1, "vm.args"]),
    ec_file:write(VmArgs, "-name foo\n\n"
                          "-args_file " ++ VmArgs ++ ".nonexistent\n\n"
                          "-setcookie cookie\n"),
    start_fail_with_vmargs(Config, VmArgs, 3).

start_fail_when_nonreadable_argsfile(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    VmArgs = filename:join([LibDir1, "vm.args"]),
    VmArgs2 = VmArgs ++ ".nonreadable",
    ec_file:write(VmArgs, "-name foo\n\n"
                          "-args_file " ++ VmArgs2 ++ "\n\n"
                          "-setcookie cookie\n"),
    ec_file:write(VmArgs2, ""),
    file:change_mode(VmArgs2, 8#00333),
    start_fail_with_vmargs(Config, VmArgs, 3).

start_fail_when_relative_argsfile(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    VmArgs = filename:join([LibDir1, "vm.args"]),
    ec_file:write(VmArgs, "-name foo\n\n"
                          "-args_file vm.args.relative\n\n"
                          "-setcookie cookie\n"),
    start_fail_with_vmargs(Config, VmArgs, 4).

start_fail_when_circular_argsfiles(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    VmArgs = filename:join([LibDir1, "vm.args"]),
    VmArgs2 = VmArgs ++ ".2",
    VmArgs3 = VmArgs ++ ".3",
    ec_file:write(VmArgs, "-name foo\n\n"
                          "-args_file " ++ VmArgs2 ++ "\n\n"
                          "-setcookie cookie\n"),
    ec_file:write(VmArgs2, "-args_file " ++ VmArgs3 ++ "\n"),
    ec_file:write(VmArgs3, "-args_file " ++ VmArgs2 ++ "\n"),
    start_fail_with_vmargs(Config, VmArgs, 5).

extension_script(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_full_app(LibDir1, "goal_app", "0.0.1",
                                   [stdlib,kernel], []),

    OutputDir = filename:join([proplists:get_value(priv_dir, Config),
                               rlx_test_utils:create_random_name("relx-output")]),

    ConfigFile = filename:join([LibDir1, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {generate_start_script, true},
                  {extended_start_script, true},
                  {extended_start_script_extensions, [
                        {bar, "extensions/bar"} 
                  ]},
                  {overlay, [
                    {copy, "./bar", "bin/extensions/bar"}]}
                 ]),

    %% write the extension script
    ok = file:write_file(filename:join([LibDir1, "./bar"]),
                         "#!/bin/bash\n"
                         "echo \\{bar, $REL_NAME, \\'$NAME\\', $COOKIE\\}.\n"
                         "exit 0"),

    {ok, _State} = relx:do(foo, undefined, [], [LibDir1], 3,
                           OutputDir, ConfigFile),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo start"])),
    timer:sleep(2000),
    {ok, "pong"} = sh(filename:join([OutputDir, "foo", "bin", "foo ping"])),
    %% write the extension script output to a file
    {ok, Str} = sh(filename:join([OutputDir, "foo", "bin", "foo bar"])),
    ec_file:write(filename:join([OutputDir, "bar.txt"]), Str),
    os:cmd(filename:join([OutputDir, "foo", "bin", "foo stop"])),
    {ok, [Term]} = file:consult(filename:join([OutputDir, "bar.txt"])),
    {ok, {bar, foo, _, foo} = Term}.

%%-------------------------------------------------------------------
%% Helper Function for start_fail_when_* tests
%%-------------------------------------------------------------------
start_fail_with_vmargs(Config, VmArgs, ExpectedCode) ->
    LibDir1 = proplists:get_value(lib1, Config),

    rlx_test_utils:create_app(LibDir1, "goal_app", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relx.config"]),

    rlx_test_utils:write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app]},
                  {lib_dirs, [filename:join(LibDir1, "*")]},
                  {vm_args, VmArgs},
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
    {error, ExpectedCode, _} = sh(filename:join([OutputDir, "foo", "bin", "foo start"])).

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
