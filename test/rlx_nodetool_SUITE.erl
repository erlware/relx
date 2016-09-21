%%% @author Vipin Nair <swvist@gmail.com>
%%% @copyright (C) 2016, Vipin Nair
-module(rlx_nodetool_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(RUN(Nodetool, Cmd), strip(os:cmd(Nodetool ++ " " ++ Cmd))).


all() ->
    [start_stop, {group, running_node}].

groups() ->
    [{running_node, [], [rpc, rpcterms]}].

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    net_kernel:start([fqdn(ct), shortnames]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(start_stop, Config) ->
    {AppName, AppNode, AppNodeTool} = setup(Config),
    [{app_name, AppName},
     {app_node, AppNode},
     {app_nodetool, AppNodeTool} | Config];

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

init_per_group(running_node, Config) ->
    {AppName, AppNode, AppNodeTool} = setup(Config),

    %% Start Node
    ok = start_node(AppNodeTool, AppNode),
    [{app_name, AppName},
     {app_node, AppNode},
     {app_nodetool, AppNodeTool} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

start_stop(Config) ->
    AppNode = ?config(app_node, Config),
    Nodetool = ?config(app_nodetool, Config),

    ?RUN(Nodetool, "start"),
    timer:sleep(1000),
    ?assertEqual(pong, net_adm:ping(AppNode)),

    ?RUN(Nodetool, "stop"),
    ?assertEqual(pang, net_adm:ping(AppNode)),

    ok.

rpc(Config) ->
    %% AppNode = ?config(app_node, Config),
    Nodetool = ?config(app_nodetool, Config),

    Result1 = ?RUN(Nodetool,"rpc erlang length abc"),
    ?assertEqual(3, list_to_integer(Result1)),

    Result2 = ?RUN(Nodetool,"rpc string concat abc def"),
    ?assertEqual("\"abcdef\"", Result2),

    ok.

rpcterms(Config) ->
    AppNode = ?config(app_node, Config),
    Nodetool = ?config(app_nodetool, Config),

    ?assertEqual(AppNode, list_to_atom(?RUN(Nodetool, "rpcterms erlang node"))),

    Result = ?RUN(Nodetool,"rpcterms erlang length [1, 2, 3, 4]."),
    ?assertEqual(4, list_to_integer(Result)),

    ok.


%%%===================================================================
%%% Helper Functions
%%%===================================================================


setup(Config) ->
    AppName = list_to_atom(rlx_test_utils:create_random_name("app_")),
    erlang:set_cookie(node(), AppName),

    PrivDir = proplists:get_value(priv_dir, Config),
    OutputDir = filename:join([PrivDir, rlx_test_utils:create_random_name("relx-output")]),
    ok = rlx_util:mkdir_p(OutputDir),

    ConfigFile = filename:join([OutputDir, "relx.config"]),
    rlx_test_utils:write_config(ConfigFile, [{release, {AppName, "0.0.1"},
                                              [stdlib, kernel, sasl]},
                                             {extended_start_script, true}]),

    {ok, State} = relx:do(undefined, undefined, [], [OutputDir], 3, OutputDir, ConfigFile),
    [{{AppName, "0.0.1"}, Release}] = ec_dictionary:to_list(rlx_state:realized_releases(State)),

    rlx_release:applications(Release),

    AppNode = fqdn(AppName),

    AppNodeTool = filename:join([OutputDir, atom_to_list(AppName), "bin",
                                 atom_to_list(AppName)]),

    {AppName, AppNode, AppNodeTool}.


fqdn(Name) when is_atom(Name) ->
    {ok, HostName} = inet:gethostname(),
    list_to_atom(atom_to_list(Name) ++ "@" ++ HostName).

strip(Output) ->
    string:strip(Output, right, $\n).

start_node(Nodetool, AppNode) ->
    ?RUN(Nodetool, "start"),
    timer:sleep(1000),
    ?assertEqual(pong, net_adm:ping(AppNode)),
    ok.
