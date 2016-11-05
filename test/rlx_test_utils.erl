%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2015, Tristan Sloughter
-module(rlx_test_utils).

-compile(export_all).

create_app(Dir, Name, Vsn, Deps, LibDeps) ->
    AppDir = filename:join([Dir, Name ++ "-" ++ Vsn]),
    write_app_file(AppDir, Name, Vsn, Deps, LibDeps),
    write_src_file(AppDir, Name),
    write_beam_file(AppDir, Name),
    rlx_app_info:new(erlang:list_to_atom(Name), Vsn, AppDir,
                     Deps, []).

create_full_app(Dir, Name, Vsn, Deps, LibDeps) ->
    AppDir = filename:join([Dir, Name ++ "-" ++ Vsn]),
    write_full_app_files(AppDir, Name, Vsn, Deps, LibDeps),
    compile_src_files(AppDir),
    rlx_app_info:new(erlang:list_to_atom(Name), Vsn, AppDir,
                     Deps, []).

create_empty_app(Dir, Name, Vsn, Deps, LibDeps) ->
    AppDir = filename:join([Dir, Name ++ "-" ++ Vsn]),
    write_app_file(AppDir, Name, Vsn, Deps, LibDeps),
    rlx_app_info:new(erlang:list_to_atom(Name), Vsn, AppDir,
                     Deps, []).

write_beam_file(Dir, Name) ->
    Beam = filename:join([Dir, "ebin", "not_a_real_beam" ++ Name ++ ".beam"]),
    ok = filelib:ensure_dir(Beam),
    ok = ec_file:write_term(Beam, testing_purposes_only).

write_src_file(Dir, Name) ->
    Src = filename:join([Dir, "src", "not_a_real_beam" ++ Name ++ ".erl"]),
    ok = filelib:ensure_dir(Src),
    ok = ec_file:write_term(Src, testing_purposes_only).

write_appup_file(AppInfo, DownVsn) ->
    Dir = rlx_app_info:dir(AppInfo),
    Name = rlx_util:to_string(rlx_app_info:name(AppInfo)),
    Vsn = rlx_app_info:vsn_as_string(AppInfo),
    Filename = filename:join([Dir, "ebin", Name ++ ".appup"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, {Vsn, [{DownVsn, []}], [{DownVsn, []}]}).

write_app_file(Dir, Name, Version, Deps, LibDeps) ->
    Filename = filename:join([Dir, "ebin", Name ++ ".app"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(Name, Version, Deps, LibDeps)).

get_app_metadata(Name, Vsn, Deps, LibDeps) ->
    {application, erlang:list_to_atom(Name),
     [{description, ""},
      {vsn, Vsn},
      {modules, []},
      {included_applications, LibDeps},
      {registered, []},
      {applications, Deps}]}.

write_full_app_files(Dir, Name, Vsn, Deps, LibDeps) ->
    %% write out the .app file
    AppFilename = filename:join([Dir, "ebin", Name ++ ".app"]),
    ok = filelib:ensure_dir(AppFilename),
    ok = ec_file:write_term(AppFilename,
                            get_full_app_metadata(Name, Vsn, Deps, LibDeps)),
    %% write out the _app.erl file
    ApplicationFilename = filename:join([Dir, "src", Name ++ "_app.erl"]),
    ok = filelib:ensure_dir(ApplicationFilename),
    ok = file:write_file(ApplicationFilename, full_application_contents(Name)),
    %% write out the supervisor
    SupervisorFilename = filename:join([Dir, "src", Name ++ "_sup.erl"]),
    ok = filelib:ensure_dir(SupervisorFilename),
    ok = file:write_file(SupervisorFilename, supervisor_contents(Name)),
    %% and finally the gen_server
    GenServerFilename = filename:join([Dir, "src", Name ++ "_srv.erl"]),
    ok = filelib:ensure_dir(GenServerFilename),
    ok = file:write_file(GenServerFilename, gen_server_contents(Name)),
    ok.

compile_src_files(Dir) ->
    %% compile all *.erl files in src to ebin
    SrcDir = filename:join([Dir, "src"]),
    OutputDir = filename:join([Dir, "ebin"]),
    lists:foreach(fun(SrcFile) ->
                    {ok, _} = compile:file(SrcFile, [{outdir, OutputDir},
                                                     return_errors])
                  end, ec_file:find(SrcDir, "\\.erl")),
    ok.

get_full_app_metadata(Name, Vsn, Deps, LibDeps) ->
    {application, erlang:list_to_atom(Name),
    [{description, ""},
     {vsn, Vsn},
     {modules, [goal_app_app,goal_app_sup,goal_app_srv]},
     {mod, {erlang:list_to_atom(Name ++  "_app"),
            []}},
     {included_applications, LibDeps},
     {registered, []},
     {applications, Deps}]}.

full_application_contents(Name) ->
    "-module("++Name++"_app).\n"
    "-behaviour(application).\n"
    "-export([start/2, stop/1]).\n"
    "start(_StartType, _StartArgs) ->\n"
    "   "++Name++"_sup:start_link().\n"
    "stop(_State) ->\n"
    "   ok.\n".

supervisor_contents(Name) ->
    "-module("++Name++"_sup).\n"
    "-behaviour(supervisor).\n"
    "-export([start_link/0]).\n"
    "-export([init/1]).\n"
    "-define(SERVER, ?MODULE).\n"
    "start_link() ->\n"
    "    supervisor:start_link({local, ?SERVER}, ?MODULE, []).\n"
    "init([]) ->\n"
    "    {ok, { {one_for_all, 0, 1},\n"
    "            [{"++Name++"_srv, {"++Name++"_srv, start_link, []},\n"
    "              transient, 5000, worker, ["++Name++"_srv]}\n"
    "            ]\n"
    "         }}.\n".

gen_server_contents(Name) ->
    "-module("++Name++"_srv).\n"
    "-behaviour(gen_server).\n"
    "-record(state, {}).\n"
    "-export([start_link/0]).\n"
    "-export([init/1,handle_call/3,handle_cast/2,\n"
    "         handle_info/2,terminate/2,code_change/3]).\n"
    "start_link() ->\n"
    "    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).\n"
    "init([]) ->\n"
    "    erlang:send_after(4000, self(), register_signal),"
    "    {ok, #state{}}.\n"
    "handle_call(_Event, _From, State) ->\n"
    "    {reply, ok, State}.\n"
    "handle_cast(_Event, State) ->\n"
    "    {noreply, State}.\n"
    "handle_info(register_signal, State) ->\n"
    "   erlang:register(goal_app_srv_signal, spawn(fun() -> timer:sleep(200000) end)),\n"
    "   {noreply, State};\n"
    "handle_info(_Info, State) ->\n"
    "    {noreply, State}.\n"
    "terminate(_Reason, _State) ->\n"
    "    ok.\n"
    "code_change(_OldVsn, State, _Extra) ->\n"
    "    {ok, State}.\n".

create_random_name(Name) ->
    Name ++ erlang:integer_to_list(random_uniform(1000000)).

create_random_vsn() ->
    lists:flatten([erlang:integer_to_list(random_uniform(100)),
                   ".", erlang:integer_to_list(random_uniform(100)),
                   ".", erlang:integer_to_list(random_uniform(100))]).

write_config(Filename, Values) ->
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write(Filename,
                       [io_lib:format("~p.\n", [Val]) || Val <- Values]).

test_template_contents() ->
    "{erts_vsn, \"{{erts_vsn}}\"}.\n"
     "{release_erts_version, \"{{release_erts_version}}\"}.\n"
        "{release_name, {{release_name}}}.\n"
        "{rel_vsn, \"{{release_version}}\"}.\n"
        "{release_version, \"{{release_version}}\"}.\n"
        "{log, \"{{log}}\"}.\n"
        "{output_dir, \"{{output_dir}}\"}.\n"
        "{target_dir, \"{{target_dir}}\"}.\n"
        "{config_file, \"{{ config_file }}\"}.\n"
        "{sys_config, \"{{sys_config}}\"}.\n"
        "{root_dir, \"{{root_dir}}\"}.\n"
        "{default_release_name, {{default_release_name}}}.\n"
        "{default_release_version, \"{{default_release_version}}\"}.\n"
        "{default_release, \"{{default_release}}\"}.\n"
        "{yahoo4, \"{{yahoo4}}\"}.\n"
        "{yahoo, \"{{yahoo}}\"}.\n"
        "{foo_dir, \"{{foo_dir}}\"}.\n"
        "{foo_yahoo, \"{{foo_yahoo}}\"}.\n"
        "{google, \"{{google}}\"}.\n"
        "{prop1, \"{{prop1}}\"}.\n"
        "{prop2, {{prop2}}}.\n".

escript_contents() ->
    "#!/usr/bin/env escript\n"
    "\n"
    "main(_Args) ->\n"
    "io:format(\"~s\n\",\n"
    "    [os:getenv(\"RELEASE_ROOT_DIR\")]).\n".

-ifdef(rand_module).
random_uniform(N) ->
    rand:uniform(N).
-else.
random_uniform(N) ->
    random:seed(os:timestamp()),
    random:uniform(N).
-endif.

list_to_term(String) ->
    {ok, T, _} = erl_scan:string(String++"."),
    case erl_parse:parse_term(T) of
        {ok, Term} ->
            Term;
        {error, Error} ->
            Error
    end.

unescape_string(String) ->
    re:replace(String, "\"", "",
               [global, {return, list}]).
