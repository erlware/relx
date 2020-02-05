%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2015, Tristan Sloughter
-module(rlx_test_utils).

-compile(export_all).

create_app(Dir, Name, Vsn, Deps, LibDeps) ->
    AppDir = filename:join([Dir, Name ++ "-" ++ Vsn]),
    write_app_file(AppDir, Name, Vsn, app_modules(Name), Deps, LibDeps),
    write_src_file(AppDir, Name),
    compile_src_files(AppDir),
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
    write_app_file(AppDir, Name, Vsn, [], Deps, LibDeps),
    rlx_app_info:new(erlang:list_to_atom(Name), Vsn, AppDir,
                     Deps, []).

app_modules(Name) ->
    [list_to_atom(M ++ Name) ||
        M <- ["a_real_beam"]].

write_src_file(Dir, Name) ->
    Src = filename:join([Dir, "src", "a_real_beam" ++ Name ++ ".erl"]),
    ok = filelib:ensure_dir(Src),
    ok = file:write_file(Src, beam_file_contents("a_real_beam"++Name)).

write_appup_file(AppInfo, DownVsn) ->
    Dir = rlx_app_info:dir(AppInfo),
    Name = rlx_util:to_string(rlx_app_info:name(AppInfo)),
    Vsn = rlx_app_info:vsn(AppInfo),
    Filename = filename:join([Dir, "ebin", Name ++ ".appup"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, {Vsn, [{DownVsn, []}], [{DownVsn, []}]}).

write_app_file(Dir, Name, Version, Modules, Deps, LibDeps) ->
    Filename = filename:join([Dir, "ebin", Name ++ ".app"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(Name, Version, Modules,
                                                       Deps, LibDeps)).

compile_src_files(Dir) ->
    %% compile all *.erl files in src to ebin
    SrcDir = filename:join([Dir, "src"]),
    OutputDir = filename:join([Dir, "ebin"]),
    lists:foreach(fun(SrcFile) ->
                          {ok, _} = compile:file(SrcFile, [{outdir, OutputDir},
                                                           return_errors])
                  end, ec_file:find(SrcDir, "\\.erl")),
    ok.

get_app_metadata(Name, Vsn, Modules, Deps, LibDeps) ->
    {application, erlang:list_to_atom(Name),
     [{description, ""},
      {vsn, Vsn},
      {modules, Modules},
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

beam_file_contents(Name) ->
    "-module("++Name++").".

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
        "{prop2, {{prop2}}}.\n"
        "{tpl_var, \"{{tpl_var}}\"}.\n"
        "{api_caller_var, \"{{api_caller_var}}\"}.\n".

escript_contents() ->
    "#!/usr/bin/env escript\n"
    "\n"
    "main(_Args) ->\n"
    "io:format(\"~s\n\",\n"
    "    [os:getenv(\"RELEASE_ROOT_DIR\")]).\n".

random_uniform(N) ->
    rand:uniform(N).

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

%% discovery

all_apps(LibDirs) ->
    Dirs = [list_to_binary(Dir) || Dir <- rlx_file_utils:wildcard_paths(LibDirs ++ code:get_path())],
    resolve_app_metadata(Dirs).

app_files(LibDirs) ->
    lists:foldl(fun(LibDir, Acc) ->
                        Files = app_files_paths(LibDir),
                        BinFiles = lists:map(fun(F) ->
                                                     list_to_binary(F)
                                             end, Files),
                        Acc ++ BinFiles
                end, [], LibDirs).

-spec app_files_paths(binary()) -> list(string()).
app_files_paths(LibDir) ->
    %% Search for Erlang apps in the lib dir itself
    Path1 = filename:join([binary_to_list(LibDir),
                           "*.app"]),
    %% Search for Erlang apps in subdirs of lib dir
    Path2 = filename:join([binary_to_list(LibDir),
                           "*",
                           "ebin",
                           "*.app"]),
    lists:foldl(fun(Path, Acc) ->
                        Files = filelib:wildcard(Path),
                        Files ++ Acc
                end, [], [Path1, Path2]).

-spec get_app_metadata(list(binary())) -> list({ok, rlx_app_info:t()}).
get_app_metadata(LibDirs) ->
    lists:foldl(fun(AppFile, Acc) ->
                        case is_valid_otp_app(AppFile) of
                            {ok, _} = AppMeta ->
                                [AppMeta|Acc];
                            {warning, _W} ->
                                Acc;
                            {error, _E} ->
                                Acc;
                            _ ->
                                Acc
                        end
                end, [], app_files(LibDirs)).

resolve_app_metadata(LibDirs) ->
    [App || {ok, App} <- lists:flatten(rlx_dscv_util:do(fun discover_dir/2, LibDirs))].

-spec discover_dir([file:name()], directory | file) -> {ok, rlx_app_info:t()} |
                                                       {error, Reason::term()}.
discover_dir(_File, directory) ->
    {noresult, true};
discover_dir(File, file) ->
    is_valid_otp_app(File).

-spec is_valid_otp_app(file:name()) -> {ok, rlx_app_info:t()} |
                                       {warning, Reason::term()} |
                                       {error, Reason::term()} |
                                       {noresult, false}.
is_valid_otp_app(File) ->
    %% Is this an ebin dir?
    EbinDir = filename:dirname(File),
    case filename:basename(EbinDir) of
        <<"ebin">> ->
            case filename:extension(File) of
                <<".app">> ->
                    gather_application_info(EbinDir, File);
                _ ->
                    {noresult, false}
            end;
        _ ->
            {noresult, false}
    end.


-spec gather_application_info(file:name(), file:filename()) ->
                                     {ok, rlx_app_info:t()} |
                                     {warning, Reason::term()} |
                                     {error, Reason::term()}.
gather_application_info(EbinDir, File) ->
    AppDir = filename:dirname(EbinDir),
    case file:consult(File) of
        {ok, [{application, AppName, AppDetail}]} ->
            validate_application_info(EbinDir, AppName, AppDetail);
        {error, Reason} ->
            {warning, {unable_to_load_app, AppDir, Reason}};
        _ ->
            {warning, {invalid_app_file, File}}
    end.

validate_application_info(EbinDir, AppName, AppDetail) ->
    AppDir = filename:dirname(EbinDir),
    get_vsn(AppDir, AppName, AppDetail).

-spec get_vsn(file:name(), atom(), proplists:proplist()) ->
                     {ok, rlx_app_info:t()} | {error, Reason::term()}.
get_vsn(AppDir, AppName, AppDetail) ->
    case proplists:get_value(vsn, AppDetail) of
        undefined ->
            {error, {unversioned_app, AppDir, AppName}};
        AppVsn ->
            case get_deps(AppDir, AppName, AppVsn, AppDetail) of
                {ok, App} ->
                    {ok, App};
                {error, Detail} ->
                    {error, {app_info_error, Detail}}
            end
    end.

-spec get_deps(binary(), atom(), string(), proplists:proplist()) ->
                      {ok, rlx_app_info:t()} | {error, Reason::term()}.
get_deps(AppDir, AppName, AppVsn, AppDetail) ->
    %% ensure that at least stdlib and kernel are defined as application deps
    ActiveApps = ensure_stdlib_kernel(AppName,
                                      proplists:get_value(applications, AppDetail, [])),
    LibraryApps = proplists:get_value(included_applications, AppDetail, []),
    rlx_app_info:new(AppName, AppVsn, AppDir, ActiveApps, LibraryApps).

-spec ensure_stdlib_kernel(AppName :: atom(),
                           Apps :: list(atom())) -> list(atom()).
ensure_stdlib_kernel(kernel, Deps) -> Deps;
ensure_stdlib_kernel(stdlib, Deps) -> Deps;
ensure_stdlib_kernel(_AppName, []) ->
    %% minimum required deps are kernel and stdlib
    [kernel, stdlib];
ensure_stdlib_kernel(_AppName, Deps) -> Deps.
