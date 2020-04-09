-module(rlx_config).

-export([to_state/2,
         load/2]).

-include("relx.hrl").
-include("rlx_log.hrl").

%% TODO: list out each supported config
-type t() :: [{atom(), term()}].

-export_type([t/0]).

to_state(Config, State) ->
    lists:foldl(fun load/2, {ok, State}, Config).

-spec load(term(), {ok, rlx_state:t()} | relx:error()) -> {ok, rlx_state:t()} | relx:error().
load({paths, Paths}, {ok, State}) ->
    code:add_pathsa([filename:absname(Path) || Path <- Paths]),
    {ok, State};
load({default_libs, DefaultLibs}, {ok, State}) ->
    State2 = rlx_state:put(State, default_libs, DefaultLibs),
    {ok, State2};
load({lib_dirs, Dirs}, {ok, State}) ->
    LibDirs = [list_to_binary(Dir) || Dir <- rlx_file_utils:wildcard_paths(Dirs)],
    State1 = rlx_state:add_lib_dirs(State, LibDirs),
    {ok, State1};
load({exclude_apps, ExcludeApps0}, {ok, State0}) ->
    {ok, rlx_state:exclude_apps(State0, ExcludeApps0)};
load({exclude_modules, ExcludeModules0}, {ok, State0}) ->
    {ok, rlx_state:exclude_modules(State0, ExcludeModules0)};
load({debug_info, DebugInfo}, {ok, State0}) ->
    {ok, rlx_state:debug_info(State0, DebugInfo)};
load({overrides, Overrides0}, {ok, State0}) ->
    {ok, rlx_state:overrides(State0, Overrides0)};
load({dev_mode, DevMode}, {ok, State0}) ->
    {ok, rlx_state:dev_mode(State0, DevMode)};
load({upfrom, UpFrom}, {ok, State0}) ->
    {ok, rlx_state:upfrom(State0, UpFrom)};
load({include_src, IncludeSrc}, {ok, State0}) ->
    {ok, rlx_state:include_src(State0, IncludeSrc)};
load({release, {RelName, Vsn, {extend, {RelName2, Vsn2}}}, Applications}, {ok, State0}) ->
    NewVsn = parse_vsn(Vsn),
    NewVsn2 = parse_vsn(Vsn2),
    add_extended_release(RelName, NewVsn, RelName2, NewVsn2, Applications, State0);
load({release, {RelName, Vsn, {extend, RelName2}}, Applications}, {ok, State0}) ->
    NewVsn = parse_vsn(Vsn),
    add_extended_release(RelName, NewVsn, RelName2, NewVsn, Applications, State0);
load({release, {RelName, Vsn, {extend, {RelName2, Vsn2}}}, Applications, Config}, {ok, State0}) ->
    NewVsn = parse_vsn(Vsn),
    NewVsn2 = parse_vsn(Vsn2),
    add_extended_release(RelName, NewVsn, RelName2, NewVsn2, Applications, Config, State0);
load({release, {RelName, Vsn, {extend, RelName2}}, Applications, Config}, {ok, State0}) ->
    NewVsn = parse_vsn(Vsn),
    add_extended_release(RelName, NewVsn, RelName2, NewVsn, Applications, Config, State0);
load({release, {RelName, Vsn}, {erts, ErtsVsn}, Applications}, {ok, State0}) ->
    NewVsn = parse_vsn(Vsn),
    Release0 = rlx_release:erts(rlx_release:new(RelName, NewVsn), ErtsVsn),
    Release1 = rlx_release:goals(Release0, Applications),
    {ok, rlx_state:add_configured_release(State0, Release1)};
load({release, {RelName, Vsn}, {erts, ErtsVsn}, Applications, Config}, {ok, State0}) ->
    NewVsn = parse_vsn(Vsn),
    Release0 = rlx_release:erts(rlx_release:new(RelName, NewVsn), ErtsVsn),
    Release1 = rlx_release:goals(Release0, Applications),
    Release2 = rlx_release:config(Release1, Config),
    {ok, rlx_state:add_configured_release(State0, Release2)};
load({release, {RelName, Vsn}, Applications}, {ok, State0}) ->
    NewVsn = parse_vsn(Vsn),
    Release0 = rlx_release:new(RelName, NewVsn),
    Release1 = rlx_release:goals(Release0, Applications),
    {ok, rlx_state:add_configured_release(State0, Release1)};
load({release, {RelName, Vsn}, Applications, Config}, {ok, State0}) ->
    NewVsn = parse_vsn(Vsn),
    Release0 = rlx_release:new(RelName, NewVsn),
    Release1 = rlx_release:goals(Release0, Applications),
    Release2 = rlx_release:config(Release1, Config),
    {ok, rlx_state:add_configured_release(State0, Release2)};
load({vm_args, false}, {ok, State}) ->
    {ok, rlx_state:vm_args(State, false)};
load({vm_args, VmArgs}, {ok, State}) ->
    {ok, rlx_state:vm_args(State, filename:absname(VmArgs))};
load({vm_args_src, VmArgs}, {ok, State}) ->
    {ok, rlx_state:vm_args_src(State, filename:absname(VmArgs))};
load({sys_config, false}, {ok, State}) ->
    {ok, rlx_state:sys_config(State, false)};
load({sys_config, SysConfig}, {ok, State}) ->
    {ok, rlx_state:sys_config(State, filename:absname(SysConfig))};
load({sys_config_src, SysConfigSrc}, {ok, State}) ->
    {ok, rlx_state:sys_config_src(State, filename:absname(SysConfigSrc))};
load({root_dir, Root}, {ok, State}) ->
    {ok, rlx_state:root_dir(State, filename:absname(Root))};
load({output_dir, OutputDir}, {ok, State}) ->
    {ok, rlx_state:base_output_dir(State, filename:absname(OutputDir))};
load({overlay_vars_values, OverlayVarsValues}, {ok, State}) ->
    CurrentOverlayVarsValues = rlx_state:get(State, overlay_vars_values),
    NewOverlayVarsValues = CurrentOverlayVarsValues ++ OverlayVarsValues,
    {ok, rlx_state:put(State, overlay_vars_values, NewOverlayVarsValues)};
load({overlay_vars, OverlayVars}, {ok, State}) ->
    CurrentOverlayVars = rlx_state:get(State, overlay_vars),
    NewOverlayVars0 = list_of_overlay_vars_files(OverlayVars),
    NewOverlayVars1 = CurrentOverlayVars ++ NewOverlayVars0,
    {ok, rlx_state:put(State, overlay_vars, NewOverlayVars1)};
load({warnings_as_errors, WarningsAsErrors}, {ok, State}) ->
    {ok, rlx_state:warnings_as_errors(State, WarningsAsErrors)};
load({src_tests, SrcTests}, {ok, State}) ->
    {ok, rlx_state:src_tests(State, SrcTests)};
load({Name, Value}, {ok, State})
  when erlang:is_atom(Name) ->
    {ok, rlx_state:put(State, Name, Value)};
load(_, Error={error, _}) ->
    Error;
load(InvalidTerm, _) ->
    ?RLX_ERROR({invalid_term, InvalidTerm}).

%%

parse_vsn(Vsn) when Vsn =:= git ; Vsn =:= "git" ->
    try_vsn(fun git_tag_vsn/0);
parse_vsn({git, short}) ->
    try_vsn(fun() -> git_ref("--short") end);
parse_vsn({git, long}) ->
    try_vsn(fun() -> git_ref("") end);
parse_vsn({file, File}) ->
    try_vsn(fun() ->
                    {ok, Vsn} = file:read_file(File),
                    rlx_util:to_string(rlx_string:trim(rlx_util:to_string(Vsn), both, "\n"))
            end);
parse_vsn(Vsn) when Vsn =:= semver ; Vsn =:= "semver" ->
    try_vsn(fun git_tag_vsn/0);
parse_vsn({semver, _}) ->
    try_vsn(fun git_tag_vsn/0);
parse_vsn({cmd, Command}) ->
    try_vsn(fun() -> rlx_util:sh(Command) end);
parse_vsn(Vsn) ->
    Vsn.

try_vsn(Fun) ->
    try
        Fun()
    catch
        error:_ ->
            "0.0.0"
    end.

%% TODO: handle versions in rebar3 or whatever else is calling relx
git_ref(Arg) ->
    String = rlx_util:sh("git rev-parse " ++ Arg ++ " HEAD"),
    Vsn = rlx_string:trim(String, both, "\n"),
    case length(Vsn) =:= 40 orelse length(Vsn) =:= 7 of
        true ->
            Vsn;
        false ->
            %% if the result isn't exactly either 40 or 7 characters then
            %% it must have failed
            {ok, Dir} = file:get_cwd(),
            ?log_warn("Getting ref of git repo failed in directory ~ts. Falling back to version 0",
                      [Dir]),
            "0.0.0"
    end.

%% TODO: handle versions in rebar3 or whatever else is calling relx
git_tag_vsn() ->
    {Vsn, RawRef, RawCount} = collect_default_refcount(),
    build_vsn_string(Vsn, RawRef, RawCount).

collect_default_refcount() ->
    %% Get the tag timestamp and minimal ref from the system. The
    %% timestamp is really important from an ordering perspective.
    RawRef = rlx_util:sh("git log -n 1 --pretty=format:'%h\n' "),

    {Tag, Vsn} = parse_tags(),
    RawCount = get_patch_count(Tag),
    {Vsn, RawRef, RawCount}.

get_patch_count(RawRef) ->
    Ref = re:replace(RawRef, "\\s", "", [global]),
    Cmd = io_lib:format("git rev-list --count ~s..HEAD", [Ref]),
    rlx_util:sh(Cmd).

build_vsn_string(Vsn, RawRef, RawCount) ->
    %% Cleanup the tag and the Ref information. Basically leading 'v's and
    %% whitespace needs to go away.
    RefTag = [".ref", re:replace(RawRef, "\\s", "", [global])],
    Count = re:replace(RawCount, "\\s", "", [global]),

    %% Create the valid [semver](http://semver.org) version from the tag
    case Count of
        <<"0">> ->
            lists:flatten(Vsn);
        _ ->
            lists:flatten([Vsn, "+build.", Count, RefTag])
    end.

parse_tags() ->
    Tag = rlx_util:sh("git describe --abbrev=0 --tags"),
    Vsn = rlx_string:trim(rlx_string:trim(Tag, leading, "v"), leading, "\n"),
    {Tag, Vsn}.


add_extended_release(RelName, NewVsn, RelName2, NewVsn2, Applications, State0) ->
    Release0 = rlx_release:new(RelName, NewVsn),
    ExtendRelease = rlx_state:get_configured_release(State0, RelName2, NewVsn2),
    Applications1 = rlx_release:goals(ExtendRelease),
    Release1 = rlx_release:goals(Release0, merge_application_goals(Applications, Applications1)),
    {ok, rlx_state:add_configured_release(State0, Release1)}.

add_extended_release(RelName, NewVsn, RelName2, NewVsn2, Applications, Config, State0) ->
    Release0 = rlx_release:new(RelName, NewVsn),
    ExtendRelease = rlx_state:get_configured_release(State0, RelName2, NewVsn2),
    Applications1 = rlx_release:goals(ExtendRelease),
    Release1 = rlx_release:goals(Release0, merge_application_goals(Applications, Applications1)),
    Release2 = rlx_release:config(Release1, Config),
    {ok, rlx_state:add_configured_release(State0, Release2)}.

list_of_overlay_vars_files(undefined) ->
    [];
list_of_overlay_vars_files([]) ->
    [];
list_of_overlay_vars_files([H | _]=Vars) when erlang:is_list(H) ;
                                              is_tuple(H) ->
    Vars;
list_of_overlay_vars_files(FileName) when is_list(FileName) ->
    [FileName].

merge_application_goals(Goals, BaseGoals) ->
    ParsedGoals = rlx_release:parse_goals(Goals),
    maps:merge(BaseGoals, ParsedGoals).
