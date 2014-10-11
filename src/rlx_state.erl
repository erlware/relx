%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
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
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012 Erlware, LLC.
%%%
%%% @doc Provides state management services for the relx tool. Generally,
%%% those things that are fixed have a direct api. Those things that are mutable
%%% have a more mutable api.
-module(rlx_state).

-export([new/2,
         new/3,
         log/1,
         actions/1,
         output_dir/1,
         base_output_dir/1,
         base_output_dir/2,
         lib_dirs/1,
         add_lib_dirs/2,
         overrides/1,
         overrides/2,
         skip_apps/1,
         skip_apps/2,
         goals/1,
         goals/2,
         config_file/1,
         config_file/2,
         cli_args/1,
         cli_args/2,
         providers/1,
         providers/2,
         add_provider/2,
         prepend_hook/3,
         append_hook/3,
         hooks/2,
         vm_args/1,
         vm_args/2,
         sys_config/1,
         sys_config/2,
         root_dir/1,
         root_dir/2,
         add_configured_release/2,
         get_configured_release/3,
         configured_releases/1,
         realized_releases/1,
         realized_releases/2,
         add_realized_release/2,
         get_realized_release/3,
         update_realized_release/2,
         default_configured_release/1,
         default_configured_release/3,
         available_apps/1,
         available_apps/2,
         get/2,
         get/3,
         put/3,
         caller/1,
         caller/2,
         dev_mode/1,
         dev_mode/2,
         include_src/1,
         include_src/2,
         upfrom/1,
         upfrom/2,
         format/1,
         format/2]).


-export_type([t/0,
             releases/0,
             cmd_args/0,
             action/0]).

-record(state_t, {log :: ec_cmd_log:t(),
                  root_dir :: file:name(),
                  caller :: caller(),
                  actions=[] :: [action()],
                  output_dir :: file:name(),
                  lib_dirs=[] :: [file:name()],
                  config_file=[] :: file:filename() | undefined,
                  cli_args=[] :: proplists:proplist(),
                  goals=[] :: [rlx_depsolver:constraint()],
                  providers=[] :: [providers:t()],
                  available_apps=[] :: [rlx_app_info:t()],
                  default_configured_release :: {rlx_release:name() | undefined, rlx_release:vsn() |undefined} | undefined,
                  vm_args :: file:filename() | undefined,
                  sys_config :: file:filename() | undefined,
                  overrides=[] :: [{AppName::atom(), Directory::file:filename()}],
                  skip_apps=[] :: [AppName::atom()],
                  configured_releases :: releases(),
                  realized_releases :: releases(),
                  dev_mode=false :: boolean(),
                  include_src=true :: boolean(),
                  upfrom :: string() | binary() | undefined,
                  config_values :: ec_dictionary:dictionary(Key::atom(),
                                                            Value::term())}).

%%============================================================================
%% types
%%============================================================================

-type releases() :: ec_dictionary:dictionary({rlx_release:name(),
                                              rlx_release:vsn()},
                                             rlx_release:t()).
-type cmd_args() :: proplists:proplist().
-type caller() :: command_line | api.
-type action() :: release | relup | tar.

-opaque t() :: record(state_t).

%%============================================================================
%% API
%%============================================================================
-spec new(string(), undefined | [atom()]) -> t() | relx:error().
new(Config, Targets) ->
    new(Config, [], Targets).

-spec new(string(), proplists:proplist(), undefined | [atom()]) -> t() | relx:error().
new(Config, CommandLineConfig, undefined) ->
    new(Config, CommandLineConfig, [release]);
new(Config, CommandLineConfig, Targets)
  when erlang:is_list(CommandLineConfig),
     erlang:is_list(Targets) ->
    {ok, Root} = file:get_cwd(),

    Caller = proplists:get_value(caller, CommandLineConfig, api),
    Log = proplists:get_value(log, CommandLineConfig, ec_cmd_log:new(error, Caller)),
    State0 = #state_t{log=Log,
                      config_file=Config,
                      cli_args=CommandLineConfig,
                      actions=Targets,
                      caller=Caller,
                      root_dir=Root,
                      output_dir=filename:join(Root, "_rel"),
                      providers=[],
                      default_configured_release=undefined,
                      configured_releases=ec_dictionary:new(ec_dict),
                      realized_releases=ec_dictionary:new(ec_dict),
                      config_values=ec_dictionary:new(ec_dict)},
    State1 = rlx_state:put(State0, default_libs, true),
    State2 = rlx_state:put(State1, system_libs, undefined),
    State3 = rlx_state:put(State2, overlay_vars, []),

    create_logic_providers(State3).

%% @doc the actions targeted for this system
-spec actions(t()) -> [action()].
actions(#state_t{actions=Actions}) ->
    Actions.

%% @doc the application overrides for the system
-spec overrides(t()) -> [{AppName::atom(), Directory::file:filename()}].
overrides(#state_t{overrides=Overrides}) ->
    Overrides.

%% @doc the application overrides for the system
-spec overrides(t(), [{AppName::atom(), Directory::file:filename()}]) -> t().
overrides(State, Overrides) ->
    State#state_t{overrides=Overrides}.

-spec skip_apps(t()) -> [AppName::atom()].
skip_apps(#state_t{skip_apps=Apps}) ->
    Apps.

%% @doc the application overrides for the system
-spec skip_apps(t(), [AppName::atom()]) -> t().
skip_apps(State, SkipApps) ->
    State#state_t{skip_apps=SkipApps}.

%% @doc get the current log state for the system
-spec log(t()) -> ec_cmd_log:t().
log(#state_t{log=LogState}) ->
    LogState.

-spec output_dir(t()) -> file:name().
output_dir(State=#state_t{output_dir=OutDir}) ->
    {RelName, _RelVsn} = default_configured_release(State),
    filename:join(OutDir, RelName).

-spec base_output_dir(t()) -> file:name().
base_output_dir(#state_t{output_dir=OutDir}) ->
    OutDir.

-spec base_output_dir(t(), Directory::file:filename()) -> t().
base_output_dir(State, Directory) ->
    State#state_t{output_dir=Directory}.

-spec lib_dirs(t()) -> [file:name()].
lib_dirs(#state_t{lib_dirs=LibDir}) ->
    LibDir.

-spec add_lib_dirs(t(), [file:name()]) -> t().
add_lib_dirs(State=#state_t{lib_dirs=LibDir}, Dirs) ->
    State#state_t{lib_dirs=lists:umerge(lists:sort(LibDir), lists:sort(Dirs))}.

-spec goals(t()) -> [rlx_depsolver:constraint()].
goals(#state_t{goals=TS}) ->
    TS.

-spec goals(t(), [rlx_depsolver:constraint()]) -> t().
goals(State, Goals) ->
    State#state_t{goals=Goals}.

-spec config_file(t()) -> file:filename() | proplists:proplist() | undefined.
config_file(#state_t{config_file=ConfigFiles}) ->
    ConfigFiles.

-spec config_file(t(), file:filename() | proplists:proplist() | undefined) -> t().
config_file(State, ConfigFiles) ->
    State#state_t{config_file=ConfigFiles}.

-spec cli_args(t()) -> proplists:proplist().
cli_args(#state_t{cli_args=CliArgs}) ->
    CliArgs.

-spec cli_args(t(), proplists:proplist()) -> t().
cli_args(State, CliArgs) ->
    State#state_t{cli_args=CliArgs}.

-spec providers(t()) -> [providers:t()].
providers(#state_t{providers=Providers}) ->
    Providers.

-spec vm_args(t()) -> file:filename() | undefined.
vm_args(#state_t{vm_args=VmArgs}) ->
    VmArgs.

-spec vm_args(t(), file:filename()) -> t().
vm_args(State, VmArgs) ->
    State#state_t{vm_args=VmArgs}.

-spec sys_config(t()) -> file:filename() | undefined.
sys_config(#state_t{sys_config=SysConfig}) ->
    SysConfig.

-spec sys_config(t(), file:filename()) -> t().
sys_config(State, SysConfig) ->
    State#state_t{sys_config=SysConfig}.

-spec root_dir(t()) -> file:filename() | undefined.
root_dir(#state_t{root_dir=RootDir}) ->
    RootDir.

-spec root_dir(t(), file:filename()) -> t().
root_dir(State, RootDir) ->
    State#state_t{root_dir=filename:absname(RootDir)}.

-spec providers(t(), [providers:t()]) -> t().
providers(M, NewProviders) ->
    M#state_t{providers=NewProviders}.

-spec add_provider(t(), providers:t()) -> t().
add_provider(M=#state_t{providers=Providers}, Provider) ->
    M#state_t{providers=[Provider | Providers]}.

-spec add_configured_release(t(), rlx_release:t()) -> t().
add_configured_release(M=#state_t{configured_releases=Releases}, Release) ->
    M#state_t{configured_releases=ec_dictionary:add({rlx_release:name(Release),
                                          rlx_release:vsn(Release)},
                                         Release,
                                         Releases)}.

-spec get_configured_release(t(), rlx_release:name(), rlx_release:vsn()) -> rlx_release:t().
get_configured_release(#state_t{configured_releases=Releases}, Name, Vsn) ->
    ec_dictionary:get({Name, Vsn}, Releases).

-spec configured_releases(t()) -> releases().
configured_releases(#state_t{configured_releases=Releases}) ->
    Releases.

-spec realized_releases(t()) -> releases().
realized_releases(#state_t{realized_releases=Releases}) ->
    Releases.

-spec realized_releases(t(), releases()) -> t().
realized_releases(State, Releases) ->
    State#state_t{realized_releases=Releases}.

-spec add_realized_release(t(), rlx_release:t()) -> t().
add_realized_release(State = #state_t{realized_releases=Releases}, Release) ->
    NewReleases = ec_dictionary:add({rlx_release:name(Release), rlx_release:vsn(Release)},
                                    Release, Releases),
    State#state_t{realized_releases=NewReleases}.

-spec get_realized_release(t(), rlx_release:name(), rlx_release:vsn()) -> rlx_release:t().
get_realized_release(#state_t{realized_releases=Releases}, Name, Vsn) ->
    ec_dictionary:get({Name, Vsn}, Releases).

-spec update_realized_release(t(), rlx_release:t()) ->
     t().
update_realized_release(M=#state_t{realized_releases=Releases}, Release) ->
    M#state_t{realized_releases=ec_dictionary:add({rlx_release:name(Release),
                                                   rlx_release:vsn(Release)},
                                                  Release,
                                                  Releases)}.

-spec default_configured_release(t()) -> {rlx_release:name() | undefined,
                                         rlx_release:vsn() | undefined} | default.
default_configured_release(#state_t{default_configured_release=Def}) ->
    Def.

-spec default_configured_release(t(), rlx_release:name(), rlx_release:vsn()) -> t().
default_configured_release(M, Name, Vsn) ->
    M#state_t{default_configured_release={Name, Vsn}}.

-spec available_apps(t()) -> [rlx_app_info:t()].
available_apps(#state_t{available_apps=Apps}) ->
    Apps.

-spec available_apps(t(), [rlx_app_info:t()]) -> t().
available_apps(M, NewApps) ->
    M#state_t{available_apps=NewApps}.

-spec get(t(), atom()) -> term().
get(#state_t{config_values=Config}, Key)
  when erlang:is_atom(Key) ->
    ec_dictionary:get(Key, Config).

-spec get(t(), atom(), DefaultValue::term()) -> term().
get(#state_t{config_values=Config}, Key, DefaultValue)
  when erlang:is_atom(Key) ->
  try
      ec_dictionary:get(Key, Config)
  catch
      throw:not_found ->
          DefaultValue
  end.

-spec put(t(), atom(), term()) ->t().
put(M=#state_t{config_values=Config}, Key, Value)
  when erlang:is_atom(Key) ->
    M#state_t{config_values=ec_dictionary:add(Key, Value, Config)}.

-spec caller(t()) -> caller().
caller(#state_t{caller=Caller}) ->
    Caller.

-spec caller(t(), caller()) -> t().
caller(S, Caller) ->
    S#state_t{caller=Caller}.

-spec dev_mode(t()) -> boolean().
dev_mode(#state_t{dev_mode=DevMode}) ->
    DevMode.

-spec dev_mode(t(), boolean()) -> t().
dev_mode(S, DevMode) ->
    S#state_t{dev_mode=DevMode}.

-spec include_src(t()) -> boolean().
include_src(#state_t{include_src=IncludeSrc}) ->
    IncludeSrc.

-spec include_src(t(), boolean()) -> t().
include_src(S, IncludeSrc) ->
    S#state_t{include_src=IncludeSrc}.

-spec upfrom(t()) -> string() | binary() | undefined.
upfrom(#state_t{upfrom=UpFrom}) ->
    UpFrom.

-spec upfrom(t(), string() | binary() | undefined) -> t().
upfrom(State, UpFrom) ->
    State#state_t{upfrom=UpFrom}.

-spec format(t()) -> iolist().
format(Mod) ->
    format(Mod, 0).

-spec format(t(), non_neg_integer()) -> iolist().
format(#state_t{log=LogState, output_dir=OutDir, lib_dirs=LibDirs,
                caller=Caller, config_values=Values0,
                goals=Goals, config_file=ConfigFile,
                providers=Providers},
       Indent) ->
    Values1 = ec_dictionary:to_list(Values0),
    [rlx_util:indent(Indent),
     <<"state(">>, erlang:atom_to_list(Caller), <<"):\n">>,
     rlx_util:indent(Indent + 2), <<"log: ">>, ec_cmd_log:format(LogState), <<",\n">>,
     rlx_util:indent(Indent + 2), "config file: ", rlx_util:optional_to_string(ConfigFile), "\n",
     rlx_util:indent(Indent + 2), "goals: \n",
     [[rlx_util:indent(Indent + 3), rlx_depsolver:format_constraint(Goal), ",\n"] || Goal <- Goals],
     rlx_util:indent(Indent + 2), "output_dir: ", OutDir, "\n",
     rlx_util:indent(Indent + 2), "lib_dirs: \n",
     [[rlx_util:indent(Indent + 3), LibDir, ",\n"] || LibDir <- LibDirs],
     rlx_util:indent(Indent + 2), "providers: \n",
     [[rlx_util:indent(Indent + 3), providers:format(Provider), ",\n"] || Provider <- Providers],
     rlx_util:indent(Indent + 2), "provider config values: \n",
     [[rlx_util:indent(Indent + 3), io_lib:format("~p", [Value]), ",\n"] || Value <- Values1]].

prepend_hook(State=#state_t{providers=_Providers}, Target, Hook) ->
    {Providers1, State1} = add_hook(pre, Target, Hook, State),
    State1#state_t{providers=Providers1}.

append_hook(State=#state_t{providers=_Providers}, Target, Hook) ->
    {Providers1, State1} = add_hook(post, Target, Hook, State),
    State1#state_t{providers=Providers1}.

-spec hooks(t(), atom()) -> {[providers:t()], [providers:t()]}.
hooks(_State=#state_t{providers=Providers}, Target) ->
    Provider = providers:get_provider(Target, Providers),
    providers:hooks(Provider).

%% ===================================================================
%% Internal functions
%% ===================================================================

add_hook(Which, Target, Hook, State) ->
    {ok, State1} = providers:new(Hook, State),
    Providers1 = providers(State1),
    HookProvider = providers:get_provider_by_module(Hook, Providers1),
    Provider = providers:get_provider(Target, Providers1),
    Hooks = providers:hooks(Provider),
    NewHooks = add_hook(Which, Hooks, HookProvider),
    NewProvider = providers:hooks(Provider, NewHooks),
    {[NewProvider | lists:delete(Provider, Providers1)], State1}.

add_hook(pre, {PreHooks, PostHooks}, Hook) ->
    {[Hook | PreHooks], PostHooks};
add_hook(post, {PreHooks, PostHooks}, Hook) ->
    {PreHooks, [Hook | PostHooks]}.

-spec create_logic_providers(t()) -> t() | relx:error().
create_logic_providers(State) ->
    create_all(State, [rlx_prv_discover,
                       rlx_prv_overlay,
                       rlx_prv_release,
                       rlx_prv_assembler,
                       rlx_prv_relup,
                       rlx_prv_archive]).

create_all(State, []) ->
    State;
create_all(State, [Module | Rest]) ->
    case providers:new(Module, State) of
        {ok, State1} ->
            create_all(State1, Rest);
        Error ->
             Error
    end.

%%%===================================================================
%%% Test Functions
%%%===================================================================
