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
         log/1,
         action/1,
         output_dir/1,
         lib_dirs/1,
         overrides/1,
         overrides/2,
         skip_apps/1,
         skip_apps/2,
         goals/1,
         config_file/1,
         config_file/2,
         providers/1,
         providers/2,
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
         upfrom/1,
         format/1,
         format/2]).


-export_type([t/0,
              releases/0,
              cmd_args/0]).

-record(state_t, {log :: rlx_log:t(),
                  root_dir :: file:name(),
                  caller :: caller(),
                  action :: atom(),
                  output_dir :: file:name(),
                  lib_dirs=[] :: [file:name()],
                  config_file=[] :: file:filename() | undefined,
                  goals=[] :: [rlx_depsolver:constraint()],
                  providers = [] :: [rlx_provider:t()],
                  available_apps = [] :: [rlx_app_info:t()],
                  default_configured_release :: {rlx_release:name(), rlx_release:vsn()},
                  vm_args :: file:filename() | undefined,
                  sys_config :: file:filename() | undefined,
                  overrides :: [{AppName::atom(), Directory::file:filename()}],
                  skip_apps = [] :: [AppName::atom()],
                  configured_releases :: releases(),
                  realized_releases :: releases(),
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

-opaque t() :: record(state_t).

%%============================================================================
%% API
%%============================================================================
%% @doc Create a new 'log level' for the system
-spec new(proplists:proplist(), atom()) -> t().
new(PropList, Target)
  when erlang:is_list(PropList),
     erlang:is_atom(Target) ->
    {ok, Root} = file:get_cwd(),
    State0 =
        #state_t{log = proplists:get_value(log, PropList, rlx_log:new(error)),
                 output_dir=proplists:get_value(output_dir, PropList, ""),
                 lib_dirs=[to_binary(Dir) || Dir <- proplists:get_value(lib_dirs, PropList, [])],
                 config_file=proplists:get_value(config, PropList, undefined),
                 action = Target,
                 caller = proplists:get_value(caller, PropList, api),
                 goals=proplists:get_value(goals, PropList, []),
                 providers = [],
                 configured_releases=ec_dictionary:new(ec_dict),
                 realized_releases=ec_dictionary:new(ec_dict),
                 config_values=ec_dictionary:new(ec_dict),
                 overrides = proplists:get_value(overrides, PropList, []),
                 root_dir = proplists:get_value(root_dir, PropList, Root),
                 upfrom = proplists:get_value(upfrom, PropList, undefined),
                 default_configured_release={proplists:get_value(relname, PropList, undefined),
                                  proplists:get_value(relvsn, PropList, undefined)}},
    rlx_state:put(create_logic_providers(State0),
                  disable_default_libs,
                  proplists:get_value(disable_default_libs, PropList, false)).

%% @doc the action targeted for this system
-spec action(t()) -> atom().
action(#state_t{action=Action}) ->
    Action.

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
-spec log(t()) -> rlx_log:t().
log(#state_t{log=LogState}) ->
    LogState.

-spec output_dir(t()) -> file:name().
output_dir(#state_t{output_dir=OutDir}) ->
    OutDir.

-spec lib_dirs(t()) -> [file:name()].
lib_dirs(#state_t{lib_dirs=LibDir}) ->
    LibDir.

-spec goals(t()) -> [rlx_depsolver:constraint()].
goals(#state_t{goals=TS}) ->
    TS.

-spec config_file(t()) -> file:filename() | undefined.
config_file(#state_t{config_file=ConfigFiles}) ->
    ConfigFiles.

-spec config_file(t(), file:filename() | undefined) -> t().
config_file(State, ConfigFiles) ->
    State#state_t{config_file=ConfigFiles}.

-spec providers(t()) -> [rlx_provider:t()].
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
    State#state_t{root_dir=RootDir}.

-spec providers(t(), [rlx_provider:t()]) -> t().
providers(M, NewProviders) ->
    M#state_t{providers=NewProviders}.

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

-spec default_configured_release(t()) ->
                             {rlx_release:name() | undefined, rlx_release:vsn() | undefined}.
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

-spec upfrom(t()) -> string() | binary() | undefined.
upfrom(#state_t{upfrom=UpFrom}) ->
    UpFrom.

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
     rlx_util:indent(Indent + 1), <<"log: ">>, rlx_log:format(LogState), <<",\n">>,
     rlx_util:indent(Indent + 1), "config file: ", rlx_util:optional_to_string(ConfigFile), "\n",
     rlx_util:indent(Indent + 1), "goals: \n",
     [[rlx_util:indent(Indent + 2), rlx_depsolver:format_constraint(Goal), ",\n"] || Goal <- Goals],
     rlx_util:indent(Indent + 1), "output_dir: ", OutDir, "\n",
     rlx_util:indent(Indent + 1), "lib_dirs: \n",
     [[rlx_util:indent(Indent + 2), LibDir, ",\n"] || LibDir <- LibDirs],
     rlx_util:indent(Indent + 1), "providers: \n",
     [[rlx_util:indent(Indent + 2), rlx_provider:format(Provider), ",\n"] || Provider <- Providers],
     rlx_util:indent(Indent + 1), "provider config values: \n",
     [[rlx_util:indent(Indent + 2), io_lib:format("~p", [Value]), ",\n"] || Value <- Values1]].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec create_logic_providers(t()) -> t().
create_logic_providers(State0) ->
    {ConfigProvider, {ok, State1}} = rlx_provider:new(rlx_prv_config, State0),
    {DiscoveryProvider, {ok, State2}} = rlx_provider:new(rlx_prv_discover, State1),
    {ReleaseProvider, {ok, State3}} = rlx_provider:new(rlx_prv_release, State2),
    {OverlayProvider, {ok, State4}} = rlx_provider:new(rlx_prv_overlay, State3),
    {AssemblerProvider, {ok, State5}} = rlx_provider:new(rlx_prv_assembler, State4),
    State5#state_t{providers=[ConfigProvider, DiscoveryProvider,
                              ReleaseProvider, OverlayProvider, AssemblerProvider]}.

to_binary(Dir)
  when erlang:is_list(Dir) ->
    erlang:list_to_binary(Dir);
to_binary(Dir)
  when erlang:is_binary(Dir) ->
    Dir.

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    LogState = rlx_log:new(error),
    RCLState = new([{log, LogState}], release),
    ?assertMatch(LogState, log(RCLState)).

-endif.
