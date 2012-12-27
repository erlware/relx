%%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
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
%%% @doc Provides state management services for the relcool tool. Generally,
%%% those things that are fixed have a direct api. Those things that are mutable
%%% have a more mutable api.
-module(rcl_state).

-export([new/2,
         log/1,
         output_dir/1,
         lib_dirs/1,
         overrides/1,
         overrides/2,
         goals/1,
         config_files/1,
         config_files/2,
         providers/1,
         providers/2,
         sys_config/1,
         sys_config/2,
         root_dir/1,
         root_dir/2,
         add_release/2,
         get_release/3,
         update_release/2,
         releases/1,
         default_release/1,
         default_release/3,
         available_apps/1,
         available_apps/2,
         get/2,
         get/3,
         put/3,
         caller/1,
         caller/2,
         format/1,
         format/2]).


-export_type([t/0,
              releases/0,
              cmd_args/0]).

-record(state_t, {log :: rcl_log:t(),
                  root_dir :: file:name(),
                  caller :: caller(),
                  output_dir :: file:name(),
                  lib_dirs=[] :: [file:name()],
                  config_files=[] :: [file:filename()],
                  goals=[] :: [rcl_depsolver:constraint()],
                  providers = [] :: [rcl_provider:t()],
                  available_apps = [] :: [rcl_app_info:t()],
                  default_release :: {rcl_release:name(), rcl_release:vsn()},
                  sys_config :: file:filename() | undefined,
                  overrides :: [{AppName::atom(), Directory::file:filename()}],
                  releases :: ec_dictionary:dictionary({ReleaseName::atom(),
                                                        ReleaseVsn::string()},
                                                       rcl_release:t()),
                  config_values :: ec_dictionary:dictionary(Key::atom(),
                                                            Value::term())}).

%%============================================================================
%% types
%%============================================================================

-type releases() :: ec_dictionary:dictionary({rcl_release:name(),
                                              rcl_release:vsn()},
                                             rcl_release:t()).
-type cmd_args() :: proplists:proplist().
-type caller() :: command_line | api.
-opaque t() :: record(state_t).

%%============================================================================
%% API
%%============================================================================
%% @doc Create a new 'log level' for the system
-spec new(proplists:proplist(), [file:filename()] | file:filename()) -> t().
new(PropList, Targets) when erlang:is_list(PropList) ->
    {ok, Root} = file:get_cwd(),
    State0 =
        #state_t{log = proplists:get_value(log, PropList, rcl_log:new(error)),
                 output_dir=proplists:get_value(output_dir, PropList, ""),
                 lib_dirs=get_lib_dirs(proplists:get_value(lib_dirs, PropList, [])),
                 config_files=process_config_files(Targets),
                 goals=proplists:get_value(goals, PropList, []),
                 providers = [],
                 releases=ec_dictionary:new(ec_dict),
                 config_values=ec_dictionary:new(ec_dict),
                 overrides = proplists:get_value(overrides, PropList, []),
                 root_dir = proplists:get_value(root_dir, PropList, Root),
                 default_release={proplists:get_value(relname, PropList, undefined),
                                  proplists:get_value(relvsn, PropList, undefined)}},
    create_logic_providers(State0).

%% @doc the application overrides for the system
-spec overrides(t()) -> [{AppName::atom(), Directory::file:filename()}].
overrides(#state_t{overrides=Overrides}) ->
    Overrides.

%% @doc the application overrides for the system
-spec overrides(t(), [{AppName::atom(), Directory::file:filename()}]) -> t().
overrides(State, Overrides) ->
    State#state_t{overrides=Overrides}.

%% @doc get the current log state for the system
-spec log(t()) -> rcl_log:t().
log(#state_t{log=LogState}) ->
    LogState.

-spec output_dir(t()) -> file:name().
output_dir(#state_t{output_dir=OutDir}) ->
    OutDir.

-spec lib_dirs(t()) -> [file:name()].
lib_dirs(#state_t{lib_dirs=LibDir}) ->
    LibDir.

-spec goals(t()) -> [rcl_depsolver:constraint()].
goals(#state_t{goals=TS}) ->
    TS.

-spec config_files(t()) -> [file:filename()].
config_files(#state_t{config_files=ConfigFiles}) ->
    ConfigFiles.

-spec config_files(t(), [file:filename()]) -> t().
config_files(State, ConfigFiles) ->
    State#state_t{config_files=ConfigFiles}.

-spec providers(t()) -> [rcl_provider:t()].
providers(#state_t{providers=Providers}) ->
    Providers.

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

-spec providers(t(), [rcl_provider:t()]) -> t().
providers(M, NewProviders) ->
    M#state_t{providers=NewProviders}.

-spec add_release(t(), rcl_release:t()) -> t().
add_release(M=#state_t{releases=Releases}, Release) ->
    M#state_t{releases=ec_dictionary:add({rcl_release:name(Release),
                                          rcl_release:vsn(Release)},
                                         Release,
                                         Releases)}.

-spec update_release(t(), rcl_release:t()) -> t().
update_release(M=#state_t{releases=Releases}, Release) ->
    M#state_t{releases=ec_dictionary:add({rcl_release:name(Release),
                                          rcl_release:vsn(Release)},
                                         Release,
                                         Releases)}.

-spec get_release(t(), rcl_release:name(), rcl_release:vsn()) -> rcl_release:t().
get_release(#state_t{releases=Releases}, Name, Vsn) ->
    ec_dictionary:get({Name, Vsn}, Releases).

-spec releases(t()) -> releases().
releases(#state_t{releases=Releases}) ->
    Releases.

-spec default_release(t()) ->
                             {rcl_release:name() | undefined, rcl_release:vsn() | undefined}.
default_release(#state_t{default_release=Def}) ->
    Def.

-spec default_release(t(), rcl_release:name(), rcl_release:vsn()) -> t().
default_release(M, Name, Vsn) ->
    M#state_t{default_release={Name, Vsn}}.

-spec available_apps(t()) -> [rcl_app_info:t()].
available_apps(#state_t{available_apps=Apps}) ->
    Apps.

-spec available_apps(t(), [rcl_app_info:t()]) -> t().
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

-spec format(t()) -> iolist().
format(Mod) ->
    format(Mod, 0).

-spec format(t(), non_neg_integer()) -> iolist().
format(#state_t{log=LogState, output_dir=OutDir, lib_dirs=LibDirs,
                caller=Caller, config_values=Values0,
                goals=Goals, config_files=ConfigFiles,
                providers=Providers},
       Indent) ->
    Values1 = ec_dictionary:to_list(Values0),
    [rcl_util:indent(Indent),
     <<"state(">>, erlang:atom_to_list(Caller), <<"):\n">>,
     rcl_util:indent(Indent + 1), <<"log: ">>, rcl_log:format(LogState), <<",\n">>,
     rcl_util:indent(Indent + 1), "config files: \n",
     [[rcl_util:indent(Indent + 2), ConfigFile, ",\n"] || ConfigFile <- ConfigFiles],
     rcl_util:indent(Indent + 1), "goals: \n",
     [[rcl_util:indent(Indent + 2), rcl_depsolver:format_constraint(Goal), ",\n"] || Goal <- Goals],
     rcl_util:indent(Indent + 1), "output_dir: ", OutDir, "\n",
     rcl_util:indent(Indent + 1), "lib_dirs: \n",
     [[rcl_util:indent(Indent + 2), LibDir, ",\n"] || LibDir <- LibDirs],
     rcl_util:indent(Indent + 1), "providers: \n",
     [[rcl_util:indent(Indent + 2), rcl_provider:format(Provider), ",\n"] || Provider <- Providers],
     rcl_util:indent(Indent + 1), "provider config values: \n",
     [[rcl_util:indent(Indent + 2), io_lib:format("~p", [Value]), ",\n"] || Value <- Values1]].

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec get_lib_dirs([file:name()]) -> [file:name()].
get_lib_dirs(CmdDirs) ->
    case os:getenv("ERL_LIBS") of
        false ->
            CmdDirs;
        EnvString ->
            [Lib || Lib <- re:split(EnvString, ":|;"),
                     filelib:is_dir(Lib)] ++ CmdDirs
    end.

-spec create_logic_providers(t()) -> t().
create_logic_providers(State0) ->
    {ConfigProvider, {ok, State1}} = rcl_provider:new(rcl_prv_config, State0),
    {DiscoveryProvider, {ok, State2}} = rcl_provider:new(rcl_prv_discover, State1),
    {ReleaseProvider, {ok, State3}} = rcl_provider:new(rcl_prv_release, State2),
    {OverlayProvider, {ok, State4}} = rcl_provider:new(rcl_prv_overlay, State3),
    {AssemblerProvider, {ok, State5}} = rcl_provider:new(rcl_prv_assembler, State4),
    State5#state_t{providers=[ConfigProvider, DiscoveryProvider,
                              ReleaseProvider, OverlayProvider, AssemblerProvider]}.


%% @doc config files can come in as either a single file name or as a list of
%% files. We what to support both where possible.
process_config_files(File = [Char | _])
  when erlang:is_integer(Char) ->
    [File];
process_config_files(Files = [File | _])
  when erlang:is_list(File) ->
    Files;
process_config_files([]) ->
    [].

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    LogState = rcl_log:new(error),
    RCLState = new([{log, LogState}], []),
    ?assertMatch(LogState, log(RCLState)).

-endif.
