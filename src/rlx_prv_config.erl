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
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  A module that provides config parsing and support to the system
%%% @end
%%%-------------------------------------------------------------------
-module(rlx_prv_config).

-behaviour(rlx_provider).

%% API
-export([init/1,
         do/1,
         format_error/1]).

-include("relx.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Required by the system, but not used in this provider
-spec init(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
init(State) ->
    {ok, State}.

%% @doc parse all the configs currently specified in the state,
%% populating the state as a result.
-spec do(rlx_state:t()) ->{ok,  rlx_state:t()} | relx:error().
do(State) ->
    case rlx_state:config_file(State) of
        [] ->
            search_for_dominating_config(State);
        undefined ->
            search_for_dominating_config(State);
        ConfigFile when erlang:is_list(ConfigFile) ->
            load_config(ConfigFile, State)
    end.

-spec format_error(Reason::term()) -> iolist().
format_error({consult, ConfigFile, Reason}) ->
    io_lib:format("Unable to read file ~s: ~s", [ConfigFile,
                                                 file:format_error(Reason)]);
format_error({invalid_term, Term}) ->
    io_lib:format("Invalid term in config file: ~p", [Term]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
search_for_dominating_config({ok, Cwd}) ->
    ConfigFile = filename:join(Cwd, "relx.config"),
    case ec_file:exists(ConfigFile) of
        true ->
            {ok, ConfigFile};
        false ->
            search_for_dominating_config(parent_dir(Cwd))
    end;
search_for_dominating_config({error, _}) ->
    no_config;
search_for_dominating_config(State0) ->
    {ok, Cwd} = file:get_cwd(),
    case search_for_dominating_config({ok, Cwd}) of
        {ok, Config} ->
            %% we need to set the root dir on state as well
            {ok, RootDir} = parent_dir(Config),
            State1 = rlx_state:root_dir(State0, RootDir),
            load_config(Config, rlx_state:config_file(State1, Config));
        no_config ->
            {ok, State0}
    end.

%% @doc Given a directory returns the name of the parent directory.
-spec parent_dir(Filename::string()) ->
                        {ok, DirName::string()} | {error, no_parent_dir}.
parent_dir(Filename) ->
    parent_dir(filename:split(Filename), []).

%% @doc Given list of directories, splits the list and returns all dirs but the
%%  last as a path.
-spec parent_dir([string()], [string()]) ->
                        {ok, DirName::string()} | {error, no_parent_dir}.
parent_dir([_H], []) ->
    {error, no_parent_dir};
parent_dir([], []) ->
    {error, no_parent_dir};
parent_dir([_H], Acc) ->
    {ok, filename:join(lists:reverse(Acc))};
parent_dir([H | T], Acc) ->
    parent_dir(T, [H | Acc]).

-spec load_config(file:filename(), rlx_state:t()) ->
                         {ok, rlx_state:t()} | relx:error().
load_config(ConfigFile, State) ->
    {ok, CurrentCwd} = file:get_cwd(),
    ok = file:set_cwd(filename:dirname(ConfigFile)),
    Result = case file:consult(ConfigFile) of
                 {error, Reason} ->
                     ?RLX_ERROR({consult, ConfigFile, Reason});
                 {ok, Terms} ->
                     lists:foldl(fun load_terms/2, {ok, State}, Terms)
             end,
    ok = file:set_cwd(CurrentCwd),
    Result.

-spec load_terms(term(), {ok, rlx_state:t()} | relx:error()) ->
                        {ok, rlx_state:t()} | relx:error().
load_terms({default_release, RelName, RelVsn}, {ok, State}) ->
    {ok, rlx_state:default_configured_release(State, RelName, RelVsn)};
load_terms({paths, Paths}, {ok, State}) ->
    code:add_pathsa([filename:absname(Path) || Path <- Paths]),
    {ok, State};
load_terms({lib_dirs, Dirs}, {ok, State}) ->
    State2 =
        rlx_state:add_lib_dirs(State,
                               [list_to_binary(filename:absname(Dir)) || Dir <- Dirs]),
    {ok, State2};
load_terms({providers, Providers0}, {ok, State0}) ->
    Providers1 = gen_providers(Providers0, State0),
    case Providers1 of
        {_, E={error, _}} ->
            E;
        {Providers3, {ok, State3}} ->
            {ok, rlx_state:providers(State3, Providers3)}
    end;
load_terms({add_providers, Providers0}, {ok, State0}) ->
    Providers1 = gen_providers(Providers0, State0),
    case Providers1 of
        {_, E={error, _}} ->
            E;
        {Providers3, {ok, State1}} ->
            ExistingProviders = rlx_state:providers(State1),
            {ok, rlx_state:providers(State1, ExistingProviders ++ Providers3)}
    end;
load_terms({skip_apps, SkipApps0}, {ok, State0}) ->
    {ok, rlx_state:skip_apps(State0, SkipApps0)};
load_terms({overrides, Overrides0}, {ok, State0}) ->
    {ok, rlx_state:overrides(State0, Overrides0)};
load_terms({dev_mode, DevMode}, {ok, State0}) ->
    {ok, rlx_state:dev_mode(State0, DevMode)};
load_terms({include_src, IncludeSrc}, {ok, State0}) ->
    {ok, rlx_state:include_src(State0, IncludeSrc)};
load_terms({release, {RelName, Vsn, {extend, RelName2}}, Applications}, {ok, State0}) ->
    Release0 = rlx_release:new(RelName, Vsn),
    ExtendRelease = rlx_state:get_configured_release(State0, RelName2, Vsn),
    Applications1 = rlx_release:goals(ExtendRelease),
    case rlx_release:goals(Release0,
                          lists:umerge(lists:usort(Applications),
                                      lists:usort(Applications1))) of
        E={error, _} ->
            E;
        {ok, Release1} ->
            {ok, rlx_state:add_configured_release(State0, Release1)}
        end;
load_terms({release, {RelName, Vsn}, Applications}, {ok, State0}) ->
    Release0 = rlx_release:new(RelName, Vsn),
    case rlx_release:goals(Release0, Applications) of
        E={error, _} ->
            E;
        {ok, Release1} ->
            {ok, rlx_state:add_configured_release(State0, Release1)}
        end;
load_terms({release, {RelName, Vsn}, {erts, ErtsVsn},
            Applications}, {ok, State}) ->
    Release0 = rlx_release:erts(rlx_release:new(RelName, Vsn), ErtsVsn),
    case rlx_release:goals(Release0, Applications) of
        E={error, _} ->
            E;
        {ok, Release1} ->
            {ok, rlx_state:add_configured_release(State, Release1)}
    end;
load_terms({vm_args, VmArgs}, {ok, State}) ->
    {ok, rlx_state:vm_args(State, filename:absname(VmArgs))};
load_terms({sys_config, SysConfig}, {ok, State}) ->
    {ok, rlx_state:sys_config(State, filename:absname(SysConfig))};
load_terms({overlay_vars, OverlayVars}, {ok, State}) ->
    CurrentOverlayVars = rlx_state:get(State, overlay_vars),
    NewOverlayVars = list_of_overlay_vars_files(OverlayVars),
    NewOverlayVars = lists:umerge(lists:usort(NewOverlayVars), lists:usort(CurrentOverlayVars)),
    {ok, rlx_state:put(State, overlay_vars, NewOverlayVars)};
load_terms({Name, Value}, {ok, State})
  when erlang:is_atom(Name) ->
    {ok, rlx_state:put(State, Name, Value)};
load_terms(_, Error={error, _}) ->
    Error;
load_terms(InvalidTerm, _) ->
    ?RLX_ERROR({invalid_term, InvalidTerm}).

-spec gen_providers([module()], rlx_state:t()) ->
                           {[rlx_provider:t()], {ok, rlx_state:t()} | relx:error()}.
gen_providers(Providers, State) ->
    lists:foldl(fun(ProviderName, {Providers1, {ok, State1}}) ->
                        {Provider, State2} = rlx_provider:new(ProviderName, State1),
                        {[Provider | Providers1], State2};
                   (_, E={_, {error, _}}) ->
                        E
                end, {[], {ok, State}}, Providers).

list_of_overlay_vars_files(undefined) ->
    [];
list_of_overlay_vars_files([]) ->
    [];
list_of_overlay_vars_files([H | _]=FileNames) when erlang:is_list(H) ->
    FileNames;
list_of_overlay_vars_files(FileName) when is_list(FileName) ->
    [FileName].
