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
-module(rlx_config).

%% API
-export([do/1,
         format_error/1]).

-include("relx.hrl").

%% TODO: fill in with typespec for all available config values
-type config() :: term().
-type t() :: [config()].

-export_type([t/0]).

%%%===================================================================
%%% API
%%%===================================================================

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
    io_lib:format("Invalid term in config file: ~p", [Term]);
format_error({failed_to_parse, Goal}) ->
    io_lib:format("Unable to parse goal ~s", [Goal]);
format_error({invalid_goal, Goal}) ->
    io_lib:format("Invalid goal: ~p", [Goal]).

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

-spec config_script_file(file:filename(), rlx_state:t()) -> file:filename().
config_script_file(ConfigFile, _State) ->
    ConfigFile ++ ".script".

bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

-spec apply_config_script(proplists:proplist(), file:filename()) ->
                                proplists:proplist().
apply_config_script(ConfigData, ConfigScriptFile) ->
    {ok, Config} = file:script(ConfigScriptFile, bs([{'CONFIG', ConfigData},
                                                     {'SCRIPT', ConfigScriptFile}])),
    Config.

-spec load_config(file:filename() | proplists:proplist(), rlx_state:t()) ->
                         {ok, rlx_state:t()} | relx:error().
load_config(ConfigFile, State) ->
    {ok, CurrentCwd} = file:get_cwd(),
    CliTerms = rlx_state:cli_args(State),
    Config0 = case filelib:is_regular(ConfigFile) of
                true ->
                    ok = file:set_cwd(filename:dirname(ConfigFile)),
                    Result = case file:consult(ConfigFile) of
                                {error, Reason} ->
                                    ?RLX_ERROR({consult, ConfigFile, Reason});
                                {ok, Terms} -> merge_configs(CliTerms, Terms)
                             end,
                    ok = file:set_cwd(CurrentCwd),
                    Result;
                false -> merge_configs(CliTerms, ConfigFile)
              end,
    % we now take the merged config and try to apply a config script to it,
    % get a new config as a result
    case Config0 of
        {error, _} = Error -> Error;
        _ ->
            ConfigScriptFile = config_script_file(ConfigFile, State),
            Config1 = case filelib:is_regular(ConfigScriptFile) of
                        false -> Config0;
                        true -> apply_config_script(Config0, ConfigScriptFile)
                      end,
            lists:foldl(fun rlx_config_terms:load/2, {ok, State}, Config1)
    end.

merge_configs([], ConfigTerms) ->
    ConfigTerms;
merge_configs([{_Key, undefined} | CliTerms], ConfigTerms) ->
    merge_configs(CliTerms, ConfigTerms);
merge_configs([{_Key, []} | CliTerms], ConfigTerms) ->
    merge_configs(CliTerms, ConfigTerms);
merge_configs([{Key, Value} | CliTerms], ConfigTerms) ->
    case Key of
        X when X =:= lib_dirs
             ; X =:= goals
             ; X =:= overrides ->
            case lists:keyfind(Key, 1, ConfigTerms) of
                {Key, Value2} ->
                    MergedValue = lists:umerge([Value, Value2]),
                    merge_configs(CliTerms, lists:keyreplace(Key, 1, ConfigTerms, {Key, MergedValue}));
                false ->
                    merge_configs(CliTerms, ConfigTerms++[{Key, Value}])
            end;
        overlay_vars ->
            case lists:keyfind(overlay_vars, 1, ConfigTerms) of
                {_, [H | _] = Vars} when is_list(H) ;
                                         is_tuple(H) ->
                    MergedValue = Vars ++ Value,
                    merge_configs(CliTerms, lists:keyreplace(overlay_vars, 1, ConfigTerms, {Key, MergedValue}));
                {_, Vars} when is_list(Vars) ->
                    MergedValue = [Vars | Value],
                    merge_configs(CliTerms, lists:keyreplace(overlay_vars, 1, ConfigTerms, {Key, MergedValue}));
                false ->
                    merge_configs(CliTerms, ConfigTerms++[{Key, Value}])
            end;
        default_release when Value =:= {undefined, undefined} ->
            %% No release specified in cli. Prevent overwriting default_release in ConfigTerms.
            merge_configs(CliTerms, lists:keymerge(1, ConfigTerms, [{Key, Value}]));
        _ ->
            merge_configs(CliTerms, lists:reverse(lists:keystore(Key, 1, lists:reverse(ConfigTerms), {Key, Value})))
    end.
