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
%%% @doc Trivial utility file to help handle common tasks
-module(rlx_util).

-export([get_code_paths/2,
         release_output_dir/2,
         make_script/2,
         mkdir_p/1,
         to_binary/1,
         to_string/1,
         to_atom/1,
         is_error/1,
         error_reason/1,
         indent/1,
         optional_to_string/1,
         wildcard_paths/1]).

-define(ONE_LEVEL_INDENT, "     ").
%%============================================================================
%% types
%%============================================================================


%%============================================================================
%% API
%%============================================================================

%% @doc Generates the correct set of code paths for the system.
-spec get_code_paths(rlx_release:t(), file:name()) -> [file:name()].
get_code_paths(Release, OutDir) ->
    LibDir = filename:join(OutDir, "lib"),
    [filename:join([LibDir,
                    erlang:atom_to_list(rlx_app_info:name(App)) ++ "-" ++
                        rlx_app_info:original_vsn(App), "ebin"]) ||
        App <- rlx_release:application_details(Release)].

-spec release_output_dir(rlx_state:t(), rlx_release:t()) -> string().
release_output_dir(State, Release) ->
    OutputDir = rlx_state:output_dir(State),
    filename:join([OutputDir,
                   "releases",
                   rlx_release:vsn(Release)]).

-spec make_script([term()],
                  fun(([term()]) -> Res)) -> Res.
make_script(Options, RunFun) ->
    %% Erts 5.9 introduced a non backwards compatible option to
    %% erlang this takes that into account
    Erts = erlang:system_info(version),
    case ec_semver:gte(Erts, "5.9") of
        true ->
            RunFun([no_warn_sasl | Options]);
        _ ->
            RunFun(Options)
    end.

%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_p(string()) -> ok | {error, Reason::file:posix()}.
mkdir_p(Path) ->
    %% We are exploiting a feature of ensuredir that that creates all
    %% directories up to the last element in the filename, then ignores
    %% that last element. This way we ensure that the dir is created
    %% and not have any worries about path names
    DirName = filename:join([filename:absname(Path), "tmp"]),
    filelib:ensure_dir(DirName).

%% @doc ident to the level specified
-spec indent(non_neg_integer()) -> iolist().
indent(Amount) when erlang:is_integer(Amount) ->
    [?ONE_LEVEL_INDENT || _ <- lists:seq(1, Amount)].

-spec to_binary(iolist() | binary()) -> binary().
to_binary(String) when erlang:is_list(String) ->
    erlang:iolist_to_binary(String);
to_binary(Bin) when erlang:is_binary(Bin) ->
    Bin.

-spec to_string(binary() | string() | atom()) -> string().
to_string(Binary) when erlang:is_binary(Binary) ->
    erlang:binary_to_list(Binary);
to_string(Atom) when erlang:is_atom(Atom) ->
    erlang:atom_to_list(Atom);
to_string(Else) when erlang:is_list(Else) ->
    Else.

-spec to_atom(atom() | string() | binary()) -> atom().
to_atom(Binary)
  when erlang:is_binary(Binary) ->
    erlang:list_to_atom(to_string(Binary));
to_atom(String)
  when erlang:is_list(String) ->
    erlang:list_to_atom(String);
to_atom(Atom)
  when erlang:is_atom(Atom) ->
    Atom.

%% @doc get the reason for a particular relx error
-spec error_reason(relx:error()) -> any().
error_reason({error, {_, Reason}}) ->
    Reason.
%% @doc check to see if the value is a relx error
-spec is_error(relx:error() | any()) -> boolean().
is_error({error, _}) ->
    true;
is_error(_) ->
    false.

%% @doc convert optional argument to empty string if undefined
optional_to_string(undefined) ->
    "";
optional_to_string(Value) when is_list(Value) ->
    Value.

%% @doc expand wildcards and names in the given paths
-spec wildcard_paths([file:filename_all()]) -> [string()].
wildcard_paths(Paths) ->
    [filename:absname(Expanded) || Path <- Paths, Expanded <- wildcard(Path)].

%% In case the given directory does not expand,
%% we return it back in a list so we trigger the
%% proper error reportings.
-spec wildcard(file:filename_all()) -> [string()].
wildcard(Path) when is_binary(Path) ->
    wildcard(binary_to_list(Path));
wildcard(Path) when is_list(Path) ->
    case filelib:wildcard(Path) of
        []   -> [Path];
        Paths -> Paths
    end.

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
