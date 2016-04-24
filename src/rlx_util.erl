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
         wildcard_paths/1,
         render/1,
         render/2,
         load_file/3,
         template_files/0,
         escript_foldl/3,
         intensity/0,
         symlink_or_copy/2]).

-export([os_type/1]).

-define(DFLT_INTENSITY,   high).
-define(ONE_LEVEL_INDENT, "     ").

-include_lib("kernel/include/file.hrl").
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
    case io_lib:printable_list(Value) of
        true ->
            Value;
        false ->
            ""
    end.

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

-spec render(binary() | iolist()) ->
                {ok, binary()} | {error, render_failed}.
render(Template) ->
    render(Template, []).

-spec render(binary() | iolist(), proplists:proplist()) ->
                {ok, binary()} | {error, render_failed}.
render(Template, Data) when is_list(Template) ->
    render(ec_cnv:to_binary(Template), Data);
render(Template, Data) when is_binary(Template) ->
    case catch bbmustache:render(Template, Data,
                                 [{key_type, atom}]) of
        Bin when is_binary(Bin) -> {ok, Bin};
        _ -> {error, render_failed}
    end.

load_file(Files, escript, Name) ->
    {Name, Bin} = lists:keyfind(Name, 1, Files),
    Bin;
load_file(_Files, file, Name) ->
    {ok, Bin} = file:read_file(Name),
    Bin.

template_files() ->
    find_priv_templates() ++ escript_files().

find_priv_templates() ->
    Files = filelib:wildcard(filename:join([code:priv_dir(relx), "templates", "*"])),
    lists:map(fun(File) ->
                      {ok, Bin} = file:read_file(File),
                      {filename:basename(File), Bin}
              end, Files).

%% Scan the current escript for available files
escript_files() ->
    try
        {ok, Files} = escript_foldl(
                        fun(Name, _, GetBin, Acc) ->
                                [{filename:basename(Name), GetBin()} | Acc]
                        end, [], filename:absname(escript:script_name())),
        Files
    catch
        _:_ ->
            []
    end.

escript_foldl(Fun, Acc, File) ->
    case escript:extract(File, []) of
        {ok, [_Shebang, _Comment, _EmuArgs, Body]} ->
            case Body of
                {source, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {beam, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {archive, ArchiveBin} ->
                    zip:foldl(Fun, Acc, {File, ArchiveBin})
            end;
        {error, _} = Error ->
            Error
    end.

symlink_or_copy(Source, Target) ->
    case file:make_symlink(Source, Target) of
        ok ->
            ok;
        {error, eexist} ->
            {error, eexist};
        {error, eperm} = Error ->
            % We get eperm on Windows if we do not have 
            %   SeCreateSymbolicLinkPrivilege
            % Try the next alternative
            case os:type() of
                {win32, _} ->
                    win32_make_junction_or_copy(Source, Target);
                _ ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

win32_make_junction_or_copy(Source, Target) ->
    case filelib:is_dir(Source) of
        true ->
            win32_make_junction(Source, Target);
        _ ->
            ec_file:copy(Source, Target)
    end.

win32_make_junction(Source, Target) ->
    % The mklink will fail if the target already exists, check for that first
    case file:read_link_info(Target) of
        {error, enoent} ->
            win32_make_junction_cmd(Source, Target);
        {ok, #file_info{type = symlink}} ->
            case file:read_link(Target) of
                {ok, Source} ->
                    ok;
                {ok, _} ->
                    ok = file:del_dir(Target),
                    win32_make_junction_cmd(Source, Target);
                {error, Reason} ->
                    {error, {readlink, Reason}}
            end;
        {ok, #file_info{type = Type}} ->
            {error, {mklink_cannot_replace_existing, Type, Target}};
        Error ->
            Error
    end.

win32_make_junction_cmd(Source, Target) ->
    S = unicode:characters_to_list(Source),
    T = unicode:characters_to_list(Target),
    Cmd = "cmd /c mklink /j " ++ filename:nativename(T) ++ " " ++ filename:nativename(S),
    case os:cmd(Cmd) of
        "Junction created " ++ _ ->
            ok;
        [] ->
            % When mklink fails it prints the error message to stderr which
            % is not picked up by os:cmd() hence this case switch is for
            % an empty message
            {error, make_junction_failed}
    end.

%% @doc Returns the color intensity, we first check the application envorinment
%% if that is not set we check the environment variable RELX_COLOR.
intensity() ->
    case application:get_env(relx, color_intensity) of
        undefined ->
            R = case os:getenv("RELX_COLOR") of
                    "high" ->
                        high;
                    "low" ->
                        low;
                    _ ->
                        ?DFLT_INTENSITY
                end,
            application:set_env(relx, color_intensity, R),
            R;
        {ok, Mode} ->
            Mode
    end.

os_type(State) ->
  case include_erts_is_win32(State) of
    true -> {win32,nt};
    false -> os:type()
  end.

include_erts_is_win32(State) ->
  case rlx_state:get(State, include_erts, true) of
    true -> false;
    false -> false;
    Path -> is_win32_erts(Path,State)
  end.

is_win32_erts(Path,State) ->
  case filelib:wildcard(filename:join([Path,"bin","erl.exe"])) of
    [] -> false;
    _ ->
      ec_cmd_log:info(rlx_state:log(State),
        "Including Erts is win32 ~n", []),
      true
  end.




%%%===================================================================
%%% Test Functions
%%%===================================================================
