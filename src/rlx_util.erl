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

-define(DFLT_INTENSITY,   high).
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

render(Template) ->
    render(Template, []).

render(Template, Data) ->
    {ok, bbmustache:render(ec_cnv:to_binary(Template), Data, [{key_type, atom}])}.

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
            ok;
        {error, _} ->
            case os:type() of
                {win32, _} ->
                    S = unicode:characters_to_list(Source),
                    T = unicode:characters_to_list(Target),
                    win32_symlink(filename:nativename(S), filename:nativename(T));
                _ ->
                    case filelib:is_dir(Target) of
                        true -> ok;
                        false ->
                            cp_r([Source], Target)
                    end
            end
    end.


win32_symlink(Source, Target) ->
    os:cmd("cmd /c mklink /j " ++ Target ++ " " ++ Source),
    ok.

-spec cp_r(list(string()), file:filename()) -> 'ok'.
cp_r([], _Dest) ->
    ok;
cp_r(Sources, Dest) ->
    case os:type() of
        {unix, _} ->
            ok;
        {win32, _} ->
            lists:foreach(fun(Src) -> ok = cp_r_win32(Src,Dest) end, Sources),
            ok
    end.

xcopy_win32(Source,Dest)->
    %% "xcopy \"~s\" \"~s\" /q /y /e 2> nul", Chanegd to robocopy to
    %% handle long names. May have issues with older windows.
    os:cmd("robocopy " ++ Source ++ " " ++ Dest ++ " /e /is"),
    ok.

cp_r_win32({true, SourceDir}, {true, DestDir}) ->
    %% from directory to directory
     ok = case file:make_dir(DestDir) of
             {error, eexist} -> ok;
             Other -> Other
         end,
    ok = xcopy_win32(SourceDir, DestDir);
cp_r_win32({false, Source} = S,{true, DestDir}) ->
    %% from file to directory
    cp_r_win32(S, {false, filename:join(DestDir, filename:basename(Source))});
cp_r_win32({false, Source},{false, Dest}) ->
    %% from file to file
    {ok,_} = file:copy(Source, Dest),
    ok;
cp_r_win32({true, SourceDir}, {false, DestDir}) ->
    case filelib:is_regular(DestDir) of
        true ->
            %% From directory to file? This shouldn't happen
            {error, lists:flatten(
                      io_lib:format("Cannot copy dir (~p) to file (~p)\n",
                                    [SourceDir, DestDir]))};
        false ->
            %% Specifying a target directory that doesn't currently exist.
            %% So let's attempt to create this directory
            case filelib:ensure_dir(filename:join(DestDir, "dummy")) of
                ok ->
                    ok = xcopy_win32(SourceDir, DestDir);
                {error, Reason} ->
                    {error, lists:flatten(
                              io_lib:format("Unable to create dir ~p: ~p\n",
                                            [DestDir, Reason]))}
            end
    end;
cp_r_win32(Source,Dest) ->
    Dst = {filelib:is_dir(Dest), Dest},
    lists:foreach(fun(Src) ->
                          ok = cp_r_win32({filelib:is_dir(Src), Src}, Dst)
                  end, filelib:wildcard(Source)),
    ok.

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
        Mode ->
            Mode
    end.

%%%===================================================================
%%% Test Functions
%%%===================================================================
