%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%%% Copyright 2014 Erlware, LLC. All Rights Reserved.
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
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2014 Erlware, LLC.
%%%
%%% @doc Given a complete built release this provider assembles that release
%%% into a release directory.
-module(rlx_prv_archive).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/2]).

-include("relx.hrl").

-define(PROVIDER, tar).
-define(DEPS, [release]).

%%============================================================================
%% API
%%============================================================================

-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    State1 = rlx_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {bare, false},
                                                             {deps, ?DEPS},
                                                             {example, "tar"},
                                                             {short_desc, ""},
                                                             {desc, ""},
                                                             {opts, []}])),

    {ok, State1}.

-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State) ->
    {RelName, RelVsn} = rlx_state:default_configured_release(State),
    Release = rlx_state:get_realized_release(State, RelName, RelVsn),
    OutputDir = rlx_state:output_dir(State),
    make_tar(State, Release, OutputDir).

format_error({tar_unknown_generation_error, Module, Vsn}, _) ->
    io_lib:format("Tarball generation error of ~s ~s",
                  [Module, Vsn]);
format_error({tar_generation_warn, Module, Warnings}, _) ->
    io_lib:format("Tarball generation warnings for ~p : ~p",
                  [Module, Warnings]);
format_error({tar_generation_error, Module, Errors}, _) ->
    io_lib:format("Tarball generation error for ~p reason ~p",
                  [Module, Errors]).

make_tar(State, Release, OutputDir) ->
    Name = atom_to_list(rlx_release:name(Release)),
    Vsn = rlx_release:vsn(Release),
    ErtsVersion = rlx_release:erts(Release),
    Opts = [{path, [filename:join([OutputDir, "lib", "*", "ebin"])]},
           {outdir, OutputDir} |
            case rlx_state:get(State, include_erts, true) of
                true ->
                    Prefix = code:root_dir(),
                    ErtsDir = filename:join([Prefix]),
                    [{erts, ErtsDir}];
                false ->
                    [];
                Prefix ->
                    ErtsDir = filename:join([Prefix]),
                    [{erts, ErtsDir}]
            end],
    case systools:make_tar(filename:join([OutputDir, "releases", Vsn, Name]),
                           Opts) of
        ok ->
            TempDir = ec_file:insecure_mkdtemp(),
            try
                update_tar(State, TempDir, OutputDir, Name, Vsn, ErtsVersion)
            catch
                E:R ->
                    ec_file:remove(TempDir, [recursive]),
                    ?RLX_ERROR({tar_generation_error, E, R})
            end;
        {ok, Module, Warnings} ->
            ?RLX_ERROR({tar_generation_warn, Module, Warnings});
        error ->
            ?RLX_ERROR({tar_unknown_generation_error, Name, Vsn});
        {error, Module, Errors} ->
            ?RLX_ERROR({tar_generation_error, Module, Errors})
    end.

update_tar(State, TempDir, OutputDir, Name, Vsn, ErtsVersion) ->
    TarFile = filename:join(OutputDir, Name++"-"++Vsn++".tar.gz"),
    file:rename(filename:join(OutputDir, Name++".tar.gz"), TarFile),
    erl_tar:extract(TarFile, [{cwd, TempDir}, compressed]),
    OverlayFiles = overlay_files(rlx_state:get(State, overlay, undefined), OutputDir),
    ok =
        erl_tar:create(TarFile,
                       [{"lib", filename:join(TempDir, "lib")},
                        {"releases", filename:join(TempDir, "releases")},
                        {filename:join(["releases", "start_erl.data"]),
                         filename:join([OutputDir, "releases", "start_erl.data"])},
                        {filename:join(["releases", "RELEASES"]),
                         filename:join([OutputDir, "releases", "RELEASES"])},
                        {filename:join(["releases", Vsn, "vm.args"]),
                         filename:join([OutputDir, "releases", Vsn, "vm.args"])},
                        {"bin", filename:join([OutputDir, "bin"])} |
                        case rlx_state:get(State, include_erts, true) of
                            false ->
                                [];
                            _ ->
                                [{"erts-"++ErtsVersion, filename:join(OutputDir, "erts-"++ErtsVersion)}]
                        end]++OverlayFiles, [compressed]),
    ec_cmd_log:info(rlx_state:log(State),
                 "tarball ~s successfully created!~n", [TarFile]),
    ec_file:remove(TempDir, [recursive]),
    {ok, State}.

overlay_files(undefined, _) ->
    [];
overlay_files(Overlay, OutputDir) ->
    [{to(O), filename:join(OutputDir, to(O))} || O <- Overlay, filter(O)].

to({copy, _, To}) ->
    To;
to({mkdir, To}) ->
    To;
to({template, _, To}) ->
    To.

filter({copy, _, _}) ->
    true;
filter({mkdir, _}) ->
    true;
filter({template, _, _}) ->
    true;
filter(_) ->
    false.
