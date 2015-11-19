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
-module(rlx_prv_relup).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("relx.hrl").

-define(PROVIDER, relup).
-define(DEPS, [rel_discover, resolve_release]).

%%============================================================================
%% API
%%============================================================================

-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    State1 = rlx_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {deps, ?DEPS}])),
    {ok, State1}.

-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State) ->
    {RelName, RelVsn} = rlx_state:default_configured_release(State),
    Release0 = rlx_state:get_realized_release(State, RelName, RelVsn),
    make_relup(State, Release0).

format_error({relup_generation_error, CurrentName, UpFromName}) ->
    io_lib:format("Unknown internal release error generating the relup from ~s to ~s",
                  [UpFromName, CurrentName]);
format_error({relup_generation_warning, Module, Warnings}) ->
    ["Warnings generating relup \s",
     rlx_util:indent(2), Module:format_warning(Warnings)];
format_error({no_upfrom_release_found, undefined}) ->
    io_lib:format("No earlier release for relup found", []);
format_error({no_upfrom_release_found, Vsn}) ->
    io_lib:format("Upfrom release version (~s) for relup not found", [Vsn]);
format_error({relup_script_generation_error,
              {relup_script_generation_error, systools_relup,
               {missing_sasl, _}}}) ->
    "Unfortunately, due to requirements in systools, you need to have the sasl application "
        "in both the current release and the release to upgrade from.";
format_error({relup_script_generation_error, Module, Errors}) ->
    ["Errors generating relup \n",
     rlx_util:indent(2), Module:format_error(Errors)].

make_relup(State, Release) ->
    Vsn = rlx_state:upfrom(State),
    UpFrom =
        case Vsn of
            undefined ->
                get_last_release(State, Release);
            Vsn ->
                get_up_release(State, Release, Vsn)
        end,
    case UpFrom of
        undefined ->
            ?RLX_ERROR({no_upfrom_release_found, Vsn});
        _ ->
            make_upfrom_script(State, Release, UpFrom)
    end.

get_last_release(State, Release) ->
    Releases0 = [Rel || {{_, _}, Rel} <- ec_dictionary:to_list(rlx_state:realized_releases(State))],
    Releases1 = lists:sort(fun(R1, R2) ->
                                  ec_semver:lte(rlx_release:vsn(R1),
                                                rlx_release:vsn(R2))
                          end, Releases0),
    Res = lists:foldl(fun(_Rel, R = {found, _}) ->
                              R;
                         (Rel, Prev) ->
                              case rlx_release:vsn(Rel) == rlx_release:vsn(Release)  of
                                  true ->
                                      {found, Prev};
                                  false ->
                                      Rel
                              end
                      end, undefined, Releases1),
    case Res of
        {found, R} ->
            R;
        Else ->
            Else
    end.

get_up_release(State, Release, Vsn) ->
    Name = rlx_release:name(Release),
    try
        ec_dictionary:get({Name, Vsn}, rlx_state:realized_releases(State))
    catch
        throw:not_found ->
            undefined
    end.

make_upfrom_script(State, Release, UpFrom) ->
    OutputDir = rlx_state:output_dir(State),
    Options = [{outdir, OutputDir},
               {path, rlx_util:get_code_paths(Release, OutputDir) ++
                   rlx_util:get_code_paths(UpFrom, OutputDir)},
               silent],
    CurrentRel = strip_rel(rlx_release:relfile(Release)),
    UpFromRel = strip_rel(rlx_release:relfile(UpFrom)),
    ec_cmd_log:debug(rlx_state:log(State),
                  "systools:make_relup(~p, ~p, ~p, ~p)",
                  [CurrentRel, UpFromRel, UpFromRel, Options]),
    case rlx_util:make_script(Options,
                     fun(CorrectOptions) ->
                             systools:make_relup(CurrentRel, [UpFromRel], [UpFromRel], CorrectOptions)
                     end) of
        ok ->
            ec_cmd_log:info(rlx_state:log(State),
                          "relup from ~s to ~s successfully created!",
                          [UpFromRel, CurrentRel]),
            {ok, State};
        error ->
            ?RLX_ERROR({relup_script_generation_error, CurrentRel, UpFromRel});
        {ok, RelUp, _, []} ->
            ec_cmd_log:info(rlx_state:log(State),
                          "relup successfully created!"),
            write_relup_file(State, Release, RelUp),
            {ok, State};
        {ok,_, Module,Warnings} ->
            ?RLX_ERROR({relup_script_generation_warn, Module, Warnings});
        {error,Module,Errors} ->
            ?RLX_ERROR({relup_script_generation_error, Module, Errors})
    end.

write_relup_file(State, Release, Relup) ->
    OutDir = rlx_util:release_output_dir(State, Release),
    RelupFile = filename:join(OutDir, "relup"),
    ok = ec_file:write_term(RelupFile, Relup).

strip_rel(Name) ->
    rlx_util:to_string(filename:join(filename:dirname(Name),
                                     filename:basename(Name, ".rel"))).
