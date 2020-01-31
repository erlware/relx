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
%%% @doc Given a complete built release this provider assembles that release
%%% into a release directory.
-module(rlx_prv_assembler).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("relx.hrl").

-define(PROVIDER, release).
-define(DEPS, [resolve_release]).

%%============================================================================
%% API
%%============================================================================
-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    State1 = rlx_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {deps, ?DEPS},
                                                             {hooks, {[], [overlay]}}])),
    {ok, State1}.

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State) ->
    print_dev_mode(State),
    {RelName, RelVsn} = rlx_state:default_configured_release(State),
    Release = rlx_state:get_realized_release(State, RelName, RelVsn),
    case rlx_release:realized(Release) of
        true ->
            rlx_assemble:do(Release, State);
        false ->
            ?RLX_ERROR({unresolved_release, RelName, RelVsn})
    end.

-spec format_error(ErrorDetail::term()) -> iolist().
format_error({unresolved_release, RelName, RelVsn}) ->
    io_lib:format("The release has not been resolved ~p-~s", [RelName, RelVsn]);
format_error({ec_file_error, AppDir, TargetDir, E}) ->
    io_lib:format("Unable to copy OTP App from ~s to ~s due to ~p",
                  [AppDir, TargetDir, E]);
format_error({vmargs_does_not_exist, Path}) ->
    io_lib:format("The vm.args file specified for this release (~s) does not exist!",
                  [Path]);
format_error({vmargs_src_does_not_exist, Path}) ->
    io_lib:format("The vm.args.src file specified for this release (~s) does not exist!",
                  [Path]);
format_error({config_does_not_exist, Path}) ->
    io_lib:format("The sys.config file specified for this release (~s) does not exist!",
                  [Path]);
format_error({config_src_does_not_exist, Path}) ->
    io_lib:format("The sys.config.src file specified for this release (~s) does not exist!",
                  [Path]);
format_error({sys_config_parse_error, ConfigPath, Reason}) ->
    io_lib:format("The config file (~s) specified for this release could not be opened or parsed: ~s",
                  [ConfigPath, file:format_error(Reason)]);
format_error({specified_erts_does_not_exist, ErtsVersion}) ->
    io_lib:format("Specified version of erts (~s) does not exist",
                  [ErtsVersion]);
format_error({release_script_generation_error, RelFile}) ->
    io_lib:format("Unknown internal release error generating the release file to ~s",
                  [RelFile]);
format_error({release_script_generation_warning, Module, Warnings}) ->
    ["Warnings generating release \s",
     rlx_util:indent(2), Module:format_warning(Warnings)];
format_error({unable_to_create_output_dir, OutputDir}) ->
    io_lib:format("Unable to create output directory (possible permissions issue): ~s",
                  [OutputDir]);
format_error({release_script_generation_error, Module, Errors}) ->
    ["Errors generating release \n",
     rlx_util:indent(2), Module:format_error(Errors)];
format_error({unable_to_make_symlink, AppDir, TargetDir, Reason}) ->
    io_lib:format("Unable to symlink directory ~s to ~s because \n~s~s",
                  [AppDir, TargetDir, rlx_util:indent(2),
                   file:format_error(Reason)]);
format_error(boot_script_generation_error) ->
    "Unknown internal release error generating start_clean.boot";
format_error({boot_script_generation_warning, Module, Warnings}) ->
    ["Warnings generating start_clean.boot \s",
     rlx_util:indent(2), Module:format_warning(Warnings)];
format_error({boot_script_generation_error, Module, Errors}) ->
    ["Errors generating start_clean.boot \n",
     rlx_util:indent(2), Module:format_error(Errors)];
format_error({strip_release, Reason}) ->
    io_lib:format("Stripping debug info from release beam files failed becuase ~s",
                  [beam_lib:format_error(Reason)]);
format_error({rewrite_app_file, AppFile, Error}) ->
    io_lib:format("Unable to rewrite .app file ~s due to ~p",
                  [AppFile, Error]).
%%

print_dev_mode(State) ->
    case rlx_state:dev_mode(State) of
        true ->
            ec_cmd_log:info(rlx_state:log(State),
                            "Dev mode enabled, release will be symlinked");
        false ->
            ok
    end.
