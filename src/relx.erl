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
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012 Erlware, LLC.
%%% @doc
-module(relx).

-export([release/3,
         build_release/3,
         build_relup/4,
         format_error/1]).

-include("relx.hrl").

-type error() :: {error, {Module::module(), Reason::term()}}.
-type config_goal() :: rlx_release:name() |
                       {rlx_release:name(), rlx_release:vsn() | rlx_release:type()} |
                       {rlx_release:name(), rlx_release:vsn(), rlx_release:type() | rlx_release:incl_apps()} |
                       {rlx_release:name(), rlx_release:vsn(), rlx_release:type(), rlx_release:incl_apps()}.

-export_type([config_goal/0,
              error/0]).

-type release() :: #{name := atom(),
                     vsn := string(),
                     applications := list(),
                     relfile_path := file:filename_all() | undefined}.

-spec release(rlx_release:name(), rlx_release:vsn(), [config_goal()]) -> release().
release(Name, Vsn, Goals) ->
    #{name => Name,
      vsn => Vsn,
      applications => Goals,
      relfile_path => undefined}.

-spec build_release(release(), [rlx_app:t()], rlx_config:t()) -> ok | {error, term()}.
build_release(_Release, Apps, Config) ->
    State = rlx_state:new(),
    {ok, State1} = rlx_config_terms:to_state(Config, State),
    {ok, State2} = rlx_prv_release:do(rlx_state:available_apps(State1, Apps)),
    {ok, State3} = rlx_prv_assembler:do(State2),
    rlx_prv_overlay:do(State3).

-spec build_relup(release(), release(), [rlx_app:t()], rlx_config:t()) -> ok | {error, term()}.
build_relup(_Release1, _Release2, _Apps, _Config) ->
    ok.

-spec format_error(Reason::term()) -> string().
format_error({error, {relx, Reason}}) ->
    format_error(Reason);
format_error({error, {Module, Reason}}) ->
    io_lib:format("~s~n", [Module:format_error(Reason)]).

