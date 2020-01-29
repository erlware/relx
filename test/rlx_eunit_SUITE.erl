%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 92 -*-
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
-module(rlx_eunit_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         all/0,
         depsolver/1,
         goal/1,
         app_info/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [depsolver, goal, app_info].

depsolver(_Config) ->
    ok = eunit:test(rlx_depsolver).

goal(_Config) ->
    ok = eunit:test(rlx_goal).

app_info(_Config) ->
    ok = eunit:test(rlx_app_info).
