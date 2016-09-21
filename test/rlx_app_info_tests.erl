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
%%%---------------------------------------------------------------------------
%%% @doc test for target spec parsing
-module(rlx_app_info_tests).

-include_lib("eunit/include/eunit.hrl").
-include("relx.hrl").

name_test() ->
    {ok, AppInfo0} = rlx_app_info:new(),
    AppInfo1 = rlx_app_info:name(AppInfo0, app_name),
    ?assertMatch(app_name,
                 rlx_app_info:name(AppInfo1)).

vsn_test() ->
    {ok, AppInfo0} = rlx_app_info:new(),
    {ok, AppInfo1} = rlx_app_info:vsn(AppInfo0, "1.2.3"),
    ?assertMatch({{1,2,3},{[],[]}},
                 rlx_app_info:vsn(AppInfo1)).

dir_test() ->
    {ok, AppInfo0} = rlx_app_info:new(),
    AppInfo1 = rlx_app_info:dir(AppInfo0, "dir"),
    ?assertMatch("dir",
                 rlx_app_info:dir(AppInfo1)).
