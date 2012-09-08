%%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
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
%%% @doc test for target spec parsing
-module(rclt_goal).

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    ?assertMatch({ok, <<"getopt">>},
                 rcl_goal:parse("getopt")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, '='}},
                 rcl_goal:parse("getopt=0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, '='}},
                 rcl_goal:parse("getopt:0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, '='}},
                 rcl_goal:parse("getopt-0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, gte}},
                 rcl_goal:parse("getopt >= 0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, gte}},
                 rcl_goal:parse("getopt:gte:0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, gt}},
                 rcl_goal:parse("getopt>0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, gt}},
                 rcl_goal:parse("getopt:gt:0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, lte}},
                 rcl_goal:parse("getopt<= 0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, lte}},
                 rcl_goal:parse("getopt:lte:0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, lt}},
                 rcl_goal:parse("getopt<0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, pes}},
                 rcl_goal:parse("getopt ~>0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, pes}},
                 rcl_goal:parse("getopt: pes:0.5.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, {{0,6,1},{[],[]}}, between}},
                 rcl_goal:parse("getopt:btwn:0.5.1,0.6.1")),
    ?assertMatch({ok, {<<"getopt">>, {{0,5,1},{[],[]}}, {{0,6,1},{[],[]}}, between}},
                 rcl_goal:parse("getopt:between :0.5.1,0.6.1")).

fail_test() ->
    ?assertMatch({fail,_},
                 rcl_goal:parse("got:")),
    ?assertMatch({fail,_},
                 rcl_goal:parse("between:btwn:0.5")),
    ?assertMatch({fail,_},
                 rcl_goal:parse("between:btwn:0.5,")).
