%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the  License.  You may obtain
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
%%% @doc
%%%  Utilities to help with parsing of target specs
-module(rcl_goal_utils).

-export([to_op/1,
         to_vsn/1]).

%%============================================================================
%% types
%%============================================================================

%%============================================================================
%% API
%%============================================================================
-spec to_op(iolist()) -> rcl_depsolver:constraint_op().
to_op(List)
  when erlang:is_list(List) ->
    to_op(erlang:iolist_to_binary(List));
to_op(<<"=">>) ->
    '=';
to_op(<<":">>) ->
    '=';
to_op(<<"-">>) ->
    '=';
to_op(<<"<">>) ->
    'lt';
to_op(<<":lt:">>) ->
    'lt';
to_op(<<"<=">>) ->
    'lte';
to_op(<<":lte:">>) ->
    'lte';
to_op(<<">">>) ->
    'gt';
to_op(<<":gt:">>) ->
    'gt';
to_op(<<">=">>) ->
    'gte';
to_op(<<":gte:">>) ->
    'gte';
to_op(<<"~>">>) ->
    'pes';
to_op(<<":pes:">>) ->
    'pes';
to_op(<<":btwn:">>) ->
    'between';
to_op(<<":between:">>) ->
    'between'.

to_vsn(Version) when erlang:is_list(Version) ->
    to_vsn(erlang:iolist_to_binary(Version));
to_vsn(Vsn) ->
    rcl_depsolver:parse_version(Vsn).

%%%===================================================================
%%% Test Functions
%%%===================================================================
