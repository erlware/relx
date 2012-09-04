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
%%%
%%% @doc Provides state management services for the relcool tool. Generally,
%%% those things that are fixed have a direct api. Those things that are mutable
%%% have a more mutable api.
-module(rcl_state).

-export([new/1,
        log/1]).

-export_type([state/0]).

-record(?MODULE, {log :: rcl_log:state()}).

%%============================================================================
%% types
%%============================================================================
-opaque state() :: record(?MODULE).

%%============================================================================
%% API
%%============================================================================
%% @doc Create a new 'log level' for the system
-spec new(proplists:proplist()) -> state().
new(PropList) when erlang:is_list(PropList) ->
    #?MODULE{log = proplists:get_value(log, PropList, rcl_log:new(error))}.

%% @doc get the current log state for the system
-spec log(state()) -> rc_log:state().
log(#?MODULE{log=LogState}) ->
    LogState.

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    LogState = rcl_log:new(error),
    RCLState = new([{log, LogState}]),
    ?assertMatch(LogState, log(RCLState)).

-endif.
