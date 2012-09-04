%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
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
%%% @doc This provides simple output functions for relcool. You should use this
%%% to talk to the users if you are wrting code for the system
-module(rcl_log).

-export([new/1,
         log/4,
         should/2,
         debug/2,
         debug/3,
         info/2,
         info/3,
         error/2,
         error/3,
         log_level/1,
         atom_log_level/1]).

-export_type([int_log_level/0,
              log_level/0,
              state/0]).

-include_lib("relcool/include/relcool.hrl").

%%============================================================================
%% types
%%============================================================================

-type int_log_level() :: 0..2.
%% Why no warn? because for our purposes there is no difference between error
%% and warn
-type log_level() :: error | info | debug.
-opaque state() :: {?MODULE, log_level()}.

%%============================================================================
%% API
%%============================================================================
%% @doc Create a new 'log level' for the system
-spec new(int_log_level() | log_level()) -> state().
new(LogLevel) when LogLevel >= 0, LogLevel =< 2 ->
    {?MODULE, LogLevel};
new(AtomLogLevel)
  when AtomLogLevel =:= error;
       AtomLogLevel =:= info;
       AtomLogLevel =:= debug ->
    LogLevel = case AtomLogLevel of
                   error -> 0;
                   info -> 1;
                   debug -> 2
               end,
    new(LogLevel).

%% @doc log at the debug level given the current log state with a string
-spec debug(state(), string()) -> none().
debug(LogState, String) ->
    debug(LogState, "~s~n", [String]).

%% @doc log at the debug level given the current log state with a format string
%% and argements @see io:format/2
-spec debug(state(), string(), [any()]) -> none().
debug(LogState, FormatString, Args) ->
    log(LogState, ?RCL_DEBUG, FormatString, Args).

%% @doc log at the info level given the current log state with a string
-spec info(state(), string()) -> none().
info(LogState, String) ->
    info(LogState, "~s~n", [String]).

%% @doc log at the info level given the current log state with a format string
%% and argements @see io:format/2
-spec info(state(), string(), [any()]) -> none().
info(LogState, FormatString, Args) ->
    log(LogState, ?RCL_INFO, FormatString, Args).

%% @doc log at the error level given the current log state with a string
-spec error(state(), string()) -> none().
error(LogState, String) ->
    error(LogState, "~s~n", [String]).

%% @doc log at the error level given the current log state with a format string
%% and argements @see io:format/2
-spec error(state(), string(), [any()]) -> none().
error(LogState, FormatString, Args) ->
    log(LogState, ?RCL_ERROR, FormatString, Args).

%% @doc when the module log level is less then or equal to the log level for the
%% call then write the log info out. When its not then ignore the call.
-spec log(state(), int_log_level(), string(), [any()]) -> ok.
log({?MODULE, DetailLogLevel}, LogLevel, FormatString, Args)
  when DetailLogLevel =< LogLevel,
       erlang:is_list(Args) ->
    io:format(FormatString, Args);
log(_, _, _, _) ->
    ok.

%% @doc return a boolean indicating if the system should log for the specified
%% levelg
-spec should(state(), int_log_level()) -> ok.
should({?MODULE, DetailLogLevel}, LogLevel)
  when DetailLogLevel >= LogLevel ->
    true;
should(_, _) ->
    false.

%% @doc get the current log level as an integer
-spec log_level(state()) -> int_log_level().
log_level({?MODULE, DetailLogLevel}) ->
    DetailLogLevel.

%% @doc get the current log level as an atom
-spec atom_log_level(state()) -> log_level().
atom_log_level({?MODULE, ?RCL_ERROR}) ->
    error;
atom_log_level({?MODULE, ?RCL_INFO}) ->
    info;
atom_log_level({?MODULE, ?RCL_DEBUG}) ->
    debug.

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

should_test() ->
    ErrorLogState = new(error),
    ?assert(should(ErrorLogState, ?RCL_ERROR)),
    ?assert(not should(ErrorLogState, ?RCL_INFO)),
    ?assert(not should(ErrorLogState, ?RCL_DEBUG)),
    ?assertEqual(?RCL_ERROR, log_level(ErrorLogState)),
    ?assertEqual(error, atom_log_level(ErrorLogState)),

    InfoLogState = new(info),
    ?assert(should(InfoLogState, ?RCL_ERROR)),
    ?assert(should(InfoLogState, ?RCL_INFO)),
    ?assert(not should(InfoLogState, ?RCL_DEBUG)),
    ?assertEqual(?RCL_INFO, log_level(InfoLogState)),
    ?assertEqual(info, atom_log_level(InfoLogState)),

    DebugLogState = new(debug),
    ?assert(should(DebugLogState, ?RCL_ERROR)),
    ?assert(should(DebugLogState, ?RCL_INFO)),
    ?assert(should(DebugLogState, ?RCL_DEBUG)),
    ?assertEqual(?RCL_DEBUG, log_level(DebugLogState)),
    ?assertEqual(debug, atom_log_level(DebugLogState)).

-endif.
