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
%%% @doc This provides simple output functions for relx. You should use this
%%% to talk to the users if you are wrting code for the system
-module(rlx_log).

-export([new/1,
         new/2,
         log/4,
         should/2,
         debug/2,
         debug/3,
         info/2,
         info/3,
         error/2,
         error/3,
         log_level/1,
         atom_log_level/1,
         format/1]).

-export_type([t/0,
              int_log_level/0,
              atom_log_level/0,
              log_level/0,
              log_fun/0]).

-include_lib("relx/include/relx.hrl").

-define(RED, 31).
-define(GREEN, 32).
-define(YELLOW, 33).
-define(BLUE, 34).
-define(MAGENTA, 35).
-define(CYAN, 36).

-define(PREFIX, "===> ").

-record(state_t, {mod=?MODULE :: rlx_log,
                  log_level=0 :: int_log_level(),
                  caller=api :: api | command_line}).

%%============================================================================
%% types
%%============================================================================

-type log_level() :: int_log_level() | atom_log_level().

-type int_log_level() :: 0..2.

%% Why no warn? because for our purposes there is no difference between error
%% and warn
-type atom_log_level() :: error | info | debug.

-type log_fun() :: fun(() -> iolist()).

-type color() :: 31..36.

-opaque t() :: record(state_t).

%%============================================================================
%% API
%%============================================================================
%% @doc Create a new 'log level' for the system
-spec new(log_level()) -> t().
new(LogLevel) ->
    new(LogLevel, api).

new(LogLevel, Caller) when LogLevel >= 0, LogLevel =< 2 ->
    #state_t{mod=?MODULE, log_level=LogLevel, caller=Caller};
new(AtomLogLevel, Caller)
  when AtomLogLevel =:= error;
       AtomLogLevel =:= info;
       AtomLogLevel =:= debug ->
    LogLevel = case AtomLogLevel of
                   error -> 0;
                   info -> 1;
                   debug -> 2
               end,
    new(LogLevel, Caller).


%% @doc log at the debug level given the current log state with a string or
%% function that returns a string
-spec debug(t(), string() | log_fun()) -> ok.
debug(LogState, Fun)
  when erlang:is_function(Fun) ->
    log(LogState, ?RLX_DEBUG, fun() -> colorize(LogState, ?CYAN, false, Fun()) end);
debug(LogState, String) ->
    debug(LogState, "~s~n", [String]).

%% @doc log at the debug level given the current log state with a format string
%% and argements @see io:format/2
-spec debug(t(), string(), [any()]) -> ok.
debug(LogState, FormatString, Args) ->
    log(LogState, ?RLX_DEBUG, colorize(LogState, ?CYAN, false, FormatString), Args).

%% @doc log at the info level given the current log state with a string or
%% function that returns a string
-spec info(t(), string() | log_fun()) -> ok.
info(LogState, Fun)
    when erlang:is_function(Fun) ->
    log(LogState, ?RLX_INFO, fun() -> colorize(LogState, ?GREEN, false, Fun()) end);
info(LogState, String) ->
    info(LogState, "~s~n", [String]).

%% @doc log at the info level given the current log state with a format string
%% and argements @see io:format/2
-spec info(t(), string(), [any()]) -> ok.
info(LogState, FormatString, Args) ->
    log(LogState, ?RLX_INFO, colorize(LogState, ?GREEN, false, FormatString), Args).

%% @doc log at the error level given the current log state with a string or
%% format string that returns a function
-spec error(t(), string() | log_fun()) -> ok.
error(LogState, Fun)
    when erlang:is_function(Fun) ->
    log(LogState, ?RLX_ERROR, fun() -> colorize(LogState, ?RED, false, Fun()) end);
error(LogState, String) ->
    error(LogState, "~s~n", [String]).

%% @doc log at the error level given the current log state with a format string
%% and argements @see io:format/2
-spec error(t(), string(), [any()]) -> ok.
error(LogState, FormatString, Args) ->
    log(LogState, ?RLX_ERROR, colorize(LogState, ?GREEN, false, FormatString), Args).

%% @doc Execute the fun passed in if log level is as expected.
-spec log(t(), int_log_level(), log_fun()) -> ok.
log(#state_t{mod=?MODULE, log_level=DetailLogLevel}, LogLevel, Fun)
    when DetailLogLevel >= LogLevel ->
    io:format("~s~n", [Fun()]);
log(_, _, _) ->
    ok.

%% @doc when the module log level is less then or equal to the log level for the
%% call then write the log info out. When its not then ignore the call.
-spec log(t(), int_log_level(), string(), [any()]) -> ok.
log(#state_t{mod=?MODULE, log_level=DetailLogLevel}, LogLevel, FormatString, Args)
  when DetailLogLevel >= LogLevel,
       erlang:is_list(Args) ->
    io:format(FormatString, Args);
log(_, _, _, _) ->
    ok.

%% @doc return a boolean indicating if the system should log for the specified
%% levelg
-spec should(t(), int_log_level() | any()) -> boolean().
should(#state_t{mod=?MODULE, log_level=DetailLogLevel}, LogLevel)
  when DetailLogLevel >= LogLevel ->
    true;
should(_, _) ->
    false.

%% @doc get the current log level as an integer
-spec log_level(t()) -> int_log_level().
log_level(#state_t{mod=?MODULE, log_level=DetailLogLevel}) ->
    DetailLogLevel.

%% @doc get the current log level as an atom
-spec atom_log_level(t()) -> atom_log_level().
atom_log_level(#state_t{mod=?MODULE, log_level=?RLX_ERROR}) ->
    error;
atom_log_level(#state_t{mod=?MODULE, log_level=?RLX_INFO}) ->
    info;
atom_log_level(#state_t{mod=?MODULE, log_level=?RLX_DEBUG}) ->
    debug.

-spec format(t()) -> iolist().
format(Log) ->
    [<<"(">>,
     erlang:integer_to_list(log_level(Log)), <<":">>,
     erlang:atom_to_list(atom_log_level(Log)),
     <<")">>].

-spec colorize(t(), color(), boolean(), string()) -> string().
colorize(#state_t{caller=command_line}, Color, false, Msg) when is_integer(Color) ->
    colorize_(Color, 0, Msg);
colorize(_LogState, _Color, _Bold, Msg) ->
    Msg.

-spec colorize_(color(), integer(), string()) -> string().
colorize_(Color, Bold, Msg) when is_integer(Color), is_integer(Bold)->
    lists:flatten(io_lib:format("\033[~B;~Bm~s~s\033[0m", [Bold, Color, ?PREFIX, Msg])).

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

should_test() ->
    ErrorLogState = new(error),
    ?assertMatch(true, should(ErrorLogState, ?RLX_ERROR)),
    ?assertMatch(true, not should(ErrorLogState, ?RLX_INFO)),
    ?assertMatch(true, not should(ErrorLogState, ?RLX_DEBUG)),
    ?assertEqual(?RLX_ERROR, log_level(ErrorLogState)),
    ?assertEqual(error, atom_log_level(ErrorLogState)),

    InfoLogState = new(info),
    ?assertMatch(true, should(InfoLogState, ?RLX_ERROR)),
    ?assertMatch(true, should(InfoLogState, ?RLX_INFO)),
    ?assertMatch(true, not should(InfoLogState, ?RLX_DEBUG)),
    ?assertEqual(?RLX_INFO, log_level(InfoLogState)),
    ?assertEqual(info, atom_log_level(InfoLogState)),

    DebugLogState = new(debug),
    ?assertMatch(true, should(DebugLogState, ?RLX_ERROR)),
    ?assertMatch(true, should(DebugLogState, ?RLX_INFO)),
    ?assertMatch(true, should(DebugLogState, ?RLX_DEBUG)),
    ?assertEqual(?RLX_DEBUG, log_level(DebugLogState)),
    ?assertEqual(debug, atom_log_level(DebugLogState)).

-endif.
