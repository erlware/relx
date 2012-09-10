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
%%% @doc
-module(relcool).

-export([main/1, opt_spec_list/0]).

%%============================================================================
%% types
%%============================================================================

%%============================================================================
%% API
%%============================================================================
-spec main([string()]) -> ok.
main(Args) ->
    OptSpecList = opt_spec_list(),
    case rcl_cmd_args:args2state(getopt:parse(OptSpecList, Args)) of
        {ok, {State, _Target}} ->
            run_relcool_process(State);
        Error={error, _} ->
            report_error(Error)
    end.

-spec opt_spec_list() -> [getopt:option_spec()].
opt_spec_list() ->
    [
     {relname,  $n, "relname",  string,  "Specify the name for the release that will be generated"},
     {relvsn, $v, "relvsn", string, "Specify the version for the release"},
     {goals, $g, "goal", string, "Specify a target constraint on the system. These are "
      "usually the OTP"},
     {output_dir, $o, "output-dir", string, "The output directory for the release. This is `./` by default."},
     {lib_dir, $l, "lib-dir", string, "Additional dirs that should be searched for OTP Apps"},
     {log_level, $V, "verbose", {integer, 2}, "Verbosity level, maybe between 0 and 2"}
    ].

%%============================================================================
%% internal api
%%============================================================================
run_relcool_process(State) ->
    rcl_log:info(rcl_state:log(State), "Starting relcool build process ..."),
    rcl_log:debug(rcl_state:log(State),
                  fun() ->
                          rcl_state:format(State)
                  end),
    ok.

-spec usage() -> ok.
usage() ->
    getopt:usage(opt_spec_list(), "relcool", "[*release-specification-file*]").


-spec report_error(term()) -> none().
report_error(Error) ->
    io:format("~s~n", [to_error(Error)]),
    usage(),
    erlang:halt(127).

-spec to_error({error, Reason::term()}) -> string().
to_error({error,{invalid_option_arg, Arg}}) ->
    case Arg of
        {goals, Goal} ->
            io_lib:format("Invalid Goal argument -g ~p~n", [Goal]);
        {relname, RelName} ->
            io_lib:format("Invalid Release Name argument -n ~p~n", [RelName]);
        {relvsn, RelVsn} ->
            io_lib:format("Invalid Release Version argument -n ~p~n", [RelVsn]);
        {output_dir, Outdir} ->
            io_lib:format("Invalid Output Directory argument -n ~p~n", [Outdir]);
        {lib_dir, LibDir} ->
            io_lib:format("Invalid Library Directory argument -n ~p~n", [LibDir]);
        {log_level, LogLevel} ->
            io_lib:format("Invalid Library Directory argument -n ~p~n", [LogLevel])
    end;
to_error({error, {invalid_config_file, Config}}) ->
    io_lib:format("Invalid configuration file specified: ~s", [Config]);
to_error({error, {failed_to_parse, Spec}}) ->
    io_lib:format("Unable to parse spec ~s", [Spec]);
to_error({error, {unable_to_create_output_dir, OutputDir}}) ->
    io_lib:format("Unable to create output directory (possible permissions issue): ~s",
                  [OutputDir]);
to_error({error, {not_directory, Dir}}) ->
    io_lib:format("Library directory does not exist: ~s", [Dir]);
to_error({error, {invalid_log_level, LogLevel}}) ->
    io_lib:format("Invalid log level specified -V ~p, log level must be in the"
                  " range 0..2", [LogLevel]);
to_error({error, Error}) ->
    io_lib:format("~p~n", [Error]).
