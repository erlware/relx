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
%%% @doc This provider uses the lib_dir setting of the state. It searches the
%%% Lib Dirs looking for all OTP Applications that are available. When it finds
%%% those OTP Applications it loads the information about them and adds them to
%%% the state of available apps. This implements the rlx_provider behaviour.
-module(rlx_dscv_util).

-export([do/2,
         format_error/1]).

-include_lib("relx.hrl").

%%============================================================================
%% Types
%%============================================================================

-type process_fun(Result) :: fun((file:name(), file | directory) ->
                                        {ok, Result} |
                                        {error, term()} |
                                        {ok, Result, Recurse::boolean()} |
                                        {noresult, Recurse::boolean()} |
                                        {error, term()}).

%%============================================================================
%% API
%%============================================================================

%% @doc recursively dig down into the library directories specified in the state
%% looking for OTP Applications
-spec do(process_fun([term()] | term()), [file:name()]) ->
                [term() | {error, term()}].
do(ProcessDir, LibDirs) ->
    lists:flatten(ec_plists:map(fun(LibDir) ->
                                        discover_dir(ProcessDir, LibDir,
                                                     ec_file:type(LibDir))
                                end, LibDirs)).

-spec format_error([ErrorDetail::term()]) -> iolist().
format_error(ErrorDetails)
  when erlang:is_list(ErrorDetails) ->
    [[format_detail(ErrorDetail), "\n"] || ErrorDetail <- ErrorDetails].

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec format_detail(ErrorDetail::term()) -> iolist().
format_detail({accessing, File, eaccess}) ->
    io_lib:format("permission denied accessing file ~s", [File]);
format_detail({accessing, File, Type}) ->
    io_lib:format("error (~p) accessing file ~s", [Type, File]);
format_detail({not_a_directory, EbinDir}) ->
    io_lib:format("~s is not a directory when it should be a directory", [EbinDir]).

-spec discover_dir(process_fun([term()] | term()),
                   file:name(),
                   directory | file | symlink) ->
                          [{ok, term()}
                           | {error, Reason::term()}]
                              | []
                              | {ok, term()}
                              | {error, Reason::term()}.
discover_dir(ProcessDir, File, directory) ->
    case ProcessDir(File, directory) of
        {ok, Result, true} ->
            [{ok, Result} | recurse(ProcessDir, File)];
        {noresult, true} ->
            recurse(ProcessDir, File);
        {ok, Result, _} ->
            [{ok, Result}];
        {noresult, _} ->
            [];
        Err = {error, _} ->
            [Err]
    end;
discover_dir(ProcessDir, File, file) ->
    case ProcessDir(File, file) of
        {ok, Result} ->
            [{ok, Result}];
        {noresult, _} ->
            [];
        Warn = {warning, _} ->
            [Warn];
        Err = {error, _} ->
            [Err]
    end;
discover_dir(ProcessDir, File, symlink) ->
    case filelib:is_dir(File) of
        false ->
            discover_dir(ProcessDir, File, file);
        true ->
            discover_real_symlink_dir(ProcessDir, File)
    end.

discover_real_symlink_dir(ProcessDir, File) ->
    {ok, CurCwd} = file:get_cwd(),
    ok = file:set_cwd(File),
    {ok, ActualRealDir} = file:get_cwd(),
    ok = file:set_cwd(CurCwd),
    lists:prefix(iolist_to_list(filename:absname(ActualRealDir)),
                 iolist_to_list(filename:absname(File))),
    case ProcessDir(File, directory) of
        {ok, Result, true} ->
            [{ok, Result} | recurse(ProcessDir, File)];
        {noresult, true} ->
            recurse(ProcessDir, File);
        {ok, Result, _} ->
            [{ok, Result}];
        {noresult, _} ->
            [];
        Err = {error, _} ->
            [Err]
    end.

recurse(ProcessDir, File) ->
    case file:list_dir(File) of
        {error, Reason} ->
            {error, {accessing, File, Reason}};
        {ok, List} ->
            ec_plists:map(fun(LibDir) ->
                                  discover_dir(ProcessDir, LibDir, ec_file:type(LibDir))
                          end, [filename:join([File, Dir]) || Dir <- List])
    end.

iolist_to_list(IoList) ->
    erlang:binary_to_list(erlang:iolist_to_binary(IoList)).
