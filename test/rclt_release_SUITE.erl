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
%%%-------------------------------------------------------------------
%%% @author Eric Merrit <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Eric Merrit
-module(rclt_release_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         make_release/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    LibDir1 = filename:join([DataDir, create_random_name("lib_dir1_")]),
    ok = rcl_util:mkdir_p(LibDir1),
    State = rcl_state:new([{lib_dirs, [LibDir1]}], []),
    [{lib1, LibDir1},
     {state, State} | Config].

all() ->
    [make_release].

make_release(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    [(fun({Name, Vsn}) ->
              create_app(LibDir1, Name, Vsn, [kernel, stdlib], [])
      end)(App)
     ||
        App <-
            [{create_random_name("lib_app1_"), create_random_vsn()}
             || _ <- lists:seq(1, 100)]],

    create_app(LibDir1, "goal_app_1", "0.0.1", [stdlib,kernel,non_goal_1], []),
    create_app(LibDir1, "lib_dep_1", "0.0.1", [stdlib,kernel], []),
    create_app(LibDir1, "goal_app_2", "0.0.1", [stdlib,kernel,goal_app_1,non_goal_2], []),
    create_app(LibDir1, "non_goal_1", "0.0.1", [stdlib,kernel], [lib_dep_1]),
    create_app(LibDir1, "non_goal_2", "0.0.1", [stdlib,kernel], []),

    ConfigFile = filename:join([LibDir1, "relcool.config"]),
    write_config(ConfigFile,
                 [{release, {foo, "0.0.1"},
                   [goal_app_1,
                    goal_app_2]}]),
    OutputDir = filename:join([proplists:get_value(data_dir, Config),
                               create_random_name("relcool-output")]),
    {ok, State} = relcool:do(undefined, undefined, [], [LibDir1], 2,
                              OutputDir, [ConfigFile]),
    [{{foo, "0.0.1"}, Release}] = ec_dictionary:to_list(rcl_state:releases(State)),
    AppSpecs = rcl_release:applications(Release),
    ?assert(lists:keymember(stdlib, 1, AppSpecs)),
    ?assert(lists:keymember(kernel, 1, AppSpecs)),
    ?assert(lists:member({non_goal_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({non_goal_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_1, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({goal_app_2, "0.0.1"}, AppSpecs)),
    ?assert(lists:member({lib_dep_1, "0.0.1", load}, AppSpecs)).

%%%===================================================================
%%% Helper Functions
%%%===================================================================
create_app(Dir, Name, Vsn, Deps, LibDeps) ->
    AppDir = filename:join([Dir, Name]),
    write_app_file(AppDir, Name, Vsn, Deps, LibDeps),
    write_beam_file(AppDir, Name),
    rcl_app_info:new(erlang:list_to_atom(Name), Vsn, AppDir,
                     Deps, []).

write_beam_file(Dir, Name) ->
    Beam = filename:join([Dir, "ebin", "not_a_real_beam" ++ Name ++ ".beam"]),
    ok = filelib:ensure_dir(Beam),
    ok = ec_file:write_term(Beam, testing_purposes_only).

write_app_file(Dir, Name, Version, Deps, LibDeps) ->
    Filename = filename:join([Dir, "ebin", Name ++ ".app"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(Name, Version, Deps, LibDeps)).

get_app_metadata(Name, Vsn, Deps, LibDeps) ->
    {application, erlang:list_to_atom(Name),
     [{description, ""},
      {vsn, Vsn},
      {modules, []},
      {included_applications, LibDeps},
      {applications, Deps}]}.

create_random_name(Name) ->
    random:seed(erlang:now()),
    Name ++ erlang:integer_to_list(random:uniform(1000000)).

create_random_vsn() ->
    random:seed(erlang:now()),
    lists:flatten([erlang:integer_to_list(random:uniform(100)),
                   ".", erlang:integer_to_list(random:uniform(100)),
                   ".", erlang:integer_to_list(random:uniform(100))]).

write_config(Filename, Values) ->
    ok = ec_file:write(Filename,
                       [io_lib:format("~p.\n", [Val]) || Val <- Values]).
