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
%%% @author Eric Merrit <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Eric Merrit
-module(rlx_discover_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         normal_case/1,
         no_beam_case/1,
         bad_ebin_case/1]).

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
    LibDir2 = filename:join([DataDir, create_random_name("lib_dir2_")]),
    ok = rlx_util:mkdir_p(LibDir1),
    ok = rlx_util:mkdir_p(LibDir2),
    State = rlx_state:new([{lib_dirs, [LibDir1, LibDir2]}], [release]),
    [{lib1, LibDir1},
     {lib2, LibDir2},
     {state, State} | Config].


all() ->
    [normal_case, no_beam_case, bad_ebin_case].

normal_case(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    Apps1 = [(fun({Name, Vsn}) ->
                      create_app(LibDir1, Name, Vsn)
              end)(App)
             ||
                App <-
                    [{create_random_name("lib_app1_"), create_random_vsn()}
                     || _ <- lists:seq(1, 100)]],

    LibDir2 = proplists:get_value(lib2, Config),
    Apps2 = [(fun({Name, Vsn}) ->
                      create_app(LibDir2, Name, Vsn)
              end)(App)
             || App <-
                    [{create_random_name("lib_app2_"), create_random_vsn()}
                     || _ <- lists:seq(1, 100)]],
    State0 = rlx_state:put(proplists:get_value(state, Config),
                            default_libs, false),
    {DiscoverProvider, {ok, State1}} = rlx_provider:new(rlx_prv_discover, State0),
    {ok, State2} = rlx_provider:do(DiscoverProvider, State1),
    lists:foreach(fun(App) ->
                          ?assertMatch(true, lists:member(App, rlx_state:available_apps(State2)))
                  end, Apps1),

    lists:foreach(fun(App) ->
                          ?assertMatch(true, lists:member(App, rlx_state:available_apps(State2)))
                  end, Apps2),
    Length = erlang:length(Apps2) +
        erlang:length(Apps2),
    ?assertMatch(Length, erlang:length(rlx_state:available_apps(State2))).

no_beam_case(Config) ->
    %% do not ignore apps with no beam files if no modules in app file
    LibDir1 = proplists:get_value(lib1, Config),
    _Apps1 = [(fun({Name, Vsn}) ->
                      create_app(LibDir1, Name, Vsn)
              end)(App)
             ||
                App <-
                    [{create_random_name("lib_app1_"), create_random_vsn()}
                     || _ <- lists:seq(1, 100)]],

    LibDir2 = proplists:get_value(lib2, Config),
    _Apps2 = [(fun({Name, Vsn}) ->
                      create_app(LibDir2, Name, Vsn)
              end)(App)
             || App <-
                    [{create_random_name("lib_app2_"), create_random_vsn()}
                     || _ <- lists:seq(1, 100)]],
    BadName = create_random_name("error_bad"),
    BadVsn = create_random_vsn(),
    AppDir = filename:join([LibDir2, BadName]),
    write_app_file(AppDir, BadName, BadVsn),
    State0 = proplists:get_value(state, Config),
    %% Deliberately disable release discovery when running `rlx_prv_discover`
    State1 = rlx_state:put(State0, disable_rel_discovery, true),
    {DiscoverProvider, {ok, State2}} = rlx_provider:new(rlx_prv_discover, State1),

    ?assertMatch({ok, _},
                 rlx_provider:do(DiscoverProvider, State2)).

bad_ebin_case(Config) ->
    LibDir1 = proplists:get_value(lib1, Config),
    _Apps1 = [(fun({Name, Vsn}) ->
                      create_app(LibDir1, Name, Vsn)
              end)(App)
             ||
                 App <-
                     [{create_random_name("lib_app1_"), create_random_vsn()}
                      || _ <- lists:seq(1, 100)]],

    LibDir2 = proplists:get_value(lib2, Config),
    _Apps2 = [(fun({Name, Vsn}) ->
                      create_app(LibDir2, Name, Vsn)
              end)(App)
             || App <-
                    [{create_random_name("lib_app2_"), create_random_vsn()}
                     || _ <- lists:seq(1, 100)]],
    BadName = create_random_name("error_bad"),
    BadVsn = create_random_vsn(),
    AppDir = filename:join([LibDir2, BadName]),
    Filename = filename:join([AppDir,  <<"ebin">>, BadName ++ ".app"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_bad_app_metadata(BadName, BadVsn)),
    State0 = proplists:get_value(state, Config),
    {DiscoverProvider, {ok, State1}} = rlx_provider:new(rlx_prv_discover, State0),
    {ok, State2} = rlx_provider:do(DiscoverProvider, State1),
    ?assertMatch([], [App || App <- rlx_state:available_apps(State2),
                             BadName =:= rlx_app_info:name(App)]).

%%%===================================================================
%%% Helper functions
%%%===================================================================
create_app(Dir, Name, Vsn) ->
    AppDir = filename:join([Dir, Name]),
    write_app_file(AppDir, Name, Vsn),
    write_beam_file(AppDir, Name),
    {ok, App} = rlx_app_info:new(erlang:list_to_atom(Name), Vsn,
                                 erlang:iolist_to_binary(AppDir),
                                 [kernel, stdlib], []),
    App.

write_beam_file(Dir, Name) ->
    Beam = filename:join([Dir, "ebin", "not_a_real_beam" ++ Name ++ ".beam"]),
    ok = filelib:ensure_dir(Beam),
    ok = ec_file:write_term(Beam, testing_purposes_only).

write_app_file(Dir, Name, Version) ->
    Filename = filename:join([Dir, "ebin", Name ++ ".app"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(Name, Version)).

get_app_metadata(Name, Vsn) ->
    {application, erlang:list_to_atom(Name),
     [{description, ""},
      {vsn, Vsn},
      {modules, []},
      {applications, [kernel, stdlib]}]}.

get_bad_app_metadata(Name, Vsn) ->
    ["{application, ", Name, ",
     [{description, \"\"},
      {vsn, \"", Vsn, "\"},
      {modules, [missing],
      {applications, [kernel, stdlib]}]}."].


create_random_name(Name) ->
    random:seed(erlang:now()),
    Name ++ erlang:integer_to_list(random:uniform(1000000)).

create_random_vsn() ->
    random:seed(erlang:now()),
    lists:flatten([erlang:integer_to_list(random:uniform(100)),
                   ".", erlang:integer_to_list(random:uniform(100)),
                   ".", erlang:integer_to_list(random:uniform(100))]).
