-module(rlx_rel_discover).

-export([do/1,
         format_error/1]).

-include("relx.hrl").

-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State0) ->
    LibDirs = get_lib_dirs(State0),
    AppMeta = rlx_state:available_apps(State0),
    case rlx_rel_discovery:do(State0, LibDirs, AppMeta) of
        {ok, Releases} ->
            {ok, rlx_state:realized_releases(State0, lists:foldl(fun add/2,
                                                                 #{},
                                                                 Releases))};
        Error ->
            Error
    end.

-spec format_error(any()) -> iolist().
format_error(_) ->
    "".

%%%===================================================================
%%% Internal Functions
%%%===================================================================
%% @doc only add the release if its not documented in the system
add(Rel, Map) ->
    RelName = rlx_release:name(Rel),
    RelVsn = rlx_release:vsn(Rel),
    Map#{{RelName, RelVsn} => Rel}.

get_lib_dirs(State) ->
    LibDirs0 = rlx_state:lib_dirs(State),
    case rlx_state:get(State, default_libs, true) of
        false ->
            LibDirs0;
        true ->
            lists:flatten([LibDirs0,
                           add_release_output_dir(State)])
    end.

add_release_output_dir(State) ->
    case rlx_state:get(State, disable_discover_release_output, false) of
        true ->
            [];
        false ->
            Output = erlang:iolist_to_binary(rlx_state:base_output_dir(State)),
            case ec_file:exists(erlang:binary_to_list(Output)) of
                true ->
                    Output;
                false ->
                    []
            end
    end.
