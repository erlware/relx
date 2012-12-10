%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  A module that provides config parsing and support to the system
%%% @end
%%%-------------------------------------------------------------------
-module(rcl_prv_config).

-behaviour(rcl_provider).

%% API
-export([init/1,
         do/1,
         format_error/1]).

-include_lib("relcool/include/relcool.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Required by the system, but not used in this provider
-spec init(rcl_state:t()) -> {ok, rcl_state:t()} | relcool:error().
init(State) ->
    {ok, State}.

%% @doc parse all the configs currently specified in the state,
%% populating the state as a result.
-spec do(rcl_state:t()) ->{ok,  rcl_state:t()} | relcool:error().
do(State) ->
    case  rcl_state:config_files(State) of
        [] ->
            search_for_dominating_config(State);
        ConfigFiles ->
            lists:foldl(fun load_config/2, {ok, State}, ConfigFiles)
    end.

-spec format_error(Reason::term()) -> iolist().
format_error({consult, ConfigFile, Reason}) ->
        io_lib:format("Unable to read file ~s: ~s", [ConfigFile,
                                                     file:format_error(Reason)]);
format_error({invalid_term, Term}) ->
    io_lib:format("Invalid term in config file: ~p", [Term]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
search_for_dominating_config({ok, Cwd}) ->
    ConfigFile = filename:join(Cwd, "relcool.config"),
    case ec_file:exists(ConfigFile) of
        true ->
            {ok, ConfigFile};
        false ->
            search_for_dominating_config(parent_dir(Cwd))
    end;
search_for_dominating_config({error, _}) ->
    no_config;
search_for_dominating_config(State0) ->
    {ok, Cwd} = file:get_cwd(),
    case search_for_dominating_config({ok, Cwd}) of
        {ok, Config} ->
            %% we need to set the root dir on state as well
            {ok, RootDir} = parent_dir(Config),
            State1 = rcl_state:root_dir(State0, RootDir),
            load_config(Config, {ok, rcl_state:config_files(State1, [Config])});
        no_config ->
            {ok, State0}
    end.

%% @doc Given a directory returns the name of the parent directory.
-spec parent_dir(Filename::string()) ->
                        {ok, DirName::string()} | {error, no_parent_dir}.
parent_dir(Filename) ->
    parent_dir(filename:split(Filename), []).

%% @doc Given list of directories, splits the list and returns all dirs but the
%%  last as a path.
-spec parent_dir([string()], [string()]) ->
                        {ok, DirName::string()} | {error, no_parent_dir}.
parent_dir([_H], []) ->
    {error, no_parent_dir};
parent_dir([], []) ->
    {error, no_parent_dir};
parent_dir([_H], Acc) ->
    {ok, filename:join(lists:reverse(Acc))};
parent_dir([H | T], Acc) ->
    parent_dir(T, [H | Acc]).


-spec load_config(file:filename(), {ok, rcl_state:t()} | relcool:error()) ->
                         {ok, rcl_state:t()} | relcool:error().
load_config(_, Err = {error, _}) ->
    Err;
load_config(ConfigFile, {ok, State}) ->
    {ok, CurrentCwd} = file:get_cwd(),
    ok = file:set_cwd(filename:dirname(ConfigFile)),
    Result = case file:consult(ConfigFile) of
                 {error, Reason} ->
                     ?RCL_ERROR({consult, ConfigFile, Reason});
                 {ok, Terms} ->
                     lists:foldl(fun load_terms/2, {ok, State}, Terms)
             end,
    ok = file:set_cwd(CurrentCwd),
    Result.

-spec load_terms(term(), {ok, rcl_state:t()} | relcool:error()) ->
                        {ok, rcl_state:t()} | relcool:error().
load_terms({default_release, RelName, RelVsn}, {ok, State}) ->
    {ok, rcl_state:default_release(State, RelName, RelVsn)};
load_terms({paths, Paths}, {ok, State}) ->
    code:add_pathsa([filename:absname(Path) || Path <- Paths]),
    {ok, State};
load_terms({providers, Providers0}, {ok, State0}) ->
    Providers1 = gen_providers(Providers0, State0),
    case Providers1 of
        {_, E={error, _}} ->
            E;
        {Providers3, {ok, State3}} ->
            {ok, rcl_state:providers(State3, Providers3)}
    end;
load_terms({add_providers, Providers0}, {ok, State0}) ->
    Providers1 = gen_providers(Providers0, State0),
    case Providers1 of
        {_, E={error, _}} ->
            E;
        {Providers3, {ok, State1}} ->
            ExistingProviders = rcl_state:providers(State1),
            {ok, rcl_state:providers(State1, ExistingProviders ++ Providers3)}
    end;
load_terms({overrides, Overrides0}, {ok, State0}) ->
    {ok, rcl_state:overrides(State0, Overrides0)};
load_terms({release, {RelName, Vsn}, Applications}, {ok, State0}) ->
    Release0 = rcl_release:new(RelName, Vsn),
    case rcl_release:goals(Release0, Applications) of
        E={error, _} ->
            E;
        {ok, Release1} ->
            {ok, rcl_state:add_release(State0, Release1)}
        end;
load_terms({release, {RelName, Vsn}, {erts, ErtsVsn},
            Applications}, {ok, State}) ->
    Release0 = rcl_release:erts(rcl_release:new(RelName, Vsn), ErtsVsn),
    case rcl_release:goals(Release0, Applications) of
        E={error, _} ->
            E;
        {ok, Release1} ->
            {ok, rcl_state:add_release(State, Release1)}
    end;
load_terms({sys_config, SysConfig}, {ok, State}) ->
    {ok, rcl_state:sys_config(State, filename:absname(SysConfig))};
load_terms({Name, Value}, {ok, State})
  when erlang:is_atom(Name) ->
    {ok, rcl_state:put(State, Name, Value)};
load_terms(_, Error={error, _}) ->
    Error;
load_terms(InvalidTerm, _) ->
    ?RCL_ERROR({invalid_term, InvalidTerm}).

-spec gen_providers([module()], rcl_state:t()) ->
                           {[rcl_provider:t()], {ok, rcl_state:t()} | relcool:error()}.
gen_providers(Providers, State) ->
    lists:foldl(fun(ProviderName, {Providers1, {ok, State1}}) ->
                        {Provider, State2} = rcl_provider:new(ProviderName, State1),
                        {[Provider | Providers1], State2};
                   (_, E={_, {error, _}}) ->
                        E
                end, {[], {ok, State}}, Providers).
