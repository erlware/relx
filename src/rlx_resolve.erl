-module(rlx_resolve).

-export([release/5,
         metadata/1]).

-include("relx.hrl").

-type name() :: atom().
-type vsn() :: string().

-type type() :: permanent | transient | temporary | load | none.
-type incl_apps() :: [name()].

-type app_config() :: name() |
                      {name(), vsn() | type() | incl_apps()} |
                      {name(), type(), incl_apps()} |
                      {name(), vsn(), type() | incl_apps()} |
                      {name(), vsn(), type(), incl_apps()}.

-type application_spec() :: {name(), vsn()} |
                            {name(), vsn(), type() | incl_apps()} |
                            {name(), vsn(), type(), incl_apps()}.

-record(release, {name             :: name(),
                  vsn              :: ec_semver:any_version(),
                  erts             :: undefined | ec_semver:any_version(),
                  realized = false :: boolean(),
                  app_spec         :: [application_spec()] | undefined,
                  apps_config      :: [app_config()],
                  relfile          :: undefined | string(),
                  app_detail = []  :: [rlx_app:t()],

                  %% per-release config values
                  config = []      :: [term()]}).
-type release() :: #release{}.

-export_type([release/0]).

-spec release(name(), vsn(), [app_config()], [rlx_app:t()], term()) -> rlx_release:t().
release(Name, Vsn, AppConfig, World, _Opts) ->
    Release = #release{name=Name,
                       vsn=Vsn,
                       apps_config=AppConfig},
    process_specs(realize_erts(Release), World).

-spec metadata(release()) -> {ok, term()} | {error, term()}.
metadata(#release{name=Name, vsn=Vsn, erts=ErtsVsn, app_spec=AppSpec, realized=Realized}) ->
    case Realized of
        true ->
            {ok, {release, {erlang:atom_to_list(Name), Vsn}, {erts, ErtsVsn}, AppSpec}};
        false ->
            ?RLX_ERROR({not_realized, Name, Vsn})
    end.


-spec process_specs(release(), [rlx_app:t()]) -> {ok, release()}.
process_specs(Rel=#release{apps_config=AppConfig}, World) ->
    IncludedApps = lists:foldl(fun(#{included_applications := I}, Acc) ->
                                       sets:union(sets:from_list(I), Acc)
                               end, sets:new(), World),
    Specs = [create_app_spec(AppConfig, App, IncludedApps) || App <- World],
    {ok, Rel#release{app_spec=Specs,
                     app_detail=World,
                     realized=true}}.

-spec create_app_spec(app_config(), rlx_app:t(), sets:set(atom())) -> application_spec().
create_app_spec(AppConfig, _App=#{name := BinName,
                                  vsn := Vsn}, WorldIncludedApplications) ->
    %% use type=load for included applications
    %% TODO: shouldn't this error if it is found in Applications and IncludedApplications?
    AtomName = rlx_util:to_atom(BinName),
    Type0 = case sets:is_element(AtomName, WorldIncludedApplications) of
                true ->
                    load;
                false ->
                    permanent
            end,

    case find(AtomName, AppConfig) of
        Name when is_atom(Name) ->
            maybe_with_type({rlx_util:to_atom(BinName), Vsn}, Type0);
        {Name, Vsn1} when Vsn1 =:= Vsn ->
            maybe_with_type({Name, Vsn1}, Type0);
        Spec={_, _Vsn1, Type, IncludedApplications} when is_list(IncludedApplications) ,
                                                      Type =:= permanent ;
                                                      Type =:= transient ;
                                                      Type =:= temporary ;
                                                      Type =:= load ;
                                                      Type =:= none ->
            %% TODO: error if Vsn and Vsn1 don't match
            Spec;
        {Name, Type} when Type =:= permanent ;
                          Type =:= transient ;
                          Type =:= temporary ;
                          Type =:= load ;
                          Type =:= none ->
            maybe_with_type({Name, Vsn}, Type);
        {Name, Vsn1, Type} when Type =:= permanent ;
                                Type =:= transient ;
                                Type =:= temporary ;
                                Type =:= load ;
                                Type =:= none ->
            %% TODO: error if Vsn and Vsn1 don't match
            maybe_with_type({Name, Vsn1}, Type);
        {Name, IncludedApplications} when is_list(IncludedApplications) ->
            %% TODO: error if Vsn and Vsn1 don't match
            maybe_with_type({Name, Vsn, IncludedApplications}, Type0);
        _ ->
            %% TODO: throw a real error
            error
    end.

%% keep a clean .rel file by only included non-defaults
maybe_with_type(Tuple, permanent) ->
    Tuple;
maybe_with_type(Tuple, Type) ->
    erlang:insert_element(3, Tuple, Type).

-spec realize_erts(release()) -> release().
realize_erts(Rel=#release{erts=undefined}) ->
    Rel#release{erts=erlang:system_info(version)};
realize_erts(Rel) ->
    Rel.

%%

find(Key, []) ->
    Key;
find(Key, [Key | _]) ->
    Key;
find(Key, [Tuple | _]) when is_tuple(Tuple) ,
                            element(1, Tuple) =:= Key->
    Tuple;
find(Key, [_ | Rest]) ->
    find(Key, Rest).
