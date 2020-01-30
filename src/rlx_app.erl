-module(rlx_app).

-export([new/0,
         new/6]).

%% TODO: remove the undefines
-type t() :: #{name                 := atom() | undefined,
               vsn                  := string() | undefined,

               applications         := [atom()],
               included_application := [atom()],

               dir                  := file:name() | undefined,
               link                 := boolean() | undefined}.

-export_type([t/0]).

%% needed temporarily for backwards compat
new() ->
    #{name => undefined,
      vsn => undefined,

      applications => [],
      included_applications => [],

      dir => undefined,
      link => undefined}.

new(Name, Vsn, Dir, Applications, IncludedApplications, IsLink) ->
    #{name => Name,
      vsn => Vsn,

      applications => Applications,
      included_applications => IncludedApplications,

      dir => Dir,
      link => IsLink}.
