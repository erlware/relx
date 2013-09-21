---
layout: default
title: Relx Overlays
---

Overlays
========

Overlays as defined in Rebar are directly supported in Relx. So you
can take your Overlay configuration and move it to your
`relx.config` and it should just work. For example, you could take
the following:

{% highlight erlang %}
    {overlay_vars, "vars.config"}.
    {overlay, [{mkdir, "log/sasl"},
                {copy, "files/erl", "{{erts_vsn}}/bin/erl"},
                {copy, "files/nodetool", "{{erts_vsn}}/bin/nodetool"},
                {template, "files/app.config", "etc/app.config"},
                {template, "files/vm.args", "etc/vm.args"}]}.
{% endhighlight %}

and move it directly from your `reltool.config` to your
`relx.config` and it would work in exactly the same way. In Relx
you have more options though. Relx exposes more variables along
with the full power of a Django templating system (see
[erlydtl](http://code.google.com/p/erlydtl/)). You can look at the
documentation there for the full syntax supported. The variables
supported are listed below.

Available Variables
-------------------

* log : The current log level in the format of `(<logname>:<loglevel>)`
* output_dir : The current output directory for the built release
* target_dir : The same as `output_dir`, exists for backwards compatibility
* overridden : The current list of overridden apps (a list of app names)
* overrides.AppName : The override directory for said AppName
* goals : The list of user specified goals in the system
* lib_dirs : The list of library directories, both user specified and derived
* config_file : The list of config file used in the system
* providers : The list of provider names used for this run of relx
* sys_config : The location of the sys config file
* root_dir : The root dir of the current project
* default_release_name : The current default release name for the relx run
* default_release_version : The current default release version for the relx run
* default_release : The current default release for the relx run
* release_erts_version :  The version of the Erlang Runtime System in use
* erts_vsn : The same as `release_erts_vsn` (for backwards compatibility)
* release_name : The currently executing release
* release_version : the currently executing version
* rel_vsn : Same as `release_version`. Exists for backwards compatibility
* release_applications : A list of applications included in the release
* release.AppName.version : The version of `AppName`
* release.AppName.dir : The on system directory of `AppName`
* release.AppName.active_dependencies : The list of active application dependencies for `AppName`
* release.AppName.library_dependencies : The list of library application dependencies for `AppName`
* release.AppName.link : Indicates if this application should be linked in or not
