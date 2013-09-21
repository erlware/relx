---
layout: default
title: Relx Configuration
---

Configuration
=============

Configuration of reltool is done via Erlang key-value elements in the
`relx.config` file(s). There are various configuration elements
that can appear, each affects the running of the release building
process in specific ways. Note that these must be complete Erlang
terms terminated by a `.`.

App Dirs
--------

App directories specify where `relx` will search for OTP applications to resolve dependencies.

The app directory configuration element looks as follows:

{% highlight erl %}
{lib_dirs, [<path1>, <path2>, ...]}.
{% endhighlight %}

Paths
-----

Paths is a way to add additional code paths (that point to Erlang beam
files) to the system. These are not used for dependency resolution,
they are only used as code paths. The main use for this is simply to
provide a non-resolvable path for additional providers.

The path configuration element looks as follows:

{% highlight erl %}
    {paths, [<list of directory paths>]}.
{% endhighlight %}

So if we wanted to add `/usr/local/lib` and `/opt/lib` to the code
paths for the system we could add the following::

{% highlight erl %}
    {paths, ["/usr/local/lib", "/opt/lib"]}.
{% endhighlight %}

to our configuration.

Releases
--------

Release configuration is the bread and butter, the blood and bones of
the Relx system. It provides the framework for realizing
dependencies in the system. The release element basically minics the
standard Erlang/OTP Release Metadata file format:
http://www.erlang.org/doc/man/rel.html. There are two main
differences. The first is that you don't have to specify a complete
list of applications and you may specify version constraints instead
of hard versions.

{% highlight erl %}
    {release, {relname, vsn},
      <app goals>}
    {release, {rkelname, vsn},
     {erts, vsn},
      <app goals>}
{% endhighlight %}

See the Overview for goal syntax.

Start Script
------------

Relx generates a start script by default that should work for your
release. However, there may be times when you want to provide a custom
start script. In those situations you may disable automatic start
script creation by adding the following to your `relx.config`.

{% highlight erl %}
    {generate_start_script, false}.
{% endhighlight %}

Overlays
--------

These are documented on the Overlays wiki page. Go there for more
information.

Advanced Options
----------------

The following are options that you would probably not need to use very
often. They exist for doing unusual things with the system or adding
plugin style functionality.


### Providers

Providers are how the Relx system can be extended by a user. They
are basically Erlang modules that implement the `rcl_provider`
behaviour and are available via a code path. See the section on
providers for more information.

The `providers` element provides a completely new list of providers,
replacing any providers that may already exist. The provider element
is as follows::

{% highlight erl %}
     {providers, <list of module names>}.
{% endhighlight %}

Lets say I have three providers; `my_custom_assembler`,
`my_rpm_assembler` and `my_deb_assembler`. I could make these the
complete list of providers by doing the following in my config::

{% highlight erl %}
    {providers, [my_custom_assembler,
                 my_rpm_assembler,
                 my_deb_assembler]}.
{% endhighlight %}

Order is important in the providers as they will be executed in the
order specified.

### Add Providers

`add_providers` is very similar to `providers` with the exception that
the listed providers are added to the end of the current list of
providers rather then replacing the list all together. Add providres
looks as follows::

{% highlight erl %}
     {add_providers, <list of module names>}.
{% endhighlight %}

Lets take our previous example but only add `my_rpm_assembler` and
`my_deb_assembler` to the existing list of providers. We do this by
adding the following::

{% highlight erl %}
    {add_providers, [my_rpm_assembler,
                     my_deb_assembler]}.
{% endhighlight %}

Example Configuration
---------------------

{% highlight erl %}
%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%% Example Relx Config
%% ======================
%%
%% This is an example relx config whose purpose is to demonstrate all of the
%% options available in relx. Its not expected that you will use all of the
%% things here. In fact, there is a high likely hood that *your* relx.config
%% will be extremely minimal, as relx does a very good job of figuring out
%% things on its own.
%%
%% The Release We Are Building
%% ---------------------------
%%
%% Lets say we have a release called sexpr. The sexpr release supports versions
%% 0.0.1 and 0.0.2 with different dependencies. 0.0.1 requires erlware commons
%% 0.8.0 or lesser. 0.0.2 requires erlware_commons 0.8.1 or greater along with
%% neotoma (any version). We also do not want neotoma to be loaded. We also want
%% our default release. the one we build in the common case to be sexper 0.0.2.

%% You can tell relx about additional directories that you want searched for
%% otp apps during the discovery process. You do that in the 'lib_dirs' config.
%% You can specify these on the command line with `-l`.
{lib_dirs, ["../erlang_app"]}.

%% You can also specify these paths on the command line with `-p`. Be aware that
%% relx plays well with rebar so if you have a deps directory in the current
%% directory it will be automatically added.
{paths, ["/opt/erlang_apps"]}.


%% If you have a sys.config file you need to tell relx where it is. If you do
%% that relx will include the sys.config in the appropriate place
%% automatically.
{sys_config, "./config/sys.config"}.

%% relx will include erts by default. However, if you don't want to include
%% erts you can add the `include_erts` tuple to the config and tell relx not
%% to include it.
{include_erts, false}.

%% When we have multiple releases relx needs to know which one to build. You
%% can specify that on the command line with the `-n` and `-v` arguments to
%% relx. However, it is often more convenient to do it in the config.
{default_release, sexpr, "0.0.2"}.

{release, {sexpr, "0.0.1"},
 [sexpr,
  %% There are two syntaxes for constraints.
  %% The first is the tuple syntax shown here.
  {erlware_commons, "0.8.0", '<='}]}.

{release, {sexpr, "0.0.2"},
 [sexpr,

  %% This is the second constraint syntax, it is interchangeable with the tuple
  %% syntax and its up to you which you find more readable/usable.
  "erlware_commons>=0.8.1",

  %% You can put the release load types in the release spec here in exactly the
  %% same way that you can do it for a normal relfile. The syntax is
  %% {<constraint>, <type>}.
  {neotoma, load}]}.

%% During development its often the case that you want to substitute the app
%% that you are working on for a 'production' version of an app. You can
%% explicitly tell relx to override all versions of an app that you specify
%% with an app in an arbitrary directory. Relx will then symlink that app
%% into the release in place of the specified app. be aware though that relx
%% will check your app for consistancy so it should be a normal OTP app and
%% already be built.
{overrides, [{sexpr, "../sexpr"}]}.


%% In some cases you might want to add additional functionality to relx. You
%% can do this via a 'provider'. A provider is an implementation of the relx
%% provider behaviour. This probably shouldn't be needed very often.
{add_providers, [my_custom_functionality]}.
{% endhighlight %}
