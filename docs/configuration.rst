Configuration
=============

Configuration of reltool is done via Erlang key, value elements in the
`relcool.config` file. There are various configuration elements that
can appear, each affects the running of the system in specific
ways. Note that these must be complete Erlang terms terminated by a
`.`.

Paths
-----

Paths is a way to add additional code paths (that point to Erlang beam
files) to the system. These are not used for dependency resolution,
they are only used as code paths. The main use for this is simply to
provide a non-resolvable path for additional providers.

the path confuration element looks as follows::

    {paths, [<list of directory paths>]}.

So if we wanted to add `/usr/local/lib` and `/opt/lib` to the code
paths for the system we could add the following::

    {paths, ["/usr/local/lib", "/opt/lib"]}.

to our configuration.

Providers
---------

Providers are how the Relcool system can be extended by a user. They
are basically Erlang modules that implement the `rcl_provider`
behaviour and are available via a code path. See the section on
providers for more information.

The `providers` element provides a completely new list of providers,
replacing any providers that may already exist. The provider element
is as follows::

     {providers, <list of module names>}.

Lets say I have three providers; `my_custom_assembler`,
`my_rpm_assembler` and `my_deb_assembler`. I could make these the
complete list of providers by doing the following in my config::

    {providers, [my_custom_assembler,
                 my_rpm_assembler,
                 my_deb_assembler]}.

Order is important in the providers as they will be executed in the
order specified.

Add Providers
-------------

`add_providers` is very similar to `providers` with the exception that
the listed providers are added to the end of the current list of
providers rather then replacing the list all together. Add providres
looks as follows::


     {add_providers, <list of module names>}.

Lets take our previous example but only add `my_rpm_assembler` and
`my_deb_assembler` to the existing list of providers. We do this by
adding the following::

    {add_providers, [my_rpm_assembler,
                     my_deb_assembler]}.

Releases
--------

Release configuration is the bread and butter, the blood and bones of
the Relcool system. It provides the framework for realizing
dependencies in the system. The release element basically minics the
standard _Erlang/OTP Release Metadata file format:
http://www.erlang.org/doc/man/rel.html. There are two main
differences. The first is that you don't have to specify a complete
list of applications and you may specify version constraints instead
of hard versions.


{release, {relname, vsn},
  app goals}
{release, {relname, vsn},
{erts, vsn},
app goals}
