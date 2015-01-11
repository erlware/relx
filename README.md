[![Build Status](https://travis-ci.org/erlware/relx.png?branch=master)](https://travis-ci.org/erlware/relx)

Relx
=======

A release assembler for Erlang.

Synopsis
--------

relx [*options*] [*release-specification-file*]

Description
-----------

Relx assembles releases for an Erlang/OTP release. Given a release
specification and a list of directories in which to search for OTP
applications it will generate a release output. That output depends
heavily on what plugins available and what options are defined, but
usually it is simply a well configured release directory.

The *release-specification-file* is optional but otherwise contains
additional specification information for releases.

Building
--------

To build relx and generate a standalone escript executable:

    $ make

This creates the executable `relx`.

Note, if using your own `rebar`, it must at least be > version
2.2.0-20-g6e24cd6. Unfortunately, the 2.2.0 release of rebar is not
sufficient.

Building on Windows
-------------------

To build relx on Windows you'll need to have rebar installed and the path to
the rebar binary added to the `PATH` environment variable. To start the build
use the `bootstrap` batch file:

    c:\> bootstrap

This creates the executable `relx` and the `relx.cmd` shortcut script. Copy
both of these files to a directory and make the directory available to the
`PATH` environment variable.

Config File
-----------

By default `relx` looks for `relx.config` in the current working directory:

```erlang
{release, {relname, "vsn"},
 [app1,
  app2]}.

{extended_start_script, true}.
```

Options
-------

| Short | Long         | Type    | Default | Description                                                                               |
|:-----:|:------------:|:-------:|:------:|------------------------------------------------------------------------------------------- |
| -r    | --root       | string  | ./      | Sets the root of the project |
| -n    | --name       | string  |         | Name for the release that will be generated |
| -v    | --relvsn     | string  |         | Version for the release |
| -g    | --goal       | string  |         | A goal for the system. These are usually the OTP apps that are part of the release |
| -u    | --upfrom     | string  |         | The release to upgrade from. Only valid with relup target |
| -o    | --output-dir | string  | ./      | The output directory for the release |
| -l    | --lib-dir    | string  |         | Additional dirs to search for OTP apps |
|       | --system_libs     | boolean/string  | true | If true include a copy of system libs used to build with, if a path include system libs at that path. If false, do not include system libs |
| -p    | --path    | string  |         | Additional dirs to add to Erlang code path |
|       | --default-libs | boolean | true | Whether to use the default system added lib dirs (means you must add them all manually) |
| -V    | --verbose    | integer | 2       | The verbosity level between 0 and 3 |
| -a    | --override_app | string | | An app name and a directory to override in the form appname:dir |
| -c    | --config     | string  | ./relx.config | Config file path |
|       | --overlay_vars     | string  |  | Path to a file of overlay variables |
|       | --vm_args     | string  |  | Path to a file to use for vm.args |
|       | --sys_config     | string  |  | Path to a file to use for sys.config |
| -d    | --dev-mode     | boolean | false | Symlink all applications and configuration into the release instead of copying|
| -i    | --include-erts | boolean/string | true | If true include a copy of erts used to build with, if a path include erts at that path. If false, do not include erts |

Wiki
----

[relx wiki](https://github.com/erlware/relx/wiki)
