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
| -r    | --root       | string  | ./      | Name for the release that will be generated |
| -v    | --relvsn     | string  |         | Version for the release |
| -g    | --goal       | string  |         | A goal for the system. These are usually the OTP apps that are part of the release |
| -u    | --upfrom     | string  |         | The release to upgrade from. Only valid with relup target |
| -o    | --output-dir | string  | ./      | The output directory for the release |
| -l    | --lib-dir    | string  |         | Additional dirs to search for OTP apps |
| -p    | --path    | string  |         | Additional dirs to add to Erlang code path |
|       | --disable-default-libs | boolean | false | Disable use of the default system added lib dirs |
| -V    | --verbose    | integer | 0       | The verbosity level between 0 and 2 |
| -a    | --override_app | string | | An app name and a directory to override in the form appname:dir |
| -c    | --config     | string  | ./relx.config | Config file path |

Wiki
----

[relx wiki](https://github.com/erlware/relx/wiki)
