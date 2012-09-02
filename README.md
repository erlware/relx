# NAME

relcool - A release assembler for erlang

# SYNOPSIS

relcool [*options*] [*release-specification-file*]

# DESCRIPTION

Reltool assembles releases for an Erlang/OTP release. Given a release
specification and a list of directories in which to search for OTP
applications it will generate a release output. That output depends
heavily on what plugins available and what options are defined, but
usually it is simple a well configured release directory.

   relcool --relname foo --relvsn 0.0.1 --target-spec myapp --target-spec getopt>=0.5.1 -o output-dir --targz

The *release-specification-file* is optional but otherwise contains
additional specification information for releases.

# OPTIONS

-n *STRING*, \--relname *STRING*
:   Specify the name for the release that will be generated

-v *STRING*, \--relvsn=*STRING*
:   Specify the version for the release

-t *STRING*, \--target-spec *STRING*
:   Specify a target constraint on the system. These are usually the OTP
  Apps that are part of the release

-o *STRING*, \--output-dir=*STRING*
:  The output directory for the release. This is `./` by default.


# CONFIGURATION FILES

Configuration files

# SEE ALSO

`reltool` (1).
