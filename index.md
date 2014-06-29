---
layout: default
title: Relx by erlware
---

Relx assembles releases for an Erlang/OTP release. Given a release specification and a list of directories in which to search for OTP applications it will generate a release output. That output depends heavily on what plugins available and what options are defined, but usually it is simple a well configured release directory.

## Example Configuration

{% highlight bash %}
$ git clone https://github.com/tsloughter/erlangdc2013.git
$ cd erlangdc2013
$ cat relx.config
{release, {erlangdc, "0.0.1"},
 [estatsd
 ,erlangdc]}.
{extended_start_script, true}.

$ ./relx
...
$ ls _rel/erlangdc/
bin  erts-5.10.1  lib  log  releases
{% endhighlight %}

### Documentation

* [wiki](http://github.com/erlware/relx/wiki)
