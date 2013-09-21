---
layout: default
title: Relx Overview
---

Overview
========

Relx is a tool that, given a specification, assembles
releases. That is it makes an assessment of all of the Erlang/OTP Apps
available to it and the constraints that you supply as part of your
configuration and resolves a complete set of OTP Applications and
thier versions for use in the release. This may be a bit hard to
understand if you are not familiar with the way version resolutions
work in package management systems. So lets look at an example.

Lets say that you have the following OTP Applications

{% highlight erlang %}
     app1-1.2
      with dependencies
        app2
        app5
        app6
    app1-1.3
     with dependencies
       app2
       app6
    app2-2.0
     with dependencies
       app6
    app2-2.1
     with dependencies
       app6
       app7
    app3-2.0
    app4-1.0.0
    app5-3.0
    app6-1.0
     with dependencies
       app3
    app7-2.0
{% endhighlight %}

This is the world of OTP Apps your Relx knows about (basically OTP
Apps in the Library Directories you have specified). You have set a
config that looks like the following:

{% highlight erlang %}
     {release, {awesome_supercool, "1.0"},
        [{app1, "1.3", '>='},
         {app2, "2.0", '>'},
         app3]}
{% endhighlight %}

When the Relx process has run you will end up with a complete
release as follows

{% highlight erlang %}
    {release, {awesome_supercool, "1.0"},
      [{app1, "1.3"},
       {app2, "2.1"},
       {app3, "2.0"},
       {app6, "1.0"},
       {app7, "2.0"}]}
{% endhighlight %}
As you can see that is a fully realied view of your direct and
transative dependencies based on the world that Relx knows about
and the constraints that you specified in your configuration.
