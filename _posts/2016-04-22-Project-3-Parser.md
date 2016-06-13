---
layout: blog
categories: blog
title: Project 3 Parser
---
As promised I have uploaded an implementation of the [parser for Project 3](http://palexand.github.io/eecs662/resources/cfwaer-parser.rkt).  You'll find three things in the file.  First, I've given you a definition for the CFAER abstract syntax.  You can't write a parser without the abstract syntax and it's rather simple to define.

Second is the parser itself.  Note that it implements functions _without_ parenthesis around the argument.  As an example:

{% highlight racket %}
{fun x {+ x 1}}
{% endhighlight %}

rather than:

{% highlight racket %}
{fun {x} {+ x 1}}
{% endhighlight %}

This is not a particularly big change, particularly given that I wrote the parser for you.

Finally, when the file is loaded the parser is called on a collection of test cases.  You can throw this out if you want.  I included it to ensure that the parser actually is doing its job.
