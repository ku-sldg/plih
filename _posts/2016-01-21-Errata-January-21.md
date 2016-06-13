---
layout: blog
categories: blog
title: Errata January 21
---
A couple of quick notes from class today.

First, when using `define-type`, there are no parentheses around the new type's name.  Specifically, `(AE)` should be `AE` like this:

{% highlight racket %}
(define-type AE
  (num (n number?))
  (add (lhs AE?) (rhs AE?))
  (sub (lhs AE?) (rhs AE?)))
{% endhighlight %}

Second, `define-type` is not a part of the standard Racket (or Scheme) definition.  In DrRacket you need to specify which language to use.  The easiest way to do this is include:

{% highlight racket %}
#lang plai
{% endhighlight %}

at the top of your Racket source file.  This tells DrRacket to use the special extensions for our book.
