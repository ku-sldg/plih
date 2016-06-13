---
layout: blog
categories: blog
title: with and with
---
I'm seeing some confusion over the difference between `with` and `with`.  Specifically, the symbol `with` is being used in two different ways in class and in projects.  Let me try to explain.

This is concrete syntax for a `with` statement in WAE:

{% highlight racket %}
{with {x 5} {+ 1 x}}
{% endhighlight %}

and this uses `with` as a constructor in the WAE abstract syntax:

{% highlight racket %}
(with 'x (num 5) (add (num 1) (id 'x)))
{% endhighlight %}

The see the difference?  I thought not.

In the first example, the *symbol* `with` is used in the concrete syntax as a keyword.  It is not a function nor a constructor defined in `define-type`, but simply a symbol.

In the second example, the *construtor* `with` is used to build the abstract syntax for a `with` expression.  It is a constructor and is defined in `define-type`.

Think of it this way.  In a more traditional language, one can have a function called `with` and a string value "with".  They are different and they look different.  In Racket, they are different but there is no real syntax difference to tell them apart.  Worse yet, the concrete syntax example is the same as:

{% highlight racket %}
(with (x 5) (+ 1 x))
{% endhighlight %}

because the parenthesis symols and the braces symbols mean the same thing.  (So does the square bracket symbol by the way.)

If I weren't following the book carefully, I would use a different name for the constructor.  Maybe `WAEwith` to indicate a constructor for language WAE.  You are welcome to do this if you find the difference between `with` and `with` in the current discussion confusing.

