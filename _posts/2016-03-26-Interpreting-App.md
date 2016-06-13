---
layout: blog
categories: blog
title: Interpreting App
---
There seems to be some persistent confusion about how an `app` gets interpreted, so I thought a blog post might be in order.

Remember that an app has the form:

{% highlight racket %}
(app fun-expr arg-expr)
{% endhighlight %}

Now let's prepare to evaluate the `app` by evaluating both expressions:

* Evaluate `fun-expr` and save the value in a local variable.  Nothing magic here, just call `interp` on the `fun-expr` with the current value of `ds`
* Evaluate `arg-expr` and sane the value in a local variable.  This is optional, but I think it makes things easier.  Again, nothing magic, just call `interp` on the `arg-expr` with the current value of `ds`

Now you have the values for both the function you're going to call and the value you're going to call it on.  Remember that the result of evaluating `fun-expr` has the form:

{% highlight racket %}
(fun param body)
{% endhighlight %}

You simply need to evaluate the body using the result of evaluating `arg-expr`.  Just one step here:

* Evaluate `body` with a new deferred substitution list with `param` bound to the interpreted `arg-expr`.  This is pretty easy.  Just create a new `ds` from the current `ds` by using `aSub` to bind the parameter value to the parameter name.

Remember that one beauty of the way we are developing interpreters  is we can think about interpreting expressions individually.  Don't try to think about entire programs at once, but just one kind of expression at a time.
