---
layout: frontpage
title: Scoping
use_math: true
categories: chapter ch2
---

# Static and Dynamic Scoping

Before going further, let's take a shot at understanding why the two interpreters behave differently on the same syntax.  First, we will examine a case where they produce the same result:

{% highlight text %}
bind n = 1 in
  bind f = (lambda x in x + n) in
	app f 1
{% endhighlight %}

First, the immediate substitution interpreter defined by `evals`:

{% highlight text %}
bind n = 1 in
  bind f = (lambda x in x + n) in
	app f 1
==
bind f = (lambda x in x + 1) in
  app f 1
== app (lambda x in x + 1) 1
== 1 + 1
== 2
{% endhighlight %}

Each step represents one recursive call to `evals`.  The first step immediately substitutes `1` for `n` throughout the remainder of the expression.  The second does the same for `f`.  Finally, the `app` evaluates substituting `1` for `x` in `x+1`.  The resulting value is `2`.

Now let's look at the defered substitution interpreter implemented by `eval`:

{% highlight text %}
bind n = 1 in                     [(n,1)]
  bind f = (lambda x in x + n) in [(f,(lambda...)),(n,1)]
	app f 1                       [(f,(lambda...)),(n,1)]
== app (lambda x in x + n) 1
== x + n                          [(x,1),(f,(lambda...)),(n,1)]
== 1 + 1
== 2
{% endhighlight %}

The first `bind` adds a binding from `n` to `1` while the second adds a binding from `f` to the lambda expression.  The term `app f 1` is evaluated in the context of the resulting environment.  `f` is first replaced by the lambda, then $\beta$-reduction adds a binding of `1` to `x` in.  Now the term can be evaluated.  `x` is replaced by `1` and `n` by `1` and we're done.  The result is again `2`.

What happens in the second exam using nested binds?  Again, let's look at the immediate substitution interpreter:

{% highlight text %}
bind n = 1 in
  bind f = (lambda x in x + n) in 
    bind n = 2 in
      app f 1
==
  bind f = (lambda x in x + 1) in 
    bind n = 2 in
      app f 1
==
  bind n = 2 in
    app (lambda x in x + 1) 1
== app (lambda x in x + 1) 1
== 1 + 1
== 2
{% endhighlight %}

Immediate substitution results in `2` again following roughly the same steps as before.  The interesting step to pay attention to is the second substitution of `2` for `n`.  The first substitution replace `n` in the `lambda`, thus no `n` is present for the inner `bind` to replace.

Now compare to the deferred substitution evaluator:

{% highlight text %}
bind n = 1 in                     [(n,1)]
  bind f = (lambda x in x + n) in [(f,(lambda ...)),(n,1)]
    bind n = 2 in                 [(n,2),(f,(lambda ...)),(n,1)]
      app f 1
== app (lambda x in x + n) 1
== x + n                          [(x,1),(n,2),(f,(lambda ...)),(n,1)]
== 1 + n
== 1 + 2
== 3
{% endhighlight %}

The result is `3`, but why?  The second `bind` is key to understanding what happens.  `bind` adds bindings to the environment when deferring substition while `bind` substitutes immediately when using direct substitution.  By the time deferred substitution occurs, `(n,2)` has been added to the environment where it shadows the original `(n,1)` binding.  Thus, when `x+n` is evaluated, we get `1+2` rather than `1+1`.

## Dynamic Scoping

Where the two interpreters differ is in their implementation of _scope_.  The immediate substitution interpreter implements _static scoping_ while the deferred substitution interpreter implements _dynamic scoping_.

## Static Scoping

Assuming the immediate substitution interpreter implements a reference interpreter, we need the substituting interpreter to produce the same values.

### Closures as Function Values

A *closure* represents a function value that includes the evironment where the function is defined.

{% highlight haskell %}
data BAEVal
  NumV :: Int -> BAEVal
  ClosureV :: String -> FBAE -> Env -> BAEVal
{% endhighlight %}
