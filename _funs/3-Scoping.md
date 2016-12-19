---
layout: frontpage
title: Scoping
use_math: true
categories: chapter ch3
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

Where the two interpreters differ is in their implementation of _scope_.  The immediate substitution interpreter implements _static scoping_ while the deferred substitution interpreter implements _dynamic scoping_.  In dynamic scoping, the environment used when evaluating a function is the environment where the function is evaluated.  In the example above, `f` is evaluated in the environment where it is called - `n` is bound to `2` and `f` to the lambda.  Thus, the deferred substitution interpreter implements dynamic scoping.

Clearly dynamic scoping differs from our reference implementation.  However, is that a bad thing?  Most programming language researchers would say yes.  Here's a good example why that's the case:

{% highlight text %}
bind n = 1 in                      [(n,1)]
  bind f = (lambda x in x + n) in  [(f,(lambda...)),(n,1)]
    bind n = app f 1 in            [(n,2),(f,(lambda...)),(n,1)]
      bind n = app f 1 in          [(n,3),(n,2),(f,(lambda...)),(n,1)]
        app f 1                    == 4
{% endhighlight %}

In this expression, every time `f 1` is evaluated, the result is different.  Every time `f 1` is evaluted, the current environment must be known to determine the result.  Same function, same arguments, different result.  This is a debugging nightmare and generally, dynamic scoping is to be avoided.

## Static Scoping

Assuming the immediate substitution interpreter implements a reference interpreter, we need the substituting interpreter to produce the same values.  Specifically, we need `f` to use the environment where it is defined rather than where it is called.  Unfortunately, it's gone when `f` is evaluated.

The approach we'll use is to keep a copy of the environment where a function is defined.  In effect, we'll put a copy of the environment in the lambda value when the lambad is defined.  This structure is called a _closure_.  To implement closures we're going to need to change several things in our interpreter.

### Closures

To understand how a closure will work, lets consider the problematic `bind` evaluation from earlier in the chapter.  Recall that when `x + n` evaluates, the environment used is the dynamic environment where `n` is bound to `2`:

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

However, immediate substitution uses the static binding of `n` to `1` where `f` is defined.  We call the environment `[(n,1)]` where the `f` is defined the *static scope* of `f`.  In contrast, the environment:

{% highlight text %}
[(x,1),(n,2),(f,(lambda ...)),(n,1)]
{% endhighlight %}

where `f` is called is the _dynamic scope_ of `f`.  In general, the term _static_ refers to definition time while the term _dynamic_ refers to run time.  If we could use the static scope of `f` when evaluation occurs, all instances of `app f 1` generate the same value and the evaluation result matches the immediate substitution result.  What we want is the following trace where the static scope gets used for evaluation:

{% highlight text %}
bind n = 1 in                     [(n,1)]
  bind f = (lambda x in x + n) in [(f,(lambda ...)),(n,1)]
    bind n = 2 in                 [(n,2),(f,(lambda ...)),(n,1)]
      app f 1
== app (lambda x in x + n) 1
== x + n                          [(n,1)]
== 1 + n
== 1 + 2
== 3
{% endhighlight %}

Having information from the `lambda` definition plus the static environment in one data structure is a closure.  Psuedo-Haskell for the closure would be:

{% highlight haskell %}
data FBAE where
  ...
  Closure :: String -> FBAE -> Env -> FBAE
  ...
{% endhighlight %}

As we shall see, implementing the closure is a simple extension of what we already do.

### Values

All our interpreters thus far have taking an AST as input and produced an AST of the same type as output.  For example, `eval` for the `FBAE` interpreter has the signature:

{% highlight haskell %}
eval :: FBAE -> FBAE
{% endhighlight %}

If we pu

### Closures as Function Values

{% highlight haskell %}
data FBAEVal where
  NumV :: Int -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)
{% endhighlight %}

## Definitions

* static - definition or compile time
* dynamic - run-time
* static scoping - environment may be uniquely determined at definition time
* dynamic scoping - enviornment is not known until run-time.