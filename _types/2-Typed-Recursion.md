---
layout: frontpage
title: Typed Recursion
use_math: true
categories: chapter ch2
---

> "You're very clever, young man, very clever," said the old lady. "But it's turtles all the way down!"

# Recursion

Recursion is among the simplest and most beautiful concepts in computer science.  In the simplest sense, a recursive structure refers to itself.  We most frequently think about recursion in functions like factorial where the function calls itself.  Among the most beautiful things about recursion is it can be added with almost no language extensions. For example, factorial looks something like this:

{% highlight text %}
bind fact =
  lambda (x:TNum) in
     if x=0 then 1 else x * (app fact x-1) in
  app fact 3
{% endhighlight %}

We can write recursive functions with no extension to our language with virtually no extensions.  Now we have an iteration capability similar to what is provided by Haskell and Lisp and available in virtually every modern language.

Let's evaluate this new structure using our most receive statically scoped interpreter for `FBAE` with types:

{% highlight haskell %}
interp "bind factorial ..."
NumV *** Exception: Varible factorial not found 
{% endhighlight %}

Clearly `fact` is defined, so why did we get an error message?  Is `interp` badly implemented?  As it turns out, no.  The problem is the definition of `fact`.

Now let's take a step backwards and evaluate `fact` with our dynamically scoped interpreter for `FBAE`:

{% highlight haskell %}
interp "bind factorial ..."
(Num 6)
{% endhighlight %}

Works fine!  Why?  Obviously something different in the way static and dynamic scoping handle definitions.  Let's look at dynamic scoping and why `eval` works and then try to fix (no pun intended) the statically scoped `eval` to include recursion.

## Recursion and Dynamic Scoping

To understand why recursion works with no extensions using dynamic scoping we need only look at what makes dynamic scoping dynamic.  Remember, a dynamically scoped interpreter tries to evaluate a variable in the scope where it is used.  Let's look at `fact` again annotated with definitions in scope:

{% highlight text %}
bind fact =                                    []
  lambda (x:TNum) in                           [(x,??)]
     if x=0 then 1 else x * (app fact x-1) in  [(fact,(Lambda "x" ...))]
  app fact 3
{% endhighlight %}

Execution begins with nothing in scope when `bind` begins evaluation.  As the `lambda` begins, it's parameter is added to scope.  Thus, the body of the `lambda` can use `x`.  When the `lambda` closes, `x` leaves scope, but as the `bind`'s declaration closes `fact` is added.  When `fact 3` evaluates, `fact` is define.

The call to `fact` originally occurs within the scope of the `bind` defining it.  More importantly, the recursive call to factorial occurs in the same scope.  There are two calls to `fact` - one in `bind`'s body and another within `fact` itself.  When `fact` is called intially, it has clearly been defined.  When it is called recursively, it is in the scope of the original call - where it is evaluated.  Thus, the recursive call works just fine.

## Recursion and Static Scoping

To understand why recursion fails using static scoping, we need to look carefully at scoping again:

{% highlight text %}
bind fact =                                    []
  lambda (x:TNum) in                           [(x,??)]
     if x=0 then 1 else x * (app fact x-1) in  [(fact,(Closure "x" ...))]
  app fact 3
{% endhighlight %}

This time we need to remember that the `lambda` looks for identifiers in the scope where it is defined rather than where it executes.  Looking at `lambda`, the only variable in scope in its body is `x`.  It doesn't yet have a value, but it is in scope when it is called on an actual parameter.  In the body of `bind`, where `fact` is applied to `3` its definition is in scope.  Why then do we get an error saying `fact` is not in scope?

We get a hint if we do the same evaluation, but this time using `0` as the argument to `fact`:

{% highlight text %}
bind fact =                                    []
  lambda (x:TNum) in                           [(x,??)]
     if x=0 then 1 else x * (app fact x-1) in  [(fact,(Closure "x" ...))]
  app fact 0
== 1
{% endhighlight %}

When called on `0`, `fact` in fact works.  `fact 1 == 0`.  What about `fact 1`?  If you try this, `fact` crashes with the same error that `fact` is not found.  What gives?  It clearly was found once.  Why not the second time?

The key is understanding `fact` is evaluated in two places.  The first time in the body of `bind` where it is defined.  The second time, `fact` is evaluated in the body of `fact` where it is not defined.  The only identifier in scope is `x`.  A way to understand this is that `fact` does not know about itself because it is in scope only in the body of `bind`.

## The Fix 

We said that dynamic scoping is not what we want.  Is there a way to do recursion in a statically scoped language?  What would we need to do?  Obviously, we need `fact` to be in its closure's environment.  That's a fancy way of saying `fact` needs to know about itself.  Let's look at the closure resulting from evaluating  the `lambda` defining `fact`:

{% highlight text %}
(ClosureV "x" (if x=0 then 1 else x * (app fact x-1)) [(x,??)]
{% endhighlight %}

Nothing particularly interesting to see here except the recursive reference to `fact` and `fact` not being in the environment.  The easiest thing to do would be add `fact` to its closure's environment:

{% highlight text %}
(ClosureV "x" (if x=0 then 1 else x * (app fact x-1))
    [(fact,(ClosureV "x" (if ...) [(x,??)])),(x,??)])
{% endhighlight %}

Now `fact` will work for `0` as it did before and also for `1`.  Looking  up `fact` in the closure's environment results in the same closure.  Or does it?  Look carefully at the closure's environment.  It only contains an entry for `x`.  Worked for 0 and 1, how about 2?  let's add the closure again:

{% highlight text %}
(ClosureV "x" (if x=0 then 1 else x * (app fact x-1))
    [(fact,(ClosureV "x" (if ...)
       [(fact,(ClosureV "x" (if ...) [(x,??)])),(x,??)])
{% endhighlight %}

Bingo.  Now the closure in the enviornment for the closure knows about the closure.  Now we can call `fact` on 0, 1, 2 and 3.  But not 4.  Do you see why?  The innermost closure will never have `fact` in its environment.  Any number of nested closures can always be exceeded by 1.  Built 10 and `fact` will fail for 11, 100 fails for 101 and so forth.  No matter how many nestings, eventually recursion fails.  Furthermore, this environment  is wasteful in that we may never heed 100 recursive calls.

What can we do to solve this?  No matter how many turtles we add, there is always one at the bottom we can try to jump under.


The rule for the general recursive structure is:

$$\frac{}{[\eval e \fix \llambda i t = eval e i\maplet (fix (\llambda i b))] b}$$

Evaluating `fix` uses substitution to replace the called function with `fix` over the called function.  Note that `eval` appears on both sides of the definition.

{% highlight haskell %}
eval env (Fix t) = let (ClosureV i b e) = (eval env t) in
                     eval e (subst i (Fix (Lambda i b)) b)
{% endhighlight %}
