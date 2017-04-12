---
layout: frontpage
title: Typed Recursion
use_math: true
categories: chapter ch2
---
# Typed Recursion

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

Bingo.  Now the closure in the environment for the closure knows about the closure.  Now we can call `fact` on 0, 1, 2 and 3.  But not 4.  Do you see why?  The innermost closure will never have `fact` in its environment.  Any number of nested closures can always be exceeded by 1.  Built 10 and `fact` will fail for 11, 100 fails for 101 and so forth.  No matter how many nestings, eventually recursion fails.  Furthermore, this environment  is wasteful in that we may never heed 100 recursive calls.

What can we do to solve this?  No matter how many turtles we add, there is always one at the bottom we can try to jump under.

The rule for the general recursive structure is:

$$\frac{}{[\eval e \fix \llambda i t = eval e i\maplet (fix (\llambda i b))] b}$$

Evaluating `fix` uses substitution to replace the called function with `fix` over the called function.  Note that `eval` appears on both sides of the definition.

{% highlight haskell %}
eval env (Fix t) = let (ClosureV i b e) = (eval env t) in
                     eval e (subst i (Fix (Lambda i b)) b)
{% endhighlight %}
