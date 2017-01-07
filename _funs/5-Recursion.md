---
layout: frontpage
title: Recursion
use_math: true
categories: chapter ch2
---

> "You're very clever, young man, very clever," said the old lady. "But it's turtles all the way down!"

# Recursion

The rule for the general recursive structure is:

$$\frac{}{[\eval e \fix \llambda i t = eval e i\maplet (fix (\llambda i b))] b}$$

Evaluating `fix` uses substitution to replace the called function with `fix` over the called function.  Note that `eval` appears on both sides of the definition.

{% highlight haskell %}
eval env (Fix t) = let (ClosureV i b e) = (eval env t) in
                     eval e (subst i (Fix (Lambda i b)) b)
{% endhighlight %}
