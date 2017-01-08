---
layout: frontpage
title: Function Features
use_math: true
categories: chapter ch2
---

## Strict vs. Lazy Evaluation

Among the chief distinctions among programming languages is whether function calls are evaluated using a _strict_ or _lazy_ approach.  Most commonly used languages employ strict evaluation where all parameters to a function are evaluated before the function is called.  This approach is also called _call-by-value_ and is used by Scheme, ML, Java, and C among others.

Haskell uses a lazy evaluation mechanism where parameters are evaluated only when their values are needed.  This approach is also called _call-by-name_ and frequently implemented using a _call-by-need_ algorithm.

Before proceeding, let's determine whether our interpreters implement a strict or lazy approach.  Look at the code for evaluating `App` from the interpreter for `FBAE`:

{% highlight haskell %}
eval env (App f a) = let (Lambda i b) = (eval env f)
                         a' = (eval env a)
                     in eval ((i,a'):env) b
{% endhighlight %}

`eval` handles the actual parameter by adding `(i,a')` to the environment and evaluating the function body.  

## Currying

Currying or curried functions is a way of constructing multi-parameter functions using only single parameter functions.  If you're a Haskell programmer, currying is something you do all the time.  Look at the signature of a binary operation like 

{% highlight text %}
f x y z = (((f x) y) z)
{% endhighlight %}

## Untyped Recursion

-- Omega - Infinite combinator

{% highlight text %}
omega = (lambda x in (app x x)) (lambda x in (app x x))
{% endhighlight %}

-- Y - Fixed point Y combinator

{% highlight text %}
y = (lambda f (lambda x in (app f (app x x))) (lambda x in (app f (app x x))))
{% endhighlight %}


-- Z - Applicative Y combinator

{% highlight text %}
z = (lambda f (app (lambda x (app f (lambda v (app (app x x) v)))))
                   (lambda x (app f (lambda v (app (app x x) v))))))
{% endhighlight %}

{% highlight text %}
ff = lambda ie (lambda x (if x then x else x (app ie (x - 1))))
{% endhighlight %}

## DeBrujin Numbering

## Definitions

## Exercises

1. Many languages (Haskell included) allow both strict and lazy evaluation.  Define a new language that implements this feature by defining two versions of `App`, - `AppS` and `AppL`.  Do not worry about a concrete syntax.  Simply replace `App` in the abstract syntax and define a new `eval` function.
 
## Notes
