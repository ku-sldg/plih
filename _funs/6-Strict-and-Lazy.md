---
layout: frontpage
title: Strict and Lazy
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

The first argument to `App` is evaluated to get back a `Lambda`.  The second argument _is evaluated_ and assigned to `a'`.  `eval` then evaluates the lambda body by adding `(i,a')` to the environment and evaluating.  What is stored in the environment is the result of evaluating the `lambda`'s actual parameter implying that it is evaluated before the function gets called.  It should be clear that our evaluator implements call-by-value or strict evaluation.

How hard is it to change to call-by-name?  Turns out rather simple.  If we don't want to evaluate `a` just store `a` in the environment without evaluating it first:

{% highlight haskell %}
eval env (App f a) = let (Lambda i b) = (eval env f)
                     in eval ((i,a):env) b
{% endhighlight %}

All we did was delete the evaluation of `a` and that's it.  In this case, the argument is passed to the `lambda` without evaluation and we have implemented call-by-name or lazy evaluation.  The only additional change is to the type `Env` that must store `FBAE` abstract syntax rather than values:

{% highlight haskell %}
type Env = [(String,FBAE)]
{% endhighlight %}

There you go.  Now we have a lazy interpreter to go with our strict interpreter.

Most languages are strict, but a few like Haskell are lazy.  Why choose one over the other?  As with most decisions, there is good and bad to both.

First let's look at the biggest problem with lazy evaluation semantics.  Look at this function:

{% highlight text %}
bind silly = lambda x in x + x + x + x + x + x + x + x + x + x in
  (app silly (app silly (app silly 1 + 3 - 7 + (app f 5) + 21)))
{% endhighlight %}

This really silly function uses `x` over and over again.  Using strict semantics, the actual parameter to `silly` is evaluated only once.  Using lazy semantics, the actual parameter to silly is evaluated every silly place you see `x`.  That's pretty huge.  Call-by-need evaluation semantics eliminates this problem by replacing every instance of `x` with a kind of pointer to one common `x`.  Whenever `x` is evaluated, that one `x` is evaluated once and all copies are immediately replaced with the same value.

On the other hand, this `if` expression does not evaluate `silly` at all:

{% highlight text %}
bind silly = lambda x in x + x + x + x + x + x + x + x + x + x in
  if 0 then (app silly 5) else 5
{% endhighlight %}

It's never used in the `if` and thus not evaluated.  Using strict semantics it would be.  `if` itself shows the distinction between lazy and strict.  If we wrote `if` as a function, strict semantics causes problems.  Look at this psuedocode:

{% highlight text %}
if x y z = lambda x in lambda y in ...
{% endhighlight %}

Using strict semantics, `x`, `y`, and `z` are all evaluated before `if` is evaluated.  This is a problem because we only want to evaluate one of `y` or `z`.  Using lazy semantics, `if` can be implemented by a simple data type:

{% highlight haskell %}
data Boolean where
  true :: Boolean
  false :: Boolean

if x y z = case x of
             true -> y
             false -> z
{% endhighlight %}

Pretty groovy.  `if` is not special, but is just an ordinary function.

The final pro-lazy argument has to do with infinit structures.  One can define a list in Haskell like this:

{% highlight haskell %}
alt10 = 1:0:alt10
{% endhighlight %}

If this were done in a strict language, it would be infinite.  In Haskell as long as you don't evaluate `alt10` directly.  In strict languages, pointers are used to accomplish a similar behavior.  The pointer value is evaluated without evaluating what it points to.

## Definitions

## Exercises

1. Many languages (Haskell included) allow both strict and lazy evaluation.  Define a new language that implements this feature by defining two versions of `App`, - `AppS` and `AppL`.  Do not worry about a concrete syntax.  Simply replace `App` in the abstract syntax and define a new `eval` function.
 
## Notes
