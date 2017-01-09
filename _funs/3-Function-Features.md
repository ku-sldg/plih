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

## Currying

Functions in all our languages thus far take only a single parameter.  What that may seem limiting, it's actually not. _Currying_ or _curried functions_ is a way of constructing multi-parameter functions using only single parameter functions.  If you're a Haskell programmer, currying is something you do all the time.  Look at the signature of an operation like `plus3` that adds three `Int`s as an example:

{% highlight text %}
plus3 :: Int -> Int -> Int -> Int
{% endhighlight %}

Usually we think about such a function as a mapping from three `Int` arguments to and `Int` result because of our upbrining in traditional languages.  However, what it literally defines is a function from the single parameter `Int` to another function of type `[Int] -> [Int] -> [Int]`.  Applying `plus3` to a single argument results in a function:

{% highlight haskell %}
(plus3 1) :: Int -> Int -> Int
{% endhighlight %}

Parenthesis added to the left side for emphasis.  Applying another parameter results in:

{% highlight text %}
((plus3 1) 2) :: Int -> Int
{% endhighlight %}

and another:

{% highlight text %}
(((plus3 1) 2) 3) :: Int
{% endhighlight %}

and we now have a final result.  But the function resulting from `plus3 1` is just as much a result as `6`.  This is something we do all the time in Haskell to define and use partially instantiated functions:

{% highlight text %}
plus2 = plus3 0
{% endhighlight %}

In `FBAE` and subsequent languages currying works the same way.  We simply use `app` to achieve application of a function to an argument.  Given that we had defined `plus3` in `FBAE`, the previous function application would look like this:

{% highlight text %}
(app (app (app plus3 1) 2) 3)
{% endhighlight %}

It's a bit messier, but it works just the same an emphasizes the currying that's happening.

We can easily write `plus3` using `bind` and nested `lambda`s.  Let's walk through it starting with the `bind` of `plus3` to a `lambda` with one argument, 'x':

{% highlight text %}
bind plus3 = lambda x in ?? in 
  ...
{% endhighlight %}

Now we need to figure out what to replace `??` with.  Literally, we want to add `x` to the result of adding two more arguments.  Let's write that much:

{% highlight text %}
bind plus3 = (lambda x in x + ??) in 
  ...
{% endhighlight %}

Now we need to get the next argument.  The only way to do this is to use another nested `lambda`.  Remember, that `plus3 1` should return a function.  What should immediately follow `in` should be a function and that function needs to know about `x`:

{% highlight text %}
bind plus3 = (lambda x in (lambda y in x + y + ??)) in
  ...
{% endhighlight %}

See where we're going?  If we evaluate `plus3 1` what we'll get is `lambda y in x + y + ??` waiting for the `y` parameter.  Another `lambda` will pick up the third integer:

{% highlight text %}
bind plus3 = (lambda x in (lambda y in (lambda z in x + y + z))) in
  ...
{% endhighlight %}

Just what we want.  The `bind` now just needs a body where `plus3` is used.  Let's copy what we did earlier:

{% highlight text %}
bind plus3 to (lambda x in (lambda y in (lambda z in x + y + z))) in
  (app (app (app plus3 1) 2) 3)
{% endhighlight %}

Now we can walk trough the evaluation of the `bind` body tracking the environment along the way:

{% highlight text %}
(app (app (app plus3 1) 2) 3)
== (app (app (app (lambda x in (lambda y in (lambda z in x + y + z))) 1) 2) 3) []
== (app (app (lambda y in (lambda z in x + y + z))) 2) 3) [(x,1)]
== (app (lambda z in x + y + z))) 3) [(x,1),(y,2)]
== x + y + z [(x,1),(y,2),(z,3)]
...
== 6
{% endhighlight %}

We could add additional syntax to make both application and definition of n-ary functions simpler.  We'll leave a bit of that to an exercise.

Curried function semantics is used frequently to define multi-parameter function execution.  Haskell certainly uses this approach as does ML.  Scheme and Lisp on the other hand require explicit currying.  We'll not follow that path right now.

## Untyped Recursion

### Omega

Take a look at the following function application:

{% highlight text %}
(app (lambda x in (app x x)) (lambda y in (app y y)))
{% endhighlight %}

What will any of our evaluators this far do with this?  Let's see:

{% highlight text %}
eval [] (app (lambda x in (app x x)) (lambda y in (app y y)))
== app x x                     [(x,(lambda y in (app y y)))]
== app (lambda y in (app y y)) (lambda y in (app y y))
{% endhighlight %}

Interesting.  It would seem that this particular expression evaluates to itself.  It doesn't stop there.  This is an `app`, not a value, so it will in turn be evaluated and again evaluate to itself.  Which will evaluate to itself again and again and again. It does not terminate.  Thus far, we've not even seen iteration or recursion and suddenly we have an expression that does not terminate when evaluated.

This structure is called the _$\Omega$ combinator_ or simply $\Omega$ and is interesting precisely because when evaluated it does not terminate.  It is the basis for recursion in an untyped language, but obviously not quite the function we want  If you pop $\Omega$ into any of our `FBAE` interpreters you'll see this nontermination in action.

Let's dissect $\Omega$ just a bit more and try to understand what it does.  At its heart it's a simple `lambda` that applies its argument to itself:

{% highlight text %}
(lambda x in (app x x))
{% endhighlight %}

`x` serves as both a function and its argument.  We like to think of `app` as simple function application and in one sense that's what it is.  In another sence, `app` can define computational patterns.  In this case a simple pattern that applies a function to itself that gives us _recursion_.

Hmmm.  Every recursive function you've ever written makes a call to itself.  Like this:

{% highlight text %}
fact x = if x=0 then 1 else x * fact x-1
{% endhighlight %}

Clearly `fact` calls `fact`.  In the $\Omega$ case no such call occurs.  Or does it?  Interestingly neither instance of `lambda` has a name, but they are identical.  The first `lambda` gives the second `lambda` a name when it is instantiated.  Think about it.  When the `app` is evaluated, its argument gets the name `x` and that `x` becomes the same as the function it appears in.  Do you see the recursion?

Passing a copy of `lambda x in (app x x)` to itself and using that copy as a function gives us recursion.  `(app x x)` becomes $\Omega$ in side $\Omega$.  What a beautiful construction!

If we evaluate this expression:

{% highlight text %}
(app (lambda x in (app x x)) (lambda y in y))
{% endhighlight %}

it terminates immediately because the second `lambda` simply returns its argument.  For $\Omega$ to work, the argument `lambda` must be identical to the function `lambda`.

Now we have $\Omega$ and it's groovy and we can't use it for a darn thing other than locking up our evaluator.  Time to move on.

### Y

Omega showed us how `app` within `lambda` can create patterns.  Let's look at another one that is a touch more useful than omega:

{% highlight text %}
bind Y = (lambda f (lambda x in (app f (app x x)))
                   (lambda x in (app f (app x x))))
  in ...
{% endhighlight %}

This expression that we've named `Y` looks a bit like omega, but with a twist.  The first argument to `Y` is `f` which appears in the body in the function position of an `app`.  So, `f` would be some function that we will input to `Y`.  Let's assume that we have such an `f` we'll call `F` and apply `Y` to it and see what we get:

{% highlight text %}
bind Y = (lambda f (app (lambda x in (app f (app x x)))
                        (lambda x in (app f (app x x))))
  in (app Y F)
== (app (lambda x in (app F (app x x))) (lambda x in (app F (app x x))))
{% endhighlight %}

Now it looks even more like $\Omega$.  Lets evaluate the expression a few more times and see what we get:

{% highlight text %}
bind Y = (lambda f (app (lambda x in (app f (app x x)))
                        (lambda x in (app f (app x x))))
  in (app Y F)
== (app (lambda x in (app F (app x x))) (lambda x in (app F (app x x))))
== (app F (app x x)) [(x,(lambda x in (app F (app x x))))]
== (app F (app (lambda x in (app F (app x x))) (lambda x in (app F (app x x)))))
== (app F (app F (app x x))) [(x,(lambda x in (app F (app x x))))]
== (app F (app F (app (lambda x in (app F (app x x))) (lambda x in (app F (app x x))))))
...
{% endhighlight %}

It seems that each time we evaluate `(app Y F)`, we get a new copy of `F` out in front of the expression.  Whatever `F` might be it gets called over and over again with something akin to $\Omega$ as one of its arguments.  Regardless, `F` will be called over and over again.

Having established that, let's think about `F` in two ways.  First, can we program a kind of off switch in `F` that turns off when we've evaluated `F` enough times?  Second, can we grab input data with `F`?  In addition to not terminating, $\Omega$ could not accept any input.  Let's think about the off switch first.  If we can't turn `Y` off then it will be just as groovy as $\Omega$ and just as useless.

Whatever else is true about `F`, its first argument is the `Y` applied to `F`.  Evaluating that is what causes `F` to be called recursively. If we want to turn `(app Y F)` off then we can't make that recursive call.  Thankfully we have an `if` that allows evaluating a condition.  Let's look at a pattern for `F`:

{% highlight text %}
F = lambda g in if c then off else g)
{% endhighlight %}

`F`'s first argument is the recursive call.  If `c` is 1, then `g` is not called and `(app Y F)` terminates.  We don't know what `c` is yet, but we can look at what happens when it is not 1:

{% highlight text %}
(app Y (lambda g in if 0 then off else g))
== (app (lambda x in (app (lambda g in if 0 then off else g) (app x x)))
        (lambda x in (app (lambda g in if 0 then off else g) (app x x))))
== (app (lambda x in off)
        (lambda x in off))
== off
{% endhighlight %}

Bingo.  If `c` is ever 0, the whole thing shuts down and returns a value.  `off` is not really a value in this case, but serves as a placeholder.  What about 1?

{% highlight text %}
(app Y (lambda g in if 1 then off else g))
== (app (lambda x in (app (lambda g in if 1 then off else g) (app x x)))
        (lambda x in (app (lambda g in if 1 then off else g) (app x x))))
== (app (lambda x in (app g (app x x)))
        (lambda x in (app g (app x x))))
...
{% endhighlight %}

That appears to work as well.  So, the inclusion of an `if` in `F` seems to give us the capability of turning off the recursion.

One last problem.  `1` and `0` are great, but we really need the recursion to terminate as the result of a calculated value.  Plus, it would be nice to return something that is, well, calculated rather than some constant value like `off`.

Remember that `g` is the function called to cause recursion and is right now the only argument to `F`.  Let's try adding another that will serve as the data input to the calculation performed by `F`.  Let's see how that works.  First, let's use a concrete value for `F` and although our interpreter doesn't do it, let's hold it abstract.  This particular `F` sums up the values from 0 to its input argument `n`.

{% highlight text %}
F = lambda g in (lambda z in if z then z else z + (app g z-1))
{% endhighlight %}

Now let's set up the `Y` in `bind`.  

{% highlight text %}
bind F = (lambda g in (lambda z in if z then z else z + (app g z-1))) in
  bind Y = (lambda f (app (lambda x in (app f (app x x)))
                          (lambda x in (app f (app x x))))
    in ((app Y F) 5)
{% endhighlight %}

The function we apply to `5` is obtained by applying `Y` to `F`.  Then we apply the result to `5`:

{% highlight text %}
== (app (app (lambda x in (app F (app x x))) (lambda x in (app F (app x x)))) 5)
== (app (app F (app x x)) 5) [(x,(lambda x in (app F (app x x))))]
{% endhighlight %}

Let's evaluate the inner `app` first resulting in `x` bound to half of the `Y` combinator application to `F`.  Now lets expand `F` before going forward and evaluate the outermost `app`:

{% highlight text %}
== (app (app (lambda g in (lambda z in if z then z else z + (app g z-1))) (app x x)) 5) [(x,(lambda x in (app F (app x x))))]
== (app (lambda z in if z then z else z + (app g z-1))) 5) [(g,(app x x)),(x,(lambda x in (app F (app x x))))]
{% endhighlight %}

The result is now `g` bound to `(app x x)` in the environment.  That seems a little odd, but look carefully at the environment.  `x` is already bound to half of the `Y` application.  That's perfect!  `g` is really the recursive call we want to make if that substitution is performed.  One more `app` evaluation binds `z` to 5:

{% highlight text %}
== if z then z else z + (app g z-1) [(z,5),(g,(app x x)),(x,(lambda x in (app F (app x x))))]
{% endhighlight %}

Now we need to evaluated identifiers by replacing them with their values from the environment.

{% highlight text %}
== 5 + (app g 5-1) [(z,5),(g,(app x x)),(x,(lambda x in (app F (app x x))))]
== 5 + (app (app x x) 4) [(z,5),(g,(app x x)),(x,(lambda x in (app F (app x x))))]
{% endhighlight %}

Now we have `5+(app (app x x) 4)`, but remember what we said about `(app x x)`.  Using the current environment we can replace `x` with the `lambda` giving:

{% highlight text %}
== 5 + (app (app (lambda x in (app F (app x x))) (lambda x in (app F (app x x)))) 4) [(z,5),(g,(app x x)),(x,(lambda x in (app F (app x x))))]
{% endhighlight %}

This is exactly what we want.  Compare the second term of the sum with our original expression.  The only difference is we're using 4 rather than 5, but that's exactly what we want!  So the `Y` gives us a recursive operation builder that takes a non-recursive function like `F` and makes it recursive.  All this without `F` knowing about itself!

### Z

The Y just discussed is often called the lazy Y because it only works using lazy evaluation.  An alternative called the _Z combinator_ does the same thing for strict languages with just a few changes.  Z is often called the applicative Y combinator.

The form of the Z is as follows:

{% highlight text %}
bind Z = (lambda f (app (lambda x (app f (lambda v (app (app x x) v)))))
                   (lambda x (app f (lambda v (app (app x x) v)))))) in
   ...
{% endhighlight %}

and unfortunately it is more involved than the traditional Y.

{% highlight text %}
ff = lambda ie (lambda x (if x then x else x (app ie (x - 1))))
{% endhighlight %}

## DeBrujin Numbering

## Definitions

## Exercises

1. Many languages (Haskell included) allow both strict and lazy evaluation.  Define a new language that implements this feature by defining two versions of `App`, - `AppS` and `AppL`.  Do not worry about a concrete syntax.  Simply replace `App` in the abstract syntax and define a new `eval` function.
2. Implement the Z combinator using FBAE with static scoping.
3. Implement `multiply x y` that works by starting with `0` and adding `x` to it `y` times using the Z combinator.
 
## Notes
