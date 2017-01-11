---
layout: frontpage
title: Untyped Recursion
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

# Untyped Recursion

The first solution for recursion in a statically typed language we will explore is writing _fixed-point combinators_ that implement recursion.  These combinators come from the _lambda calculus_ developed by Alonzo Church that along with Turing Machines are the two foundational models of algorithms and computing.  More on that later.  The term _combinator_ simply means a closed expression - one with no free variables.  A _fixed-point_ is a recursive structure used to construct sets.  For this study, you need not know any of these details, but if you are serious about the study of languages learning more about all of them is most definitely in your future!

We will look at three recursive constructs.  The $\Omega$ is a trivial infinitely recursive structure.  We'll not be able to use it for much, but it defines a starting point for the `Y` and `Z` constructs.  `Y` is a lazy fixed-point and `Z` is an extension of `Y` for strict languages.  We'll look carefully at `Y` and take what we learn there to `Z`.

## Omega

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

## Y

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

## Z

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

## Discussion

While $\Omega$, `Y`, and `Z` are not commonly used constructs in common language settings, they are here because they are beautiful.  We have defined recursion without the concept of a global name space.  Neither `Y` or `Z` or the functions that are arguments to them directly reference themselves.  The only place we define names is in `lambda` definitions or `bind` definitions that could be replaced by `lambda` applications!  This is elegant and beautiful.  If you are interested in pursuing this kind of thing further, a good course in programming language semantics should be on your list of courses to take.

The `Y` combinator is historically important as it is an implementation of Curry's Paradox.  That would be Haskell Curry for whom Haskell and currying are named.  Curry denied actually inventing currying, but that's another story entirely.  Curry's Paradox is important because it introduces a contradiction in the lambda calculus that renders it useless as a deductive system.  Another thing worth looking at that we don't have time for here.

Finally, the Y Combinator that you're likely more familiar with is the venture capital firm started by Paul Graham.  Graham has several accomplishments to his name before the Y Combinator - PhD in Computer Science from MIT, MS in Painting from NYU, developed the first online web store for Yahoo (in Lisp I might add), just for starters.  Hopefully you can see why he chose the name Y Combinator for his firm.  The programming `Y` effectively creates copies of itself as many times as needed.  That's precisely the same thing that Graham's company does.

## Definitions

## Exercises

1. Implement the Z combinator using FBAE with static scoping.
2. Implement `multiply x y` that works by starting with `0` and adding `x` to it `y` times using the Z combinator.
 
## Notes
