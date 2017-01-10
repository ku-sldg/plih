---
layout: frontpage
title: Untyped Recursion
use_math: true
categories: chapter ch2
---
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

## Definitions

## Exercises

1. Implement the Z combinator using FBAE with static scoping.
2. Implement `multiply x y` that works by starting with `0` and adding `x` to it `y` times using the Z combinator.
 
## Notes
