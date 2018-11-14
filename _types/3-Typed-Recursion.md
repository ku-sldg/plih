---
layout: frontpage
title: Typed Recursion
use_math: true
categories: chapter
---

$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeofM}\;}
\newcommand\interp{\mathsf{interp}\;}
\newcommand\eval{\mathsf{evalM}\;}
\newcommand\NUM{\mathsf{NUM}\;}
\newcommand\ID{\mathsf{ID}\;}
\newcommand\iif{\mathsf{if}\;}
\newcommand\tthen{\;\mathsf{then}\;}
\newcommand\eelse{\;\mathsf{else}\;}
\newcommand\iisZero{\mathsf{isZero}\;}
\newcommand\bbind{\mathsf{bind}\;}
\newcommand\iin{\mathsf{in}\;}
\newcommand\aand{\;\mathsf{\&\&}\;}
\newcommand\lleq{\;\mathtt{<=}\;}
\newcommand\ttrue{\;\mathsf{true}}
\newcommand\ffalse{\;\mathsf{false}}
\newcommand\tnum{\;\mathsf{TNum}}
\newcommand\tbool{\;\mathsf{TBool}}
\newcommand\llambda{\mathsf{lambda}\;}
\newcommand\aapp{\mathsf{app}\;}
\newcommand\tnum{\;\mathsf{TNum}}
\newcommand\tbool{\;\mathsf{TBool}}
\newcommand\ffix{\mathsf{fix}\;}
$$

# Typed Recursion

## Typing Omega

Having established that the type of a `lambda` is of the form
`D:->:R`, typing an increment function is quite simple:

```text
typeofM cont (lambda (x:TNum) in x + 1)
== TNum -> TNum
```

It follows directly that a `lambda` taking another `lambda` and
applying it to a value is similarly typed:

```text
typeofM cont (lambda (f:Num -> Num) in (lambda (x:Num) in (f x)))
== (TNum -> TNum) -> TNum
```

The first argument to the expression is the `lambda` to be applied
while the second is the number applied.  It is thus quite natural to
have `lambda` expressions passed to other `lambda` expressions as
arguments.  The application in the body of the `lambda` applies the function
argument to a value.  We have seen this before.  Specifically, when we
looked at untyped recursion and the $\Omega$ and `Y`.

Remember $\Omega$, a simple recursive function that does not
terminate:

```text
((lambda x in (x x))
	 (lambda x in (x x)))
```

Let's add a type placeholder, call it `T->T`, to the `lambda` and
determine what a typed version of the $\Omega$ would look like:

```text
((lambda (x:T->T) in (x x))
	 (lambda (x:T->T) in (x x)))
```

Will this work?  Remember that to determine the type of an `f a`
we fine the type of `f` which must be a function type.  Then if `a`
has the same type as the domain of `f`, `f a` has the same type as
the range of the type of `f`.  However, we will never find a type for
`x x` because the type of `x` must be the domain type of `x`.  If
`x` has type `T->T`, then for `x x` to have type `T`, `x` must
also have type `T`.  This will never work as `x` would need to have
both type `T` and `T->T`.  The only way this will happen is when `T` =
`T->T`, something that is not possible in our current type system.

$\Omega$ cannot have a type.  Using the same argument, `Y` cannot have
a type.  Thus, neither can be written in our new language that
includes function types.

## Normalization

_Normalization_ is a term used to talk about termination.  We say that
a _normal form_ is a term in a language that cannot be reduced by
evaluation.  In our languages terms like `1`, `true` and `lambda x in
x + 1` are normal forms because `evalM` simply returns their values
without reduction.  We also define these normal forms to be _values_
representing acceptable evaluation results.

In all the languages we have written so far, the set of values and
normal forms are the same.  What if we removed the evaluation case for
`+` implying that `1+1` would not evaluate further.  Now terms
involving `+` join values as normal forms.  Unlike values, `+` terms
should reduce.  Normal forms that are not values are referred to as
_stuck_ values and represent errors in a language definition.  It is
always desirable to show that the only normal forms in a language are
values.

Normalization is the property that evaluating any term in a
language always terminates resulting in a value.  No non-termination
and no stuck terms.  Evaluation always halts with a value.

As it turns out, normalization and typing are strongly related.  In
our latest language, function types ensure termination.  We saw this
when failing to find a type for $\Omega$, but did not generalize to
all function applications.  Look at the types involved in this simple
evaluation:

```text
((lambda x in x+1) 3)
```

The type resulting from the application is smaller than the type of `lambda`.
This is always true because of the way application types are defined.
Specifically, given `f:D->R` and `(f d)` where `d:D`, the result
will always be `R`.  The base types for numbers and booleans are the
smallest possible types.  Like number and Boolean values, there is no
way to reduce them - they are the base cases for types.  Thus, application
aways makes a type smaller and eventually will get to a base type.
Just like subtracting 1 repeatedly from a number will eventually
result in 0.  Given that's the case, if we do repeatedly execute we
will always get to that smallest type which is always associated with
a value.

There are ins and outs to this normalization property.  It is great to
know that programs termindate in many cases.  But it is not great to
have to know how many times any iterative construct executes when
writing programs.  Furthermore, there are many programs we do not want
to terminate.  Think operating systems or controllers as two examples.
Clearly languages with function types allow this, we just need to
figure out how.

## Manipulating Closures

Lets revisit our original problem by going back and revisit the most
famous of all recursive function, factorial:

```text
bind fact = (lambda x in (if (isZero x) then 1 else x * (fact x-1)))
```

If this is dynamically scoped it executes recursively just fine.  But
when static scoping and types come into play, things go downhill fast.
Here is a basic definition of `fact` that will not execute if
statically scoped:

```text
bind fact = (lambda (x:TNum) in (if (isZero x) then 1 else x * (fact x-1)))
```

Because `fact` is not in scope, technically there is no type for this
expression either.  If we can get `fact` in scope, maybe this will
work.  Is there a way to do recursion in a typed, statically scoped
language?  Our techniques for untyped recursion do not work.  What can
we do?

We need `fact` to be in its closure's environment.  That's a fancy way
of saying `fact` needs to know about itself.  Let's look at the
closure resulting from evaluating  the `lambda` defining `fact`:

```text
(ClosureV "x" TNum (if (isZero x) then 1 else x * (fact x-1)) [])
```

Let's see if it works by applying the closure to 0:

```text
((ClosureV "x" TNum (if (isZero x) then 1 else x * (fact x-1)) []) 0)   []
== (if (isZero 0) then 1 else x * (fact x-1))                               []
== 1
```

Remember that when we execute an application, the environment from the
closure replaces the local environment. That enviornment here is
empty, thus the recursive reference to `fact` is not in the closure's
environment.  `fact 0` works only because the recursive call to
`fact` never occurs.  `fact 0` is the base case and its value can be
calculated directly.  Still can't fined a type for `fact`, but we'll
worry about that later.

Unfortunately, any value other that `0` triggers the recursive call
where `fact` must be found in the environment.  Looking at `fact 1`
demonstrates the problem immediately:

```text
((ClosureV "x" TNum (if (isZero x) then 1 else x * (fact x-1)) []) 1)  env
== evalM (if (isZero 1) then 1 else 1 * (fact 0))                           []
== 1 * (fact 0)                                                            []
== error on lookup of fact in []
```

The actual value of `env` is immaterial here because we're using
static scoping.  The empty environment in the closure is where we look
for the definition of `fact`.

The easiest fix is to simply add `fact` to its closure's environment.
We know the definition of `fact` when it is defined, so we can simply
add it to it's own closure.  That should do the trick because the
lookup of `fact` will find it.  Let's give it a try:

```text
(ClosureV "x" TNum (if x=0 then 1 else x * (fact x-1))
    [(fact,(ClosureV "x" TNum (if ...) []))])
```

There are two closures here.  The outer closure is what we will
evaluate with application and the inner closure defines the value of `fact`
in the outer closure's environment.  Now `fact` will work for `0` as
it did before and should work for other values as well.  Let's give it
a shot for `1`:

```text
((ClosureV "x" TNum (if (isZero x) then 1 else x * (fact x-1)) []) 1) []
== (if (isZero x) then 1 else x * (fact x-1)         [(x,1),(fact,(ClosureV "x" TNum (if ...) []))]
== (if (isZero 1) then 1 else 1 * (fact 0))          [(x,1),(fact,(ClosureV "x" TNum (if ...) []))]
== evalM 1 * (fact 0)                                 [(x,1),(fact,(ClosureV "x" TNum (if ...) []))]
== 1 * ((lambda x in (if (isZero x) then 1 else x * (fact x-1)) [])  0 [(x,1),(fact,(ClosureV "x" TNum (if ...) []))]
== 1 * (if (isZero x) then 1 else x * (fact x-1)     [(x,0)]
== 1 * 1
```

Good for now, but look at the last application where `fact` is no longer in
the environment.  What happened?  The environment of the _inner_
closure becomes the new environment when it is applied.  This means
`fact 2` will fail when the `if` evaluates and tries to apply
fact.  We have a fix for that! Let's just add the closure again - add
the closure to the environment of the closure in the closures'
environment:  (Say that fast 5 times)

```text
(ClosureV "x" (if x=0 then 1 else x * (fact x-1))
    [(fact,(ClosureV "x" (if ...)
       [(fact,(ClosureV "x" (if ...) [(x,??)])),(x,??)])
```

Bingo.  Now the closure in the environment for the closure knows about
the closure.  Now we can call `fact` on 0, 1, and 2.  But not 3.  Do
you see why?  The innermost closure can never have `fact` in its
environment because it is, in effect, the base case.  Any number of
nested closures you choose can always be exceeded by 1.  Build 10 and
`fact` will fail for 11, build 100 and it fails for 101 and so forth.
No matter how deep the nesting, eventually the recurse call to `fact`
fails.

What can we do to solve this?  In the immortal words of Dr. Seuss, no
matter how many turtles we add there is always one at the bottom we
can try to jump under.  We cannot write $\Omega$ thus we cannot write
Y.  We cannot use closure magic.  The only thing we are left with is
adding a new construct to our language with a different execution
behavior.

## The Fix

In this case, the fix is adding a _fixed point_, concrete syntax `fix
t`, to our statically scoped language.  Instead of using the language
to write a fixed point construct like Y, we will build the fixed point
into the language directly and take advantage of controlling
evaluation more precisely.

Fixed points are common structures in mathematics, but we only need to
understand what a basic fixed point structure looks like to solve our
problem.

The rule for the general recursive structure is:

$$\frac{}{\eval \ffix \llambda i\; b = \eval [i\mapsto (\ffix (\llambda i\; b))]\; b}$$

Evaluating `fix` uses substitution to replace the called function with
`fix` over the called function.  Note that `evalM` appears on both
sides of the definition.

The bind evaluates `t` to get a closure.  The body of the closure is
evaluated in `e` replacing `i` with `lambda i b`.  What the heck?

```haskell
evalM env (Fix f) = do { (ClosureV i b e) <- (evalM env f) ;
                        evalM e (subst i (Fix (Lambda i b)) b) }
```

To better understand how the `fix` operation works, let's evaluate
factorial of `3` using our new operation.  First, let's define `f`,
the function we will use to implement factorial:

```text
bind fact = (lambda g in (lambda x in (if x=0 then 1 else x * (g x-1)))) in ...
```

`fact` is not recursive.  It takes a function that it will call in the
recursive case and return something that looks a great deal like
factorial.  Let's do a quick thought experiment to see what `fact`
would look like if it were called on itself:

```text
((lambda g in (lambda x in (if x=0 then 1 else x * (g x-1)))) fact)
== (lambda x in (if x=0 then 1 else x * (fact x-1))))
```

That looks exactly like what we want, but it won't work until we use
`fix` to perform the instantiation of `f`.

After evaluating `f` and pulling the resulting closure apart, we have
the following bindings that will get used in the substitution:

```text
i = g
b = (lambda x in (if x=0 then 1 else x * (g x-1))
e = []
```

The parameter defined by the `fact` `lambda` expression is `g`.  Thus,
the argument name in the closure is `g`.  The body of the `fact`
`lambda` is what we think of as factorial with the recursive call
replaced by a call to `g`.  The environment is empty because there is
nothing defined when we defined the `fact` `lambda`.  Let's start the
evaluation by applying the fixed point of `fact` to `3`:

```text
((fix fact) 3)
```

Note that we are not applying `fact` to `3`, but instead applying the
fixed point of `fact` to `3` to build a recursive function.  To
evaluate the application we evaluate `fact` and apply the resulting value to
`3`.  Let's evaluate `(fix fact)` using the definition from above by
replacing `g` with `(fix (lambda g in b)):

```text
== [g->(fix (lambda g in b))]b 3
== (lambda x in (if x=0 then 1 else x * ((fix (lambda g in b)) x-1) 3
```

Now we have something we understand.  Specifically, application of a
`lambda` to the term, `3`.  Substituting `3` for x results in:

```text
== if 3 = 0 then 1 else 3 * ((fix (lambda g in b)) 3-1)
== 3 * ((fix (lambda g in b)) 2)
```

Well look at that.  We got exactly what we want!  We started by
applying the fixed point of the lambda to a value and we just got the
same thing here.  Exactly the same thing with the argument decremented
by 1.  Let's keep going by applying the same steps again:

```text
== 3 * [g->(fix (lambda g b))] b 2
== 3 * (lambda x in (if x=0 then 1 else x * ((fix (lambda g in b)) x-1) 2
== 3 * if 2 = 0 then 1 else 2 * ((fix (lambda g in b)) 2-1)
== 3 * 2 * ((fix (lambda g in b)) 1)
```

We are recursively executing just like we hoped we would.  Now we just
need to worrying about termination.  Same steps again:

```text
== 3 * 2 * ([g->(fix (lambda g b))] b 1)
== 3 * 2 * ((lambda x in (if x=0 then 1 else x * ((fix (lambda g in b)) x-1) 1)
== 3 * 2 * (if 1=0 then 1 else 1 * ((fix (lambda g in b)) 1-1))))
```

... and again:

```text
== 3 * 2 * 1 * ((fix (lambda g in b)) 0)
== 3 * 2 * 1 * ([g->(fix (lambda g b))] b 0)
== 3 * 2 * 1 * ((lambda x in (if x=0 then 1 else x * ((fix (lambda g in b)) x-1) 0)
== 3 * 2 * 1 * if 0=0 then 1 else 0 * ((fix (lambda g in b)) -1)
```

This time the `if` condition is true, so the function returns `1`
rather than evaluating the fixed point again.  The result is exactly
what we would expect:

```text
== 3 * 2 * 1 * 1
== 6
```

Finally evaluating the resulting product gives us `6` as anticipated.
Our newly added `fix` operation takes a properly formed function and
creates a recursive function.  Let's look back at the `fact` function
given to `fix`:

```text
bind fact = (lambda g in (lambda x in (if x=0 then 1 else x * (g x-1)))) in
   ((fix fact) 3)
```

This looks exactly like our original `fact` definition with a "hole"
for the recursive call.  Where `fact` appears in the initial
definition, the function `g` from the outer `lambda` appears.  This is
the general form for any recursive construction we would like to
create.  Specifically, create the recursive function and replace the
recursive instance with a variable created by an outer `lambda`.

## Typing fix

This entire discussion started with an attempt to create a statically
scoped, well-typed recursive construction.  We could not find a type
for $\Omega$ or Y, we couldn't hack closures, and finally resorted to
extending the core language to include a `fix` operator.  We now have
a statically scoped `fix` expression that will create recursive
constructs for us.

One task remains.  What is the type of `fix`?  Looking at how we
created factorial from `fact` gives is a great clue:

```text
bind fact = (lambda g in (lambda x in (if x=0 then 1 else x * (g x-1)))) in
   ((fix fact) 3)
```

`fix fact` gives us the factorial fucntion.  The previous definition
could be rewritten as:

```text
bind fact = (lambda g in (lambda x in (if x=0 then 1 else x * (g x-1)))) in
   bind factorial = (fix fact) in
     (factorial 3)
```

Looking at factorial this way, it should be clear the type of
`factorial` must be `TNat->TNat`.  Given a number, factorial will
return another number.  What then is the type of `fact`?  It takes a
value `g` and returns a function that calls `g`.  So, the argument to
`fact` must be a function.  The result must also be a function because
it is applied to a value.  `fact` takes a function and returns a
function.  If we call `typeofM` on just `fact` we learn:

```text
typeofM [] (lambda (ie:Nat->Nat) in
             (lambda (x:Nat) in
                (if (isZero x) then x else x + (ie (x-1))))
 == (Nat->Nat) -> (Nat->Nat)
 ```

`fix` takes `fact` and creates `factorial`.  Instead of applying `fix`
to an argument like application, `fix` skips the argument and environment
going straight to the substitution.  Given a function like `fact`,
`fix` creates a recursive function from the body of `fact` using the
function itself.  Just like an application, the type of `fix` is the range
of the input function:

```haskell
typeofM cont (Fix t) = do { (d :->: r) <- typeofM cont t ;
                            return r }
```
