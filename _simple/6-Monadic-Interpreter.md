---
layout: frontpage
title: Monadic Redux
use_math: true
categories: chapter ch1
---

$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeof}\;}
\newcommand\interp{\mathsf{interp}\;}
\newcommand\eval{\mathsf{eval}\;}
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
$$


> In addition to it being useful, it is also cursed and the curse of the monad is that once you get the epiphany, once you understand - "oh that's what it is" - you lose the ability to explain it to anybody.
>
> -- Douglas Crockford

# Monadic Interpreters

The `Maybe` type class is the core sequencing construct that will form the heart of our first evaluator and eventual type inference routines. The classic `Maybe` definition provides two constructors, `Just x` and `Nothing`.  By convention `Just x` contains a value resulting from a successful computation while `Nothing` indicates an error or exception.  Literally, the computation result is `Nothing`.  This makes it trivial to use `Maybe` as monad and the `do` notation to structure the kinds of computations necessary for evaluation.

## Maybe, Bind, Return

To understand how the `Maybe` monad will be used, let's take a quick look at a definition of `Maybe` as an instance of `Monad`:

```haskell
instance Monad (Maybe e) where
  return = Just
  Just m >>= k = (k m)
  Nothing >>= _ = Nothing
```

All `Monad` instances must define `return` and `>>=`, the infix representation for `bind`.  For `Maybe`, `return` is defined as the `Just` constructor making `return x` the same as `Just x`:

```haskell
return = Just
```

We use `Just x` to indicate good values, so `return x` will be used at the end of `do` expressions when a good value should be returned.  As an example, `return (Num 1)` results in `Just (Num 1)`.  One may use `return` wherever `Just` is used, so choose based on what you want your code to say.

In contrast, `Nothing` results in nothing at all.  Hold that thought.  The choice is not at all arbitrary given that we use the built-in `Maybe` implementation.  There is no monad instance method for error, but if there were it would evaluate to `Nothing`.

Two cases define the behavior of `>>=` for `Maybe`'s two constructors and an arbitrary second argument.  `>>=` is an infix operation, thus its definition may look a bit odd:

```haskell
  Just m >>= k = (k m)
  Nothing >>= _ = Nothing
```

The line first says that given `(Just m)` and a function `k` over the type of `m`, call `k` on `m`.  Pretty simple, but lets say it again.  `(Just m) >>= k` returns `(k m)`.  Binding `(Just m)` to `k` results in `(k m)`.  That's really all there is two it.  Bind takes an instance of `Just`, pulls out the argument, and applies `k` to it.

But there's a bit more to it.  The type of `bind` say something about the type of `k`:

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Yikes.  Just read carefully and you'll be just fine.  Let's dump the notation and just talk about `Maybe`:

```haskell
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
```

Maybe this is a bit clearer.  Remember that `a` and `b` are type variables, not term variables.  Their values are types.  In this case, the definition says the function argument to `>>=` must take a type `a` and produce a `Maybe b`.  The output type need not be the same as the input type.  Let's forget that for now, but it is an important `Monad` feature.  So, `>>=` does what we said earlier.  It pulls the argument to `Just` out and applies a function *whose range must also be a `Maybe`*.  But why?  We'll talk through this in a minute, but think carafully about what this might do:

```haskell
(Just 3) >>= j >>= k >>= l >>= m
```

Maybe execute `j`, `k`, `l` and `m` in sequence?  Pretty cool actually, but let's move on.

The second case says that given `Nothing` as the first argument to `>>=` return `Nothing` regardless of the second argument.  Again pretty simple, but lets say it again.  `Nothing >>= k` will simply return `Nothing` and return it regardless of what `k` is.  `Nothing` simply passes through the bind operation as if `k` were an identity function.  Look at this:

```haskell
Nothing >>= j >>= k >>= l >>= m
== Nothing
```
regardless of what `j`, `k`, `l` and `m` are.

Remember the choice of `Just` for values and `Nothing` for errors? Thinking about `>>=` in those terms it would seem `>>=` applies a function to a value in the `Just` case and passes an `Nothing` through  in the `Nothing` case.  This is exactly the behavior we want if we're executing operations in sequence.  It is exactly the monad behavior our language interpreters are structured around.

Let's look at the concept abstractly and then get concrete with some examples. If `x` is a value and `a`, `b`, and `c` are a sequence of 3 operations that might throw errors, the `>>=` behavior is exactly what we want:

1. Apply `a` to `x`.
2. If successful apply `b` to `(a x)`.
3. If not successful, don't apply `b` and return the error from `(a x)`.
4. If applying `(b (a x))` is successful, apply `c` to the result.
5. If not successful, don't apply `c` and return the error from `(b (a x))`.

If applying `a` generates an error, it will be passed through as if `b` generated it.  If `b` geneates an error, it will be passed through as if `c` generated it.  Keep going and what you'll end up with is either `Just c(b(a(x)))` or `Nothing`.  But *you don't write the code to manage errors*.  The `Maybe` monad takes care of it for you in the background.  In essence, this is what a monad always does.  A monad always takes care of something in the background that is inherent to the computation being performed.  A monad implements a model of computation.

Now we're getting weird.  Let's get a bit more concrete and look at using the `Maybe` monad and some notations that make it more comfortable.

Write an expression that does the following:

1. Subtract 10 from an initial value and throw an error if negative
2. Square the result from 1. and throw an error if odd
3. Subtract 5 from the result of 2.

Now let's write a set of functions that perform these operations, including generating errors.  We'll use `Just` to return values and `Nothing` to return errors as is common with `Maybe`:

```haskell
a = \x -> if x<10 then Nothing else (Just (x-10))
b = \y -> if (y `mod` 2)==0 then (Just (y*y)) else Nothing
c = \z -> (Just (z-5))
```

Hopefully it's clear these expressions perform the three operations and check for local errors.  Names for the expressions aren't necessary, but will make things a bit simpler.  Without using `Maybe` as a monad, we can compose these operations to do what we want on the value 10:

```haskell
case (a 10)
  Nothing -> Nothing
  (Just y) -> case (b y)
                 Nothing -> Nothing
                 (Just z) -> (c z)
== Just -5
```

Not horrible, but when composing the operations this implementation must worry about pushing around the error messages.  The `case` expressions implement managing errors in this fashion.  Now let's use the `Maybe` as a monad and take advantage of bind:

```haskell
(return 10) >>= a >>= b >>= c
== Just (-5)
```

Remember what `>>=` does.  It takes a `Maybe` value does one of two things.  If the input is `Just x` it performs an operation and returns the result or an error.  If the intput is `Nothing` it just returns nothing.  Lets think this through.  `(return 10)` is equal to `(Just 10)`, so `a` will perform it's operation that will generate `Just` or `Nothing`.  In this case, `(a 10)` returns `(Just (10-10))` or `(Just 0)`.  So:

```haskell
(return 10) >>= a
== (Just 0)
```

Now `((return 10) >>= a)` is bound to `b`:

```haskell
((return 10) >>= a) >>= b
== (Just 0) >>= b
```

`(b 0) = (Just 0)` because `0` is even.  The result is now bound to `c`:

```haskell
(((return 10) >>= a) >>= b) >>= c
== (Just 0) >>= c
```
`c 0 = (Just -5)` because `c` always subtracts 5 from its input and never generates an error. Thus our final result is:

```haskell
(Just -5)
```

What we get is exactly what we want `Just (c (b (a 10)))`.  To which you should say big deal.  If no errors occur its easy to write any fragment.  Let's try a case that does throw an error:

```haskell
(return 11) >>= a >>= b >>= c
```

Looking first at `(return 11) >>= a` works as it did before.  `11 <= 10` so we get `(Just 1)` and the result is once again bound to `b`:

```haskell
(Just 1) >>= b
```

`b` responds to this input differently because the square will be odd.  This time it returns `Nothing` and we must evaluate:

```haskell
Nothing >>= c
```

The case for `>>=` with an input of `Nothing` immediately returns `Nothing` without invoking `c`.  `c` needn't worry about implementing a pass-through for errors that come before it in sequence because it is never called if `Nothing` is input.  This behavior is what we always want if `Nothing` ever gets generated.

One more, this time with emphasis:

```haskell
(Just 9) >>= a >>= b >>= c
== Nothing
```

In this case `(a 9)` results in `Nothing` because `9<10`.  Now the magic happens:

```haskell
(return 9) >>= a >>= b >>= c
== Nothing >>= b >>= c
== Nothing >>= c
== Nothing
```

Each time `Nothing` is bound to a function, `Nothing` results because of the definition of `>>=`.  Not because of the definition of any particular participating function, but because of the `Maybe` monad itself.  Any function that consumes a value and produces a `Maybe` result can be dropped and the same behavior results.

The only usage issue is putting `10` in the `Maybe` type using `return` before beginning.  `Just` would have worked equally well, but `return` is general to any monad.  We call this _lifting_ `10` into the `Maybe` type.  Small price to pay for not managing all of the error handling.  We can even get rid of that by embedding the expression in a function:

```haskell
f x = (return x) >>= a >>= b >>= c
```

Pretty cool, but there's even more. The ever present `do` notation.

The previous implementation uses names for the various operations composed using `bind` in the examples above.  To start to understand `do`, let's pull the names off and use the expressions directly in our composition:

```haskell
(Just 10)
>>= \x -> if x<10 then Nothing else (Just (x-10))
>>= \y -> if (y `mod` 2)==0 then (Just (y*y)) else Nothing
>>= \z -> (Just (z-5))
```

With a bit of formatting magic we get:


```haskell
(Just 10) >>= \x ->
    if x<10 then Nothing else (Just (x-10)) >>= \y ->
       if (y `mod` 2)==0 then (Just (y*y)) else Nothing >>= \z ->
          (Just (z-5))
```

Literally nothing changes other than replacing names with functions and how the expression is indented.

The reason for the reformatting is to associate the input parameter for each function with the expression it is bound to by the function call.  `x` is bound to `Just 10`, `y` is bound to evaluating the first `if` expression and `z` is bound to evaluating the second `if` expression.  Now lets translate each instance of `>>=` into an instance of `<-` using the following transformation:

```haskell
m >>= \n == n <- m
```

The `do` notation is largely just a syntax transformation in the compiler to do this and a couple of other nifty things.  Performing this transformation gives us this `do` expression:

```haskell
x <- (Just 10)
y <- if x<10 then Nothing else (Just (x-10))
z <- if (y `mod` 2)==0 then (Just (y*y)) else Nothing
(Just (z-5))
```

Do you recognize that?  Maybe with a few more decorations and formatting:

```haskell
do x <- (Just 10)
   y <- if x<10 then Nothing else (Just (x-10))
   z <- if (y `mod` 2)==0 then (Just (y*y)) else Nothing
   (return (z-5))
```

Bingo!  We have the `do` notation working backwards from the `>>=` notation.  Of course we usually go the other way, but this explains the "magic" of the `do`.  It's just syntax that will work with any monad.

One other thing to point out.  Using the named functions and `>>=` has one restricting side effect that the `do` notation and the `bind` notation it is derived from do not.  Consider this:

```haskell
do x <- (Just 10)
   y <- if x<10 then Nothing else (Just (x-10))
   z <- if (y `mod` 2)==0 then (Just (y*y)) else Nothing
   (return (z+x+y))
```

where `x` and `y` are used later than in the original expression.  If you use named functions this won't work because of the statically scoped nature of Haskell.  We'll discuss this later, but for now just try to rewrite the last `do` notation using the explicitly named functions and see what happens.

## Discussion

We have now tempted fate by trying to understand the `Monad` instance `Maybe` and explain the `do` notation.  Fear not.  To move forward you need only understand that in the notation:

```haskell
do m <- n
   p <- q
   ...
   return z
```

`n` is evaluated first. If `Just x` is returned `m` is bound to `x` and control moves to `p <- q`.  `q` is evaluated and the process repeats.  If `Nothing` is ever returned, then all subsequent operations are skipped and `do` returns `Nothing`.  So, whenever `Nothing` results execution halts and we fall through.  This is exactly what we want.  A kind of exception handling where `Nothing` represents an exception.

Let's build some interpreters!
