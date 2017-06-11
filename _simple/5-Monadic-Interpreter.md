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

The `Maybe` type class used in both the interpreter and type inference routine for `ABE` is built in to Haskell.  We used `Maybe` as a construction for values using the convention that `Just` contains values and  `Nothing` indicates an error.  This made it trivial to use a monad and the `do` expression to determine what kind of thing is returned by `eval` or `typeof`.

## Maybe and bind

To understand how the `Maybe` monad is used, let's take a quick look at the monad instance for `Maybe`:

```haskell
instance Monad (Maybe e) where
        return = Just
        Nothing  >>= _ = Nothing
```

Recall that all monads define `return` and `>>=`, the infix representation for `bind`.  `return` is defined as the `Just` constructor, so `return x` is the same as `Just x`.  In our implementation using `Maybe`, remember that we used `Just` to construct good values and `Nothing` to indicate errors.  `Just (Num 1)` returns `1` while `Nothing` returns a nothing at all.  Hold that thought.  The choice is not at all arbitrary given that we used the built-in `Maybe` implementation.

Two cases define the behavior of `>>=` for `Maybe`'s two constructors.  The first says that given `(Just m)` and a function `k` over the type of `m`, call `k` on `m`.  Pretty simple, but lets say it again.  `(Just m) >>= k`  simply takes the `m` and calls `k` on it.

The second case says that given `Nothing` return `Nothing`.  Again pretty simple, but lets say it again.  `Nothing >>= k` will simply return `Nothing` and return it regardless of what `k` is.  `Nothing` simply passes through the bind operation as if `k` were an identity function.

Remember the choice of `Just` for values and `Nothing` for errors?
Thinking about `>>=` in those terms it would seem `>>=` applies a function to a value and passes an error through.  This is exactly the behavior we want if we're executing operations in sequence.  It is exactly the monad behavior `eval` is structured around.

Let's look at the concept abstractly and then get concrete with some examples. If `x` is a value and `a`, `b`, and `c` were a sequence of 3 operations that might throw errors, the `>>=` behavior is exactly what we want:

1. Apply `a` to `x`.
2. If successful apply `b` to `(a x)`.
3. If not, generate an error, don't apply `b`, and pass the error forward.
4. If applying `(b (a x))` is successful, apply `c` to the result.
5. If not, generate an error, don't apply `c` and pass the error forward.

If applying `a` generates an error, it will be passed through as if `b` generated it.  If `b` geneates an error, it will be passed through as if `c` generated it.  Keep going and what you'll end up with is either `Just c(b(a(x)))` or `Nothing`.  But *you don't write the code to manage errors*.  The `Maybe` monad takes care of it for you in the background.  In essence, this is what a monad always does.  It takes care of something in the background that is inherent to the computation being performed.  A monad instance implements a model of computation.

Now we're getting weird.  Let's get a bit more concrete and look at using the `Maybe` monad and some notations that make it more comfortable.

First let's write an expression that starts with a value, subtracts 10 and squares the result and subtracts 5:

$$(x-10)^2-5$$

If the original difference is negative or the result of the square is odd we want to throw an error.  First define the three operations as functions using `Maybe` to encode values and error messages:

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

Not horrible, but when composing the operations you must worry about pushing around the error messages.  Now let's use the `Maybe` as a monad and take advantage of bind:

```haskell
(Just 10) >>= a >>= b >>= c
== Just (-5)
```

Can you tell the difference in the two code sequences?  Let's try two additional cases:

```haskell
(Just 11) >>= a >>= b >>= c
== Nothing

(Just 9) >>= a >>= b >>= c
== Nothing
```

The only price to pay is putting `10` in the `Maybe` type using `Just` before beginning.  We call this _lifting_ 10 into the `Maybe` type.  Small price to pay for not managing all of the error handling.  We can even get rid of that by embedding the expression in a function:

```haskell
f x = (Just x) >>= a >>= b >>= c
```

Pretty cool.

One more thing - the `do` notation.  I mentioned that as something we would use earlier.  I used names for the various functions I composed in the `Maybe` example above.  Let's pull the names off and use the expressions directly in our composition:

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

Literally nothing changed other than how the expression is indented.  Try it for yourself and see how it works.

The reason for the reformatting is to associate the input parameter for each function with the expression it is bound to by the function call.  `x` takes `Just 10`, `y` takes the first big `if` and `z` takes the second big `if`.  If we use `<-` as a kind of assignment operation and ditch the bind operation we can break up the functions to look like this:

```haskell
x <- (Just 10)
y <- if x<10 then Nothing else (Just (x-10))
z <- if (y `mod` 2)==0 then (Just (y*y)) else Maybe
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

One other thing to point out that is rather important.  Using the named functions has one restricting side effect that the `do` notation and the `bind` notation it is derived from do not.  Consider this:

```haskell
do x <- (Just 10)
   y <- if x<10 then Nothing else (Just (x-10))
   z <- if (y `mod` 2)==0 then (Just (y*y)) else Nothing
   (return (z+x+y))
```

where `x` and `y` are used later than in the original expression.  If you use named functions this won't work because of the statically scoped nature of Haskell.  We'll discuss this later, but for now just try to rewrite the last `do` notation using the explicitly named functions and see what happens.

## Discussion

## Exercises

1. Rewrite `evalM` and `typeofM` to use the `Maybe` monad rather than the `Either` monad.  You will not be able to return error messages using `Maybe`.
