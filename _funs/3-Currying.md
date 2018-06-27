---
layout: frontpage
title: Currying
use_math: true
categories: chapter ch2
---

# Currying

Functions in all our languages thus far take only a single parameter. What that may seem limiting, it's actually not. _Currying_ or _curried functions_ is a way of constructing multi-parameter functions using only single parameter functions.  If you're a Haskell programmer, currying is something you do all the time.  Look at the signature of an operation like `plus3` that adds three `Int`s as an example:

```text
plus3 :: Int -> Int -> Int -> Int
```

Usually we think about such a function as a mapping from three `Int` arguments to and `Int` result because of our upbrining in traditional languages.  However, what it literally defines is a function from the single parameter `Int` to another function of type `[Int] -> [Int] -> [Int]`.  Applying `plus3` to a single argument results in a function:

```haskell
(plus3 1) :: Int -> Int -> Int
```

Parenthesis added to the left side for emphasis.  Applying another parameter results in:

```text
((plus3 1) 2) :: Int -> Int
```

and another:

```text
(((plus3 1) 2) 3) :: Int
```

and we now have a final result.  But the function resulting from `plus3 1` is just as much a result as `6`.  This is something we do all the time in Haskell to define and use partially instantiated functions:

```text
plus2 = plus3 0
```

In `FBAE` and subsequent languages currying works the same way.  We simply use `app` to achieve application of a function to an argument. Given that we had defined `plus3` in `FBAE`, the previous function application would look like this:

```text
(app (app (app plus3 1) 2) 3)
```

It's a bit messier, but it works just the same an emphasizes the currying that's happening.

We can easily write `plus3` using `bind` and nested `lambda`s.  Let's walk through it starting with the `bind` of `plus3` to a `lambda` with one argument, 'x':

```text
bind plus3 = lambda x in ?? in
  ...
```

Now we need to figure out what to replace `??` with.  Literally, we want to add `x` to the result of adding two more arguments.  Let's write that much:

```text
bind plus3 = (lambda x in x + ??) in
  ...
```

Now we need to get the next argument.  The only way to do this is to use another nested `lambda`.  Remember, that `plus3 1` should return a function.  What should immediately follow `in` should be a function and that function needs to know about `x`:

```text
bind plus3 = (lambda x in (lambda y in x + y + ??)) in
  ...
```

See where we're going?  If we evaluate `plus3 1` what we'll get is `lambda y in x + y + ??` waiting for the `y` parameter.  Another `lambda` will pick up the third integer:

```text
bind plus3 = (lambda x in (lambda y in (lambda z in x + y + z))) in
  ...
```

Just what we want.  The `bind` now just needs a body where `plus3` is used.  Let's copy what we did earlier:

```text
bind plus3 to (lambda x in (lambda y in (lambda z in x + y + z))) in
  (app (app (app plus3 1) 2) 3)
```

Now we can walk trough the evaluation of the `bind` body tracking the environment along the way:

```text
(app (app (app plus3 1) 2) 3)
== (app (app (app (lambda x in (lambda y in (lambda z in x + y + z))) 1) 2) 3) []
== (app (app (lambda y in (lambda z in x + y + z))) 2) 3) [(x,1)]
== (app (lambda z in x + y + z))) 3) [(x,1),(y,2)]
== x + y + z [(x,1),(y,2),(z,3)]
...
== 6
```

We could add additional syntax to make both application and definition of n-ary functions simpler.  We'll leave a bit of that to an exercise.

Curried function semantics is used frequently to define multi-parameter function execution.  Haskell certainly uses this approach as does ML.  Scheme and Lisp on the other hand require explicit currying.  We'll not follow that path right now.

## Definitions

## Exercises
