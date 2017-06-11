---
layout: frontpage
title: Testing
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
$$

# Testing AE

Haskell provides [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck.html) a domain-specific language widely used for testing and adapted to many other languages.  Like Parsec, QuickCheck takes a bit of time to learn, but is a worthwhile tool to have available.

What QuickCheck does is rather ingenious.  Let's assume that we've written a function and would like to test that a property holds for a set of arbitrary inputs to that function.  As a concrete example, let's define a function `f x = x * x` and a property `p v = v >= 0`.  `f` is the *function under test* and `p` is the *property being tested*.  Note the signatures of `f` and `p`:

```haskell
f::Int -> Int
f x = (x * x)

p::Int -> Bool
p v = v >= 0
```

If we want to say that `p` holds for all `f`, we would prove the following property:

$$\forall x:Int \cdot (p (f x))$$

This tells us that for every integer $x$, $p$ holds for $f x$.  (This is actually an easy proof, but for discussion let's assume we  have only testing available to us.)

To test `p` we can easily write a function:

```haskell
\x -> (p (f x))
```

and call that function with numerous random values of `x`.  The more  tests run, the more confident we are in the result.

This is precisely what QuickCheck does.  Specifically:

```haskell
quickCheck (\x -> (p (f x)))
```

will generate 100 arbitrary `Int` values and call `(\x -> (p (f x)))` on each.  In this case, the square of every integer is greater than or equal to zero, and we get the result:

```haskell
+++ OK, passed 100 tests.
```

Choosing a test that is false for some integers:

```haskell
p' v = (v /= 100)
```

causes QuickCheck to display a counterexample:

```haskell
quickCheck (\x-> (p' (f x)))
*** Failed! Falsifiable (after 18 tests and 1 shrink):
10
```

In this case, 100 random tests were enough to discover that 100 is the square of 10 and the test condition is thus false.

### Arbitrary and Generators

Part of the magic of QuickCheck is the ability to generate random test cases.  In this example, QuickCheck must generate random `Int` values.  Because `Int` is a commonly used type, the ability to generate random `Int` values is built-in to QuickCheck.  In contrast `AE` is a custom, user-defined type that is not built-in.

QuickCheck requires that the domain of any function it is called on be an instance of class `Arbitrary` providing a function `arbitrary` that generates an arbitrary value of that type.  We must make  `AE` and instance of `Arbitrary`:

```haskell
instance Arbitrary AE where
  arbitrary = ...
```

The type signature of `arbitrary` tells us what direction we need to follow:

```haskell
arbitrary :: Arbitrary a => Gen a
```

The `arbitrary` operator is of type `Gen a` where `a` is also an instance of `Arbitrary`.  `Gen` is a *generator*.

Let's start by generating an arbitrary `Num`.  The `genNumFirst` function does this by returning an arbitrary number:

```haskell
genNumFirst =
  do t <- arbitrary
     return (Num t)
```

There is quite a bit going on here.  First note that `Arbitrary` is a monad allowing use of the `do` notation to bind identifiers and sequence generators.  Using `do`, `genNumFirst` calls `arbitrary`, binds the result to `t`, and then returns `(Num t)`.  Now we see `arbitrary` at work.  The `Num` constructor in the `return` statement constrains `t` to be of type `Int`.  This is how the correct `arbitrary` instance is identified for `t`. `Int` is an instance of `Arbitrary` and its associated `arbitrary` function is used to generate a value for `t`.  `genNumFirst` generates arbitrary `Num` values ofby lifting arbitrary `Int` values into our AST.

Suppose we don't want to look at all `Int` values, but only a range.  The built in `choose` operation does exactly this by restricting the range of `arbitrary`.  `genNum` uses `choose` to generate arbitrary `Num` values over a subrange of `Int`:

```haskell
genNum =
  do t <- choose (0,100)
     return (Num t)
```

We could stop now and define `AE` to be an instance of `Arbitrary` as follows:

```haskell
instance Arbitrary AE where
  arbitrary = genNum
```

Of course our arbitrary values of `AE` are not so arbitrary as they will only include number values ranging from 0 to 100.  We need some terms that involve `Plus` and `Minus` if our intent is testing an interpreter.

Let's start with `Plus` and think about how we might generate arbitrary terms.  The simplest approach is to generate `Plus` terms over arbitrary numbers:

```haskell
genPlusFirst =
  do t1 <- genNum
     t2 <- genNum
     return (Plus t1 t2)
```

This generator gets us part way there, but only generates terms of the form `(Plus (Num v1) (Num v2))` where `v1` and `v2` are arbitrary `Int` values.  A term of the form `(Plus (Plus (Num 1) (Num 2)) (Num 3))` would never be generated.  What we need is to generate terms recursively in the `Plus` generator.  Unfortunately we don't have a generator for terms yet, so we'll have to set this aside and just use `genPlusFirst`.

`Minus` is identical to `Plus` modulo the constructor name, so we'll defer that generator as well and assume we have `genMinusFirst` available.

Now let's put our generators together to generate a more complete set of terms using `genAE`.  The simplest way to compose generators for our individual terms is to use the `oneof` operator that chooses one generator arbitrarily from a list of generators:

```haskell
genAEFirst = oneof [genNum,genPlus,genMinus]
```

If we use `genAEFirst` we arbitrarily generate a `Num`, `Plus` or `Minus`.  Getting closer, but not quite there.  Remember that `genPlusFirst` and `genMinusFirst` only build terms from `Num`.  With a generator for `AE` we can fix that now by using `genAEFirst` where we used `genNum`:

```haskell
genPlusSecond =
  do s <- genAEFirst
     t <- genAEFirst
     return (Plus s t)
```

If we do the same thing with `genMinusSecond` we will now generate arbitrary `AE` terms.  Unfortunately, those arbitrary `AE` terms are of *arbitrary size*.  Mathematically this is perfectly fine as really big things don't bother us in the symbolic world.  Pragmatically this is not perfectly fine as `genAEFirst` can now generate arbitrarily huge test cases.  If you want to see what this does, use `sample` to generate a few test cases and be prepared to terminate your Haskell process!

How do we make our generator for `AE` not go into the weeds generating huge test cases?  The easiest way is to add a size limit to the `AE` generator function by adding a size parameter to `genAE` that is decremented on each call to `genAE`.

Let's start with `genAE` by adding a size parameter, `n` and a base case when `n` hits zero:

```haskell
genAE :: Int -> Gen AE
genAE 0 =
  do term <- genNum
     return term
genAE n =
  do term <- oneof [genNum,(genPlus (n-1)),(genMinus (n-1))]
     return term
```

This new function takes a size counter and produces a generator.  If
the size counter is zero, then the generator will only produce a
number and will not recurse.  If the size counter is not zero, then
the generator will produce an arbitrary (TODO: term?) while decrementing the size counter.  `genPlus` and `genMinus` must be similarly extended to include the size counter:

```haskell
genPlus n =
  do s <- genAE n
     t <- genAE n
     return (Plus s t)
```

The counter is simply passed to `genAE` where it will be decremented in any mutually recursive call to `genPlus` or `genMinus`.  Note that it is always an option to call `genNum` when the counter is not zero.  This allows for any term whose size is less than the counter, not just equal to it.

Now we make `AE` and instance of `Arbitrary` by using `genAE` to define the `arbitrary` function:

```haskell
instance Arbitrary AE where
  arbitrary =
    sized $ \n -> genAE (rem n 10)
```

What's going on here is obfuscated by underlying construction of generators, but let's see if we can make sense of it. `sized` is a function that accepts an argument of type `Int -> Gen a`.  `sized` then chooses an arbitrary positive integer and calls its argument on that value to produce a generator.  In effect, the parameter `n` is an arbitrary value that we can use to produce a generator.  We could just use `n` as an argument to `genAE`.  What I've done is use the `rem` function to convert `n` into an arbitrary value less than 10.  This is a bit of overkill, but demonstrates how we can use `sized` to arbitrarily size a structure.  Another alternative would define `arbitrary` this way:

```haskell
arbitrary = sized $ \n -> genAE (rem n 10) + 10
```

In this case the arbitrary values would have a maximum size ranging from 10 to 19.  There are all kinds of games one can play to generate interesting arbitrary cases.

Before we go on, here's all the code for our generators and making `AE` and instance of `Abitrary` in one place:

```haskell
instance Arbitrary AE where
  arbitrary =
    sized $ \n -> genAE (rem n 10)

genNum =
  do t <- choose (0,100)
     return (Num t)

genPlus n =
  do s <- genAE n
     t <- genAE n
     return (Plus s t)

genMinus n =
  do s <- genAE n
     t <- genAE n
     return (Minus s t)

genAE :: Int -> Gen AE
genAE 0 =
  do term <- genNum
     return term
genAE n =
  do term <- oneof [genNum,(genPlus (n-1)),(genMinus (n-1))]
     return term
```

An idiom is emerging even in this simple interpreter that merits attention.  We defined our parser, interpreter, generator, and pretty printer the same way.  Define the function for each individual case, then put the cases together to define the overall function.  Remember this.  It will serve you well.

### QuickCheck

Now that we can generate arbitrary `AE` values, we can start our testing.  Like many things in Haskell, once the preparatory work is done, using what we built is rather simple.

Before we can run QuickCheck, we need to determine what it is we're going to test for.  For a traditional test case, we provide an input and an expected output.  QuickCheck performs *random testing* meaning that we need to express correctness in terms of an unknown test case.  We can't simply find `eval n` and compare it to a known value.

Let's first look at our parser for some ideas.  If the parser and pretty printer are working correctly, pretty printing an `AE` structure and parsing the result, should give us the same `AE` structure.  Formally:

$$\forall t:AE \cdot (parseAE\; (pprint\; t)) == t$$

To turn this into an input for QuickCheck we simply remove the universal quantifier and pass in a value for $t$.  The Haskell representation is:

```haskell
\t -> parseAE (pprint t) == t
```

Calling QuickCheck on this function generates 100 arbitrary `AE` values and test each one:

```haskell
quickCheck (\t -> parseAE (pprint t) == t)
+++ OK, passed 100 tests.
```

We can play with parameters to QuickCheck that adjust the number of test cases to some larger or smaller set of test cases:

```haskell
quickCheckWith stdArgs {maxSuccess=500} (\t -> parseAE (pprint t) == t)
+++ OK, passed 500 tests.
```

and define a utility function that can be used during development quickly run a set of tests on a parser instance:

```haskell
testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseAE (pprint t) == t)
```

This final function is included in the source for `AE` as an example test function.  Note the type of `testParser` taking an `Int` and generating something that is printed.

Lets move forward and test the evaluator in a similar way.  Again, what to test?  There's not much to work with here, so let's test another property that is interesting, but does not represent correctness:

```haskell
\t -> interp . pprint t == eval t
```

What this property says is evaluating an `AE` structure gives us the same thing as pretty printing and then interpreting the same structure.  Let's embed that property in a test function for the evaluator:

```haskell
testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval t))
```

Not much of a test, but if any of our components crashed QuickCheck will help find those cases.

## Testing ABE

Let's fire up QuickCheck and see what we have.  Remember that all `AE` programs famously ran to termination and produced a value.  Will the same thing hold true for `ABE` programs?  I suspect you can easily figure the answer, but let's follow a rigorous process to see why.  The process will be useful to us when our languages become more complex.

### Term Generator

We need to extend the generator from the `Testing` chapter to generate `ABE` elements.  Like our parser we simply need to define generators for the new `AST` elements and add them to the original generator.

Generating `Boolean` values is identical to generating `Num` values except we use `choose` to select among `True` and `False`:

```haskell
genBool =
  do t <- choose (True,False)
     return (Boolean t)
```

Generators for the remaining operators are identical to what we did for `Plus` and `Minus`.  Specifically, generate the arguments and put them together:

```haskell
genAnd n =
  do s <- genABE n
     t <- genABE n
     return (And s t)

genLeq n =
  do s <- genABE n
     t <- genABE n
     return (Leq s t)

genIsZero n =
  do s <- genABE n
     return (IsZero s)

genIf n =
  do s <- genABE n
     t <- genABE n
     u <- genABE n
     return (If s t u)
```

Remember that the argument `n` is our size counter that will force the generator to terminate when the new `ABE` structure reaches a specified size.

Now `genABE` for generating complete terms:

```haskell
genABE :: Int -> Gen ABE
genABE 0 =
  do term <- oneof [genNum,genBool]
     return term
genABE n =
  do term <- oneof [genNum,(genPlus (n-1))
                   ,(genMinus (n-1))
                   ,(genAnd (n-1))
                   ,(genLeq (n-1))
                   ,(genIsZero (n-1))
                   ,(genIf (n-1))]
     return term
```

Note what's going on here.  For the base case, we use `oneof` to generate either a number or a boolean.  For the recursive case, we simply add generators for new terms to the argment to `oneof` adding them to the `ABE` generator.  That's it.  We're done.

### QuickCheck

Our testing functions are identical to those for `AE` with `AE` changed to `ABE`.  Literally, that's all there is to it:

```haskell
testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseABE (pprint t) == t)

testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval t))
```

Now we're ready to go.

### Initial Tests

Using the `testParser` function we'll first test 1000 random `ABE` terms:

```haskell
testParser 1000
+++ OK, passed 1000 tests.
```

So far, so good. The `ABE` parser passes 1000 tests of arbitrary structures.

Using the `testEval` function we'll now test 1000 random `ABE` terms:

```haskell
 testEval 1000
*** Failed! (after 2 tests):
Exception:
  /Users/alex/Documents/Research/books/plih/sources/haskell/abe.hs:123:23-40: Irrefutable pattern failed for pattern (Main.Num v)
IsZero (Boolean False)
```

This result tells us that the `eval` function fails after 2 tests.  Specifically, the term `IsZero (Boolean False)` cases `eval` to fail without generating a value. It should be clear why. `IsZero` expects a number yet it is called on a Boolean value.  `IsZero (Boolean False)` is a *counterexample* for the test and gives us a direction to follow.[^1]
