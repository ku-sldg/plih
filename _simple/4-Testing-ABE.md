---
layout: frontpage
title: Testing ABE
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
