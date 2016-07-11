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

# Testing

Haskell provides [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck.html) a domain-specific language widely used for testing and adapted to many other languages.  Like Parsec, QuickCheck takes a bit of time to learn, but is a worthwhile tool to have available.

What QuickCheck does is rather ingenious.  Let's assume that we've written a function and would like to test that a property holds for a set of arbitrary inputs to that function.  As a concrete example, let's define a function `f x = x * x` and a property `p v = v >= 0`.  `f` is the *function under test* and `p` is the *property being tested*.  Note the signatures of `f` and `p`:

{% highlight haskell %}
f::Int -> Int
f x = (x * x)

p::Int -> Bool
p v = v >= 0
{% endhighlight %}

If we want to say that `p` holds for all `f`, we would prove the following property:

$$\forall x:Int \cdot (p (f x))$$

This tells us that for every integer $x$, $g$ holds for $f x$.  (This is actually an easy proof, but for discussion let's assume we  have only testing available to us.)

To test `p` we can easily write a function:

{% highlight haskell %}
\x -> (p (f x))
{% endhighlight %}

and call that function with numerous random values of `x`.  The more  tests run, the more confident we are in the result.

This is precisely what QuickCheck does.  Specifically:

{% highlight haskell %}
quickCheck (\x -> (p (f x)))
{% endhighlight %}

will generate 100 arbitrary `Int` values and call `(\x -> (p (f x)))` on each.  In this case, the square of every integer is greater than or equal to zero, and we get the result:

{% highlight haskell %}
+++ OK, passed 100 tests.
{% endhighlight %}

Choosing a test that is false for some integers:

{% highlight haskell %}
p' v = (v /= 100)
{% endhighlight %}

causes QuickCheck to display a counterexample:

{% highlight haskell %}
quickCheck (\x-> (p' (f x)))
*** Failed! Falsifiable (after 18 tests and 1 shrink): 
10
{% endhighlight %}

In this case, 100 random tests were enough to discover that 100 is the square of 10 and the test condition is thus false.

### Arbitrary and Generators

Part of the magic of QuickCheck is the ability to generate random test cases.  In this example, QuickCheck must generate random `Int` values.  Because `Int` is a commonly used type, the ability to generate random `Int` values is built-in to QuickCheck.  In contrast `AE` is a custom, user-defined type that is not built-in.

QuickCheck requires that the domain of any function it is called on be an instance of class `Arbitrary` providing a function `arbitrary` that generates an arbitrary value of that type.  We must make  `AE` and instance of `Arbitrary`:

{% highlight haskell %}
instance Arbitrary AE where
  arbitrary = ...
{% endhighlight %}

The type signature of `arbitrary` tells is what direction we need to follow:

{% highlight haskell %}
arbitrary :: Arbitrary a => Gen a
{% endhighlight %}

The `arbitrary` operator is of type `Gen a` where `a` is also an instance of `Arbitrary`.  `Gen` is a *generator*.  

Let's start by generating an arbitrary `Num`.  The `genNum` function does this by returning an arbitrary number:

{% highlight haskell %}
genNumFirst =
  do t <- arbitrary
     return (Num t)
{% endhighlight %}

`genNumFirst` calls `arbitrary`, stores the result in `t`, and returns `(Num t)`.  Now we see `arbitrary` at work.  The `return` statement constrains `t` to be of type `Int`.  As noted, `Int` is an instance of `Arbitrary` and has an associated `arbitrary` function.  `genNumFirst` thus generates arbitrary values of `Num` by lifting arbitrary `Int` values.

Suppose we don't want to look at all `Int` values, but only a range.  The built in `choose` operation does exactly this by restricting the range of `arbitrary`.  `genNum` uses `choose` to generate arbitrary `Num` values over a subrange of `Int`:

{% highlight haskell %}
genNum =
  do t <- choose (0,100)
     return (Num t)
{% endhighlight %}

We could stop now and define `AE` to be an instance of `Arbitrary` as follows:

{% highlight haskell %}
instance Arbitrary AE where
  arbitrary = genNum
{% endhighlight %}

Of course our arbitrary values of `AE` are not so arbitrary as they will only include number values ranging from 0 to 100.  We need some terms that involve `Plus` and `Minus`.

Let's start with `Plus` and think about how we might generate arbitrary terms.  The simplest thing would be to generate `Plus` terms over arbitrary numbers:

{% highlight haskell %}
genPlusFirst =
  do t1 <- genNum
     t2 <- genNum
     return (Plus t1 t2)
{% endhighlight %}

This generator gets us part way there, but only generates terms of the form `(Plus v1 v2)` where `v1` and `v2` are arbitrary `Num` values.  A term of the form `(Plus (Plus (Num 1) (Num 2)) (Num 3))` would never be generated.  What we need is to generate terms recursively in the `Plus` generator.  Unfortunately we don't have a generator for terms yet, so we'll have to set this aside and just use `genPlusFirst`.

`Minus` is identical to `Plus` modulo the constructor name, so we'll defer that generator as well assuming that we have `genMinusFirst` available.

Now let's put our generators together to generate a more complete set of terms using `genABE`.  The simplest way to compose generators for our individual terms is to use the `oneof` operator that chooses one generator arbitrarily from a list of generators:

{% highlight haskell %}
genABEFirst = oneof [genNum,genPlus,genMinus]
{% endhighlight %}

If we use `genABEFirst` we arbitrarily generate a `Num`, `Plus` or `Minus`.  Getting closer, but not quite there.  Remember that `genPlusFirst` and `genMinusFirst` only build terms from `Num`.  With a generator for `ABE` we can fix that now by using `genABEFirst` where we used `genNum`:

{% highlight haskell %}
genPlusSecond =
  do s <- genAEFirst
     t <- genAEFirst
     return (Plus s t)
{% endhighlight %}

If we do the same thing with `genMinusSecond` we will now generate arbitrary `ABE` terms.  Unfortunately, those arbitrary `ABE` terms are  of *arbitrary size*.  Mathematically this is perfectly fine as really big things don't bother us.  Pragmatically this is not perfectly fine as `genABEFirst` can now generate arbitrarily huge test cases.  If you want to see what this does, use `sample` to generate a few test cases and be prepared to terminate your Haskell process!

How do we make our generator for `AE` not go into the weeds generating huge test cases?  The easiest way is to add a size limit to the `AE` generator function by adding a size parameter to `genAE` that is decremented on each call to `genAE`.

Let's start with `genAE` by adding a size parameter, `n` and a base case when `n` hits zero:

{% highlight haskell %}
genAE :: Int -> Gen AE
genAE 0 =
  do term <- genNum
     return term
genAE n =
  do term <- oneof [genNum,(genPlus (n-1)),(genMinus (n-1))]
     return term
{% endhighlight %}

This new function takes a size counter and produces a generator.  If the size counter is zero, then the generator will only produce a number and will not recurse.  If the size counter is not zero, then the generator will produce an arbitrary while decrementing the size counter.  `genPlus` and `genMinus` must be similarly extended to include the size counter:

{% highlight haskell %}
genPlus n =
  do s <- genAE n
     t <- genAE n
     return (Plus s t)
{% endhighlight %}

The counter is simply passed to `genAE` where it will be decremented in any mutually recursive call to `genPlus` or `genMinus`.  Note that it is always an option to call `genNum` when the counter is not zero.  This allows for any term whose size is less than the counter, not just equal to it.

Now we make `AE` and instance of `Arbitrary` by using `genAE` to define the `arbitrary` function:

{% highlight haskell %}
instance Arbitrary AE where
  arbitrary =
    sized $ \n -> genAE (rem n 10)
{% endhighlight %}

What's going on here is obfuscated by underlying construction of generators, but let's see if we can make sense of it. `sized` is a function that accepts a argument of type `Int -> Gen a`.  `sized` then chooses an arbitrary positive integer and calls its argument on that value to produce a generator.  In effect, the parameter `n` is an arbitrary value that we can use to produce a generator.  We could just us `n` as an argument to `genAE`.  What I've done is use the `rem` function to convert `n` into an arbitrary value less than 10.  This is a bit of overkill, but demonstrates how we can use `sized` to arbitrarily size a structure.  Another alternative would define `arbitrary` this way:

{% highlight haskell %}
arbitrary = sized $ \n -> genAE (rem n 10) + 10
{% endhighlight %}

In this case the arbitrary values would have a maximum size ranging from 10 to 19.  There are all kinds of games one can play to generate interesting arbitrary cases.

Before we go one, here's all the code for our generators and making `AE` and instance of `Abitrary` in one place:

{% highlight haskell %}
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
{% endhighlight %}

An idiom is emerging even in this simple interpreter that merits attention.  We defined our parser, interpreter, generator, and pretty printer the same way.  Define the function for each individual case, then put the cases together to define the overall function.  Remember this.  It will serve you well.

### QuickCheck

Now that we can generate arbitrary `AE` values, we can start our testing.  Like many things in Haskell, once the preparatory work is done, using what we built is rather simple.

Before we can run QuickCheck, we need to determine what it is we're going to test for.  For a traditional test case, we provide an input and an expected output.  QuickCheck performs *random testing* meaning that we need to express correctness in terms of an unknown test case.  We can't simply find `eval n` and compare it to a known value.

Let's first look at our parser for some ideas.  If the parser and pretty printer are working correctly, pretty printing an `AE` structure and parsing the result, should give us the same `AE` structure.  Formally:

$$\forall t:AE \cdot (parseAE\; (pprint\; t)) == t$$

To turn this into an input for QuickCheck we simply remove the universal quantifier and pass in a value for $t$.  The Haskell representation is:

{% highlight haskell %}
\t -> parseAE (pprint t) == t
{% endhighlight %}

Calling QuickCheck on this function generates 100 arbitrary `AE` values and test each one:

{% highlight haskell %}
quickCheck (\t -> parseAE (pprint t) == t)
+++ OK, passed 100 tests.
{% endhighlight %}

We can play with parameters to QuickCheck that adjust the number of test cases to some larger or smaller set of test cases:

{% highlight haskell %}
quickCheckWith stdArgs {maxSuccess=500} (\t -> parseAE (pprint t) == t)
+++ OK, passed 500 tests.
{% endhighlight %}

and define a utility function that can be used during development quickly run a set of tests on a parser instance:

{% highlight haskell %}
testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseAE (pprint t) == t)
{% endhighlight %}

This final function is included in the source for `AE` as an example test function.  Note the type of `testParser` taking an `Int` and generating something that is printed.

Lets move forward and test the evaluator in a similar way.  Again, what to test?  There's not much to work with here, so let's test another property that is interesting, but does not represent correctness:

{% highlight haskell %}
\t -> interp . pprint t == eval t
{% endhighlight %}

What this property says is evaluating an `AE` structure gives us the same thing as pretty printing and then interpreting the same structure.  Let's embed that property in a test function for the evaluator:

{% highlight haskell %}
testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval t))
{% endhighlight %}

Not much of a test, but if any of our components crashed QuickCheck will help find those cases.