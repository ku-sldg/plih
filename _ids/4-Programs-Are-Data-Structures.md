---
layout: frontpage
title: Programs Are Data Structures
use_math: true
categories: chapter ch4
---
# Programs Are Data Structures
We've seen repeatedly that the result of parsing an expression is an AST.  An interpreter is a program that consumes that data structure and produces a value.  A type checker is a program that consumes that data structure and produces a type.  Both are programs that manipulate AST structures and produce different kinds of results.  Let's take this concept further and look at another kind of program that manipulates programs.

## Optimization

Most if not all modern compilers and interpreters perform some kind of optimization on programs they process.  Such optimizations range from simple function inlining and elimination of constant calculations to sophisticated variable elimination and loop unrolling.

This simple example will take a BBAE program and perform zero elimination on numerical calculations.  Specifically, both plus and minus operations have cases involving `0` where a term can be eliminated:

{% highlight text %}
x + 0 == x
0 + x == x
x - 0 == x
{% endhighlight %}

Let's define an optimization that replaces each expression on the left with the corresponding expression on the right.

## Implementation

Implementation of the optimization involves writing an interpreter.

The signature of the optimizer accepts a BBAE and produces a BBAE:

{% highlight haskell %}
optimize :: BBAE -> BBAE
{% endhighlight %}

The constant number vale cannot be optimized and is thus left alone:

{% highlight haskell %}
optimize (Num x) = (Num x)
{% endhighlight %}

We add two cases corresponding to addition of `0` to any value.  In both cases the sum is eliminated and replaced with the non-zero term.  In each case the resulting term is recursively optimized.  The third case involving sums with no zero-valued terms remains the same following optimization:

{% highlight haskell %}
optimize (Plus (Num 0) r) = optimize r
optimize (Plus l (Num 0)) = optimize l
optimize (Plus l r) = let l' = (optimize l)
                          r' = (optimize r)
                      in (Plus l' r')
{% endhighlight %}

Optimizing subtraction works similarly.  However, we will only eliminate zero in the righthand term:

{% highlight haskell %}
optimize (Minus l (Num 0)) = optimize l
optimize (Minus l r) = let l' = (optimize l)
                           r' = (optimize r)
                       in (Minus l' r')
{% endhighlight %}

Optimization of all non-numeric terms is achieved by optimizing terms and reassembling the term.  For example, `if` is optimized by optimizing each of the three terms in the AST structure and reassembling them in a new `If` construct.  Following are cases for remaining BBAE constructs:

{% highlight haskell %}
optimize (Bind i v b) = let v' = optimize v in
                          optimize b
optimize (Id id) = (Id id)
optimize (Boolean b) = (Boolean b)

optimize (If c t e) = (If (optimize c) (optimize t) (optimize e))
{% endhighlight %}

An interpreter tht composes the optimization function with the other language parsing elements is expressed as:

{% highlight haskell %}
interp = eval [] . optimize . parseBBAE
{% endhighlight %}

## Testing

Optimizations must be extensively tested and verified.  We can use roughly the same QuickCheck function to ensure correctness that we used earlier:

{% highlight haskell %}
testOptimizedEval :: Int -> IO ()
testOptimizedEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> case typeof [] t of
           (Right _) -> ((eval []) . optimize) t == (eval [] t)
           (Left _) -> True)
{% endhighlight %}

Here we compare the result evaluating directly, `eval []`, with the result of optimizing before evaluating, `eval [] . optimize`.  Testing 10000 cases gives evidence that the optimization is correct.

## Discussion
Eliminating math operations over 0 is a trivial optimization.  However, it serves to demonstrate how an AST is just a data structure.  We wrote an operation over `BBAE` that simply transforms one program into another.  Both `eval` and `typeof` do the same thing, but are more traditionally associated with processing programs.

Also note the structure of the `optimize` function.  It is essentially the same as the interpreter and type checker. Recursive function over the structure of an expression share a common for that we will see over and over again throughout this study.  Perhaps we can abstract that pattern out of these functions and use it directly.

## Exercises
1. Write an optimizer for the Boolean operation `and` similar to that for numerical operations dealing with constants `true` and `false`.
2. Write an optimizer for the `If` expression that deals with constant `true` and `false` values in the conditional.
3. Define an optimization for `Leq` and write an optimizer for it.  Show using QuickCheck that your optimization is correct.