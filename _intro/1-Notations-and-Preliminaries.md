---
layout: frontpage
title: Notations and Preliminaries
use_math: true
categories: chapter ch0
---
$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeof}\;}
\newcommand\interp{\mathsf{interp}\;}
\newcommand\eval{\mathsf{calc}\;}
\newcommand\NUM{\mathsf{NUM}\;}
\newcommand\NAT{\mathsf{NAT}\;}
$$

# Haskell

This is _not_ a Haskell text.  I will cover some things in Haskell you might not be aware of, but I make no claims about writing big, beautiful Haskell.  Monads, GADTs, and point-free style come to mind quickly as important concepts that we’ll use.  However, don't read this book to learn Haskell.  There are much better sources both online and as traditional books:

* [Learn you a Haskell for Great Good][2]
* [Haskell Book][3]

## Data Types and Recursion

Understanding Haskell data types and recursion is critical to understanding how interpreters work.  Recursion is your friend and will serve you well.  When we define a simple data type like this example for lists:

{% highlight haskell %}
data IntList where
  Null :: IntList
  Cons :: Int -> IntList -> IntList 
{% endhighlight %}

we also a recursion framework for processing instances of the type.  If we want to write a `size` function for `IntList` we can simply define `size` over every `IntList` constructor:

{% highlight haskell %}
size Null = 0
size Cons x xs = size xs + 1
{% endhighlight %}

`Null` is the base case and `Cons` the recursive case for this definition.  The structure of `IntList` and any type created with `data` is such that a recursive function can be written using this pattern to process any such structure.  This is a powerful principle we will use repeatedly throughout this text.

## The do Notation

Haskell's `do` notation is a commonly used abstraction allowing sequential execution of monadic code.  If you are at all familiar with Haskell, you've likely seen this notation at work.  I suspect you've also tried hard to avoid the details of monadic computation and are reconsidering reading this text now that the monad has entered the conversation.

Fear not.  We will use the monadic `do` notation to order computations in parsers and test procedures.  Early in the text we won't need to understand the details of monadic computation.  We'll ease into monads and applicative functors later on after we have a better handle on how interpreters are written.

For now, here's what you need to know.  In the following `do` example, three operations are performed in sequence. `f x` is called first and the result bound to identifier `a`.  `g x` is called next, and again the result bound to identifier `b`.  Finally, `a + b` is calculated and returned as the result of the computation:

{% highlight haskell %}
do {
	a <- f x;
	b <- g y;
	return (a + b)
}
{% endhighlight %}

The trick is that `f` and `g` must be monadic.  You can't do this with arbitrary functions.  The thing you get back is also monadic and not a simple value.  The benefit is the same `do` notation works for _any_ monad.  Just choose a monad to encapsulate your computations and you get the `do` notation for free.

Let's see how this works with the `Maybe` monad with a very simple example.  Define two functions that return `Maybe` types:

{% highlight haskell %}
f x = if x > 3 then Just x else Nothing
g x = if x < 4 then Nothing else Just x
{% endhighlight %}

These two functions are rather arbitrary, but both take integers and return `Maybe Int` based on their input with respect to a constant value.  There is nothing to see here other than both return `Maybe` types.

Now a simple function `test` that uses `do` to sequence calculations:

{% highlight haskell %}
test1 = do {
  a <- f 4 ;
  b <- g a ;
  return (a + b)
  }
test1
== Just 8
{% endhighlight %}

You can see the sequential calculation.  `a` is calculated first and  `b` calculated from `a`.  Both `a` and `b` determine the return value.  Simple sequential execution.

But there's more!

If either the `a` or `b` calculation results in `Nothing`, the sequence of calculations returns `Nothing`:

{% highlight haskell %}
test2 = do {
  a <- f 2 ;
  b <- g a ;
  return (a + b)
  }
test2
== Nothing
{% endhighlight %}

All the handing of `Nothing` normally done with `case` statements is handled underneath the hood by the bind.  No need to check things in the calculation of `b` or the function `g`.

Finally, all functions are written over `Int` and not `Maybe`.  `g`'s argument is an `Int` and `+` takes two `Int`s.  The `do` notation handles `Just` and in the `<-` sequence.  `f` returns `Just 4` in the first example, but `a` gets bound to `4`.  This is a wonderfully powerful feature that we'll discuss later.  For now, these examples show how the `do` notation sequences calculations and binds values to identifiers - a topic we'll dive into shortly.

One plea before moving on.  Don't avoid monads.  They are truly beautiful computation structures that support a powerful, new abstraction for computing.  Spend some time with them and write your own.  The experience will serve you well!

# Notations

Several standard notations are used throughout for various kinds of things we need to discuss.  Mathematics, Haskell and Expressions are used to represent formal definitions, implementation code, and source language code respecitively.  We also need a meta-language for describing our languages and their meanings.

## Math

Throughout this document mathematical statements are formatted using embedded $\LaTeX$ using Mathjax:  

$$\{s:string\mid P(s)\}$$

## Haskell

Similarly, Haskell code is formatted using the traditional Haskell style:

{% highlight haskell %}
data AE = Num Int
	    | Plus AE AE
	    | Minus AE AE
	      deriving (Show,Eq)
{% endhighlight %}

## Expressions

Concrete syntax for the various languages we develop interpreters for is formatted much like Haskell, but without syntax highlighting:

{% highlight text %}
bind f = lambda x in x + 1 in
  app f 3
{% endhighlight %}


## Meta Language

A meta-language is exactly what the name implies - the language above the language.  It is used to define the various aspects of the languages we will study.

### Grammars

Grammars are represented using `::=` to define variables and `|` to express alternative.  Any symbol in all caps is considered a variable.  The following trivial grammar defines an expression languages consisting of sum and difference operations:

$$
\begin{align*}
t ::=\; & \NUM \\
	  & \mid t + t \\
	  & \mid t - t \\
	  & \mid if\; t\; then\; t\; else\; t \\
\end{align*}
$$

Any symbol to the left of the `::=` definition symbol is a meta-variable defined by the definition to the right.  The symbol $t$ represents anything defined by the expression to the right.  Note also that any subscripted or superscripted $t$ such as $t\_k$ or $t'$ is definitionally the same as $t$.

There are a few pre-defined meta-variables that include $\NUM$ for integer numbers and $\NAT$ for natural numbers.  We're not worried about writing parsers in this book, but specifying them proves useful.

### Inference Rules

Inference rules define immediate consequences like the definition of $\wedge$-introduction:

$$\frac{A,B}{A \wedge B}$$

This says that if we know $A$ and we know $B$, then we immediately know $A \wedge B$.  More generally:

$$\frac{A_0,A_1,\ldots,A_n}{C}$$

says if $A_0$ through $A_n$ are known to hold, then $C$ is known to hold.  $A_k$ are called _antecedents_ and $C$ is called a _consequent_.

An inference rule having no preconditions defines an _axiom_:

$$\frac{}{A}$$

If nothing need be true to know $A$, then $A$ is always true.

### Derivations

A derivation strings inference rules together by using the consequent of one or more rules as the antecedent of another.  Consider the derivation of the symmetric property of conjunction can be derived as follows:

$$\frac{\frac{A\wedge B}{B}\;\frac{A\wedge B}{A}}{B\wedge A}$$

1. $A \wedge B \vdash B$ **by** $\wedge$-elim
2. $A \wedge B \vdash A$ **by** $\wedge$-elim
3. $B, A \vdash B \wedge A$ **by** $\wedge$-intro

When such a derivation exists, we can say the following:

$$A\wedge B \vdash B\wedge A$$

This is read $A\wedge B$ _derives_ $B\wedge A$.  If a derivation has no precondition it defines a _theorem_.  For example, the symmetric property of conjunction is expressed as:

$$\vdash A\wedge B \Leftrightarrow B \wedge A$$

### Evaluation

Similarly, a multi-step evaluation strings together evaluation rules application.  For example, a simple arithmetic expression evaluates in the following way:

{% highlight text %}
1+3-4
== 4-4
== 0
{% endhighlight %}

`1+3-4` evaluates in one step to `4-4` and again to `0`.  In this case each occurance of `==` represents the application of an evalion rule.

## Code

Download [source]({{site.baseurl}}/haskell/do.hs) for `do` examples from this chapter.

## Notes

[1]:	https://cs.brown.edu/~sk/Publications/Books/ProgLangs/2007-04-26/ "PLAI"
[2]:	http://learnyouahaskell.com "Learn You a Haskell for Great Good"
[3]:	http://haskellbook.com "Haskell Book"
[4]:	https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Maybe.html "Maybe Monad"