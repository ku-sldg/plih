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


# What you should know

## Approach

Years ago I started teaching from two books I have grown to love.  I discovered Daniel Friedman’s _Essentials of Programming Languages_ when I was asked to teach programming languages and did not want to do the standard overview.  I dislike the comparative languages approach and was looking for a better way.  Friedman’s book completely changed the way I think about languages and the way I teach programming language material. What it did was convince me that I could teach languages by building interpreters and work gradually towards a formal view of programs and languages.

After teaching from Friedman’s book for two years, I discovered the first version of Shriram Krishnamurthy’s _Programming Languages: Application and Interpretation_.  Shriram writes beautifully and concisely and I have loved teaching from his text.  His work pulled me even more towards the building interpreters approach to teaching programming language fundamentals.  Plus, Shriram generously makes his book available [freely on the web][1].

Both _Essentials of Programming Languages_ and _PLAI_ use Scheme (now Racket) as their host language for interpreters.  Learning Common Lisp in the 1980’s change my life.  Heck, I even met my wife because of Abelson and Sussman’s classic _Structure and Interpretation of Programming Languages_.  I love the Lisp approach and the simple elegance of Scheme.

Then I met Haskell.  My PhD student, Garrin Kimmell, decided that we should shift all our laboratory development work from Java to Haskell.  I wasn’t happy using Java, but I could hire students who knew Java.  If memory serves, I told Garrin that Haskell was an experimental language that would never catch on.  We needed to use Java.  This was Friday.  On Monday, our entire Rosetta tool suite was rewritten in Haskell.  When I saw the elegance of the code and became one with the type system, I was hooked.

KU has become a bit of a Haskell colony.  We hired Andy Gill several years back and committed to Haskell as our primary research and implementation language.  (There are a few of us who still write Scheme in back alleys and speak easies, but I digress.)  For various reasons, we’ve decided to drop Scheme and focus on Haskell exclusively in our undergraduate curriculum.  While one might quibble with that choice, keeping a strong functional language presence should be something we all agree on.

Thus this text on building interpreters in Haskell using basic Haskell idioms.  I use the approach of Friedman and Krishnamurthy focusing on interpreter construction, but have shifted to Haskell as the host language.  I also focus on programs as data structures and introduce types much earlier than Friedman or Krishnamurthy.  Haskell is strongly typed making it natural and types are a huge part of what’s going on in languages today.  I also use the standard Parsec and QuickCheck Haskell libraries during development.

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

## The `do` Notation

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

`1+3-4` evaluates in one step to `4-4` and again to `0`.  In this case each occurance of `==` represents the application of an evaluation rule.

[1]:	https://cs.brown.edu/~sk/Publications/Books/ProgLangs/2007-04-26/ "PLAI"
[2]:	http://learnyouahaskell.com "Learn You a Haskell for Great Good"
[3]:	http://haskellbook.com "Haskell Book"