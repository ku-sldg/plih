---
layout: frontpage
title: Notations and Preliminaris
use_math: true
category: chapter
---
# What you should know

## Haskell

This is not a Haskell text, although I will cover some things in Haskell you might not be aware of.  Monads, GADTs, and point-free style come to mind quickly.  However, don't read this book to learn Haskell as there are much better sources both online and as traditional books:

## Data Types and Recursion

Understanding Haskell data types and recursion is critical to understanding how interpreters work.  Recursion is your friend and will serve you well.

# Notations

## Math and Code

Some things are formatted as mathematics:

$$\{s:string\mid P(s)\}$$

Some things are formatted as code:

{% highlight haskell %}
data AE = Num Int
        | Plus AE AE
        | Minus AE AE
          deriving (Show,Eq)
{% endhighlight %}

## Meta Language

### Grammars

{% highlight text %}
<AE> ::= <num> | <AE> + <AE> | <AE> - <AE>
{% endhighlight %}

### Inference Rules

Inference rules define immediate consequences like the definition of $\wedge$-introduction:

$$\frac{A,B}{A\wedge B}$$

This says that if we know $A$ and we know $B$, then we immediately know $A\wedge B$.  More generally:

$$\frac{A_0,A_1,\ldots,A_n}{C}$$

says if $A_0$ through $A_n$ are known to hold, then $C$ is known to hold.  $A_k$ are called _antecedents_ and $C$ is called a _consequent_.

A derivation strings inference rules together:

$$\frac{\frac{A\wedge B}{B}\frac{A\wedge B}{A}}{B\wedge A}$$

An inference having no preconditions defines an _axiom_:

$$\frac{}{A}$$

If nothing need be true to know $A$, then $A$ is true.

If $B$ is derivable from $A$, we say:

$$A\ldash B$$

where $\ldash$ is called the _syntactic turnstile_.
