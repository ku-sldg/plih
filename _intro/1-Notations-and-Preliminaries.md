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

Grammars are represented using `::=` to define variables and `|` to express alternative.  Any symbol in all caps is considered a variable.  The following trivial grammar defines an expression languages consisting of sum and difference operations:

$$
\begin{align*}
t ::= & \NUM \\
      & \mid t + t \\
      & \mid t - t \\
      & \mid if0\; t\; then\; t\; else\; t \\
\end{align*}
$$

Any symbol to the left of the `::=` definition symbol is a meta-variable defined by the definition to the right.  The symbol $t$ represents anything defined by the expression to the right.  Note also that any subscripted or superscripted $t$ such as $t_k$ or $t'$ is definitionally the same as $t$.

There are a few pre-defined meta-variables that include $\NUM$ for integer numbers and $\NAT$ for natural numbers.  We're not worried about writing parsers in this book, but specifying them proves useful.

### Inference Rules

Inference rules define immediate consequences like the definition of $\wedge-introduction$:

$$\frac{A, B}{A \wedge B}$$

This says that if we know $A$ and we know $B$, then we immediately know $A \wedge B$.  More generally:

$$\frac{A_0,A_1,\ldots,A_n}{C}$$

says if $A_0$ through $A_n$ are known to hold, then $C$ is known to hold.  $A_k$ are called _antecedents_ and $C$ is called a _consequent_.

An inference having no preconditions defines an _axiom_:

$$\frac{}{A}$$

If nothing need be true to know $A$, then $A$ is true.

A derivation strings inference rules together:

1. $A \wedge B \vdash B$
2. $A \wedge B \vdash A$
3. $B, A \vdash B \wedge A$