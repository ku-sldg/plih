---
layout: frontpage
title: Notations and Preliminaries
use\_math: true
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

Years ago I started teaching from two books I have grown to love.  I discovered Daniel Friedman’s _Essentials of Programming Languages_ when I was asked to teach programming languages and did not want to do the standard overview.     Friedman’s book completely changed the way I think about languages and the way I teach programming language material. What it did was convince me that I could teach languages by building interpreters and work gradually towards a formal view of software.

After teaching from Friedman’s book for two years, I discovered the first version of Shriram Krishnamurthy’s _Programming Languages: Application and Interpretation_.  Shriram writes beautifully and concisely and I have loved teaching from his text.  His work pulled me even more towards the building interpreters approach to teaching programming language fundamentals.  Plus, Shriram generously makes his book available[freely on the web][1].

Both _Essentials of Programming Languages_ and _PLAI_ use Scheme as their host language for interpreters.  Learning Common Lisp in the 1980’s change my life.  Heck, I even met my wife because of Abelson and Sussman’s classic _Structure and Interpretation of Programming Languages_.  I love the Lisp approach and the simplicity of Scheme.

Then I met Haskell.  My PhD student, Garrin Kimmell, decided that we should shift all our laboratory development work from Java to Haskell.  I wasn’t happy using Java, but I could hire students who knew Java.  If memory serves, I told Garrin that Haskell was an experimental language that would never catch on.  We needed to use Java.  This was Friday.  On Monday, our entire Rosetta tool suite was rewritten in Haskell.  When I saw the elegance of the code and became one with the type system, I was hooked.

KU has become a bit of a Haskell colony.  We hired Andy Gill several years back and committed to Haskell as our primary research and implementation language.  (There are a few of us who still write Scheme in back alleys and speak easies, but I digress.)  For various reasons, we’ve decided to drop Scheme and focus on Haskell exclusively in our undergraduate curriculum.  While one might quibble with that choice, keeping a strong functional language presence should be something we all agree on.

Thus this text on building interpreters in Haskell using basic Haskell idioms.  I use the approach of Friedman and Krishnamurthy focusing on interpreter construction, but have shifted to Haskell as the host language.  I also focus on programs as data structures and introduce types much earlier than Friedman or Krishnamurthy.  Haskell is strongly typed making it natural and types are a huge part of what’s going on in languages today.  I also use the standard Parsec and QuickCheck Haskell libraries during development.

## Haskell

This is _not_ a Haskell text.  I will cover some things in Haskell you might not be aware of, but I make no claims about writing big, beautiful Haskell.  Monads, GADTs, and point-free style come to mind quickly as important concepts that we’ll use.  However, don't read this book to learn Haskell.  There are much better sources both online and as traditional books:

[Learn you a Haskell for Great Good][2]
[Haskell Book][3]

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
\begin{align\*}
t ::= & \NUM \\
	  & \mid t + t \\
	  & \mid t - t \\
	  & \mid if0\; t\; then\; t\; else\; t \\
\end{align\*}
$$

Any symbol to the left of the `::=` definition symbol is a meta-variable defined by the definition to the right.  The symbol $t$ represents anything defined by the expression to the right.  Note also that any subscripted or superscripted $t$ such as $t\_k$ or $t'$ is definitionally the same as $t$.

There are a few pre-defined meta-variables that include $\NUM$ for integer numbers and $\NAT$ for natural numbers.  We're not worried about writing parsers in this book, but specifying them proves useful.

### Inference Rules

Inference rules define immediate consequences like the definition of $\wedge-introduction$:

$$\frac{A, B}{A \wedge B}$$

This says that if we know $A$ and we know $B$, then we immediately know $A \wedge B$.  More generally:

$$\frac{A\_0,A\_1,\ldots,A\_n}{C}$$

says if $A\_0$ through $A\_n$ are known to hold, then $C$ is known to hold.  $A\_k$ are called _antecedents_ and $C$ is called a _consequent_.

An inference having no preconditions defines an _axiom_:

$$\frac{}{A}$$

If nothing need be true to know $A$, then $A$ is true.

A derivation strings inference rules together:

1. $A \wedge B \vdash B$ by $\wedge-elim$
2. $A \wedge B \vdash A$ by $\wedge-elim$
3. $B, A \vdash B \wedge A$ by $\wedge-intro$

Similarly, an evaluation strings together evaluation rules:

{% highlight text %}
1+3-4
== 4-4
== 0
{% endhighlight %}

[1]:	https://cs.brown.edu/~sk/Publications/Books/ProgLangs/2007-04-26/ "PLAI"
[2]:	http://learnyouahaskell.com "Learn You a Haskell for Great Good"
[3]:	http://haskellbook.com "Haskell Book"