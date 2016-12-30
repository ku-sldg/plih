---
layout: frontpage
title: Function Types
use_math: true
categories: chapter ch3
---

$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeof}\;}
\newcommand\interp{\mathsf{interp}\;}
\newcommand\eval{\mathsf{eval}\;}
\newcommand\NUM{\mathsf{NUM}\;}
\newcommand\ID{\mathsf{ID}\;}
\newcommand\iif{\mathsf{if}\;}
\newcommand\tthen{\;\mathsf{then}\;}
\newcommand\eelse{\;\mathsf{else}\;}
\newcommand\iisZero{\mathsf{isZero}\;}
\newcommand\bbind{\mathsf{bind}\;}
\newcommand\iin{\mathsf{in}\;}
\newcommand\aand{\;\mathsf{\&\&}\;}
\newcommand\lleq{\;\mathtt{<=}\;}
\newcommand\ttrue{\;\mathsf{true}}
\newcommand\ffalse{\;\mathsf{false}}
\newcommand\tnum{\;\mathsf{TNum}}
\newcommand\tbool{\;\mathsf{TBool}}
\newcommand\llambda{\mathsf{lambda}\;}
\newcommand\aapp{\mathsf{app}\;}
\newcommand\tnum{\;\mathsf{TNum}}
\newcommand\tbool{\;\mathsf{TBool}}
$$

# Function Types

In prior chapters we have defined an interpreter over a laguage with only numbers, added Booleans, and showed how to handle and avoid type errors.  With functions, we don't need to add a thing to see this problem.  Consider the following legitimate expression:

{% highlight text %}
bind f = lambda x in x + 1 in
  app f f
{% endhighlight %}

The value of `f` is a lambda specifying an increment function over numbers.  In the body of the `bind`, `f` is applied to itself.  There is of course nothing wrong with applying a function to itself, but in this case the body of `f` attempts to add 1 to its argument.  Adding 1 to a function is not defined and causes any of our iterpreters to crash.

We can fix this problem as we have in the past - add run-time or predictive type checking.  Said using terminology from our disussion of scoping, we can add *dynamic* or *static* type checking.  Dynamic performed at run-time and static performed before run-time.

## Function Types

A function can be viewed as a mapping from an input value to an output value.  If the input value has a type and the output value has a type, then it makes sense that a function type is a mapping from one type to another.  Specifically we extend available types with a function type:

$$\begin{align*}
T :==\; & \tnum \mid T \rightarrow T \\
\end{align*}$$

This allows types such as $\tnum \rightarrow \tnum$, $\tnum
\rightarrow (\tnum \rightarrow \tnum)$, and $(\tnum \rightarrow \tnum)
\rightarrow \tnum$.  An informal name for the $\rightarrow$ operation is a type former or a type function that takes two types and generates a new type.  The left type is called the _domain_ and the right type the _range_ of the function type.  If no parentheses are included, we assume that $\rightarrow$ is right associative.

Now that we have function types, we can assign them to lambda expressions by defining type rules.  Let's try to define the type rule iteratively by thinking about how we derived the type of a lambda.  Let's start with a simple function that doubles its input and see if we can find a type:

{% highlight text %}
lambda x in x+x : T
{% endhighlight %}

The first thing we know is that `T` must be a function type of the form `TD->TR` where `TD` is the domain type and `TR` is the range type.  We don't know what either type is yet, but we do know:

{% highlight text %}
lambda x in x+x : TD->TR
{% endhighlight %}

The `+` operator expects numbers and returns a number.  Thus we can _infer_ that `x` must be a number and `x+x` must be a number.  Therefore, we can conclude the type of this `lambda` is `TNum->TNum`

{% highlight text %}
lambda x in x+x : TNum->TNum
{% endhighlight %}

This is interesting.  In our earlier discussion of types before functions appeared identifiers and values all had types that seemed to come from nowhere.  Numbers are all treated as type `TNum` and Booleans treated as type `TBool`.  As it turns out, all constants have a single type and that single type is defined as an axiom:



## Dynamic Type Checking

## Static Type Checking

$$\begin{align*}
t ::=\; & \NUM \mid \ID \mid t + t \mid t - t \\
	  & \mid \bbind \ID=t\; \iin t \\
	  & \mid \llambda \ID\; \ID\; t\; t \\
	  & \mid \aapp \ID \; t \\
v ::=\; & \NUM \mid \llambda \ID\; \iin t \\
T ::=\; & \tnum \mid T \rightarrow T \\
\end{align*}$$

## Definitions
* Static Type Checking - Type checking performed before interpretation
* Dynamic Type Checking - Type checking performed at run-time.