---
layout: frontpage
title: Adding Functions
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
$$

# Functions

**WORK ZONE.  PLEASE GO AWAY.  THIS CHAPTER IS IN HORRIBLE SHAPE.**

For those of us in the functional language community, functions are the essence of computation.  From Church's Lambda Calculus forward, the use of functions to encapsulate and apply computation  has been an essential part of computer science.

As an example of where we want to go, consider a haskell function `inc`  that simply increments the value of its argument and returns the result:

{% highlight haskell %}
inc x = x + 1
{% endhighlight %}

Applying `inc` to an actual parameter value substitutes that parameter with the value in the function body:

{% highlight haskell %}
inc 3
== 3 + 1
== 4
{% endhighlight %}

Like mathematics, the argument to `inc` can be a complex expression:

{% highlight haskell %}
inc ((5+1) - 3)
== inc 6 - 3
== inc 3
== 3 + 1
== 4
{% endhighlight %}

or alternatively:

{% highlight haskell %}
inc ((5+1) - 3)
== ((5+1) - 3) + 1
== 3 + 1
== 4
{% endhighlight %}

The common concept here is substitution of an identifier by an expression in the body of the function.  This will form the basis of function application.

Before we go further, let's establish a few definitions.  In the definition of `inc x` we refer to `x` as a _formal parameter_.  When we apply `inc` to an expression as in `inc ((5+1) - 3)` we refer to `((5+1) - 3)` as an _actual parameter_ or an _argument_.  We refer to the function applied to an expression such as `inc ((5+1) - 3)` as an _application_ of `inc`.  Finally, we refer to the expression that defines `inc`, `x+1` as the _body_ of inc and the _scope_ of `x` as the body of `inc`.

Given all these nice definitions we can describe the application of `inc` to any actual parameter as substituting its formal parameter, `x`, with the actual parameter from the body of `inc`.  We can be a just little more formal and general.  _The application of any function to an actual parameter is the substitution of its formal parameter with its actual parameter in its body.  Looking back to `inc 3` in light of this definition, applying `inc` to `3` is substituting `x` with `3` in `x+1`.

## Defining Functions

Having defined informally what functions do, lets talk briefly about how they are defined before going into a formal definition of their use.

### First-order Functions

*First-order functions* are functions that have a special representation that is not accessible to the programer.  They cannot take other functions as arguments or return functions, thus the name first-order.  Some languages with first-order functions do not even provide a syntax for defining functions, but most allow function definition of some type.

If the definition for `inc` above were done in a first order language, the resulting definition would add a function named `inc` to the environment that can then be called.  Specifically:

{% highlight haskell %}
inc x = x + 1
(inc 10)
{% endhighlight %}

defines `inc` and calls he resulting function on `10`.  Because `inc` is first order, it is not possible to define functions like `map` that would apply `inc` to every element of a list.  No function can be used as an argument to another.  Furthermore, no function can return a function as its return value.  While this may seem an odd thing to do right not, it is a powerful concept that allows such things as currying and partial evaluation that will come later.

### Higher-order Functions

*Higher-order functions* take other functions as arguments and may return functions as values.  Our old friend the `map` function is a great example of a higher-order function:

{% highlight haskell %}
map :: (a -> b) -> [a] -> [b]
{% endhighlight %}

The first argument to `map` is a function mapping elements of type `a` to type `b`.  Similarly, function application may result in a function.  Currying the `map` function is a good example of this:

{% highlight haskell %}
map inc
{% endhighlight %}

In this case, the application of `map` to the function `inc` results in a new function of type that adds `1` to every element of an input list.

### First-class Functions

*First-class functions* are values treated like any other value in a language.  In addition to being applied to actual parameters, they can be created statically, created at run-time, assigned to variables, passed as parameters, and returned as values.  In many languages they can even be reflected upon and examined.

Not stopping at higher-order functions, Haskell has first-class functions as does Scheme and ML.  A function value is created using a lambda or other special form.  In Haskell the following produces a function value that adds `1` to an input value:

{% highlight haskell %}
(\x -> x+1)
{% endhighlight %}

The backslash is used to resemble the lambda from lambda calculus and is easy to remember for that reason.  We can bind a function value to a name just like any other value:

{% highlight haskell %}
inc = (\x -> x+1)
{% endhighlight %}

`inc` is an identifier bound to the function `(\x -> x+1)` and behaves just like any other identifier.  Function values such as these are frequently referred to as *anonymous functions* because they do not have explicit names.  In the Haskell expression:

{% highlight haskell %}
map inc l
{% endhighlight %}

the value for `inc` is found just like the value for `l`.

The use of function values in programming languages dates back to Lisp and is finding its way into many mainstream languages. It is an elegant solution to defining functions that requires little syntax and few semantic extensions.

## Implementing First Order Functions

### Concrete and Abstract Syntax

$$\begin{align*}
t ::=\; & \NUM \mid \ID \mid t + t \mid t - t \\
	  & \mid \bbind \ID=t\; \iin t \\
	  & \mid \llambda \ID\; \ID\; t\; t \\
	  & \mid \aapp \ID \; t \\
\end{align*}$$

The $\matchsf{lambda}$ term defines a new function named by its first $\ID$ argument.  The second $\ID$ names the formal parameters and $t$ is the body.  The final $t$ is the scope over where the function is defined.  In this sense, `lambda` behaves like a `bind` defining a function over an expression.

$\aapp$ is the application of a function to an actual parameter.  $\ID$ is the name of the function and $t$ is the argument.  

We would define our old friend `inc` and use it in an expression as follows:

{% highlight text %}
lambda inc x x+1 in
  (app inc (app inc 7))
{% endhighlight %}

The result is applying `inc` twice to `7`

The associated abstract syntax is a transformation of the concrete syntax to an abstract data type:

{% highlight haskell %}
data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> String -> FBAE -> FBAE
  App :: String -> FBAE -> FBAE
  Id :: String -> FBAE
{% endhighlight %}

### Evaluation

What does it mean to apply a first-order function?

$$\frac{}{(\aapp (\llambda f\; i\; b) \; a) \rightarrow [i\mapsto a]b}[\beta-reduction]$$

The details of implementing `eval` for first-order functions is left as an exercise.

## Implementing First-Class Functions

### Concrete and Abstract Syntax

Our next language will implement first-class functions as an extension of `BAE`.  The concrete syntax for `FBAE` is a simple extension of `BAE` to include function definitions and applications.  Once again we are dropping Booleans for the time being:

$$\begin{align*}
t ::=\; & \NUM \mid \ID \mid t + t \mid t - t \\
	  & \mid \bbind \ID=t\; \iin t \\
	  & \mid \llambda \ID\; \iin t \\
	  & \mid \aapp t \; t \\
\end{align*}$$

The third and fourth lines introduce the new syntax.  Functions are defined with the $\llambda$ keyword following by an argument and function body.  The value of our `inc` function in `FBAE` becomes:

{% highlight text %}
lambda x in x+1
{% endhighlight %}

and we can give it the name `inc` using a `bind`:

{% highlight text %}
let inc = lambda x in x+1 in ...
{% endhighlight %}

where the ellipsis represents the remainder of the program using `inc`.  Applying inc is done using an `app` as in:

{% highlight text %}
let inc = lambda x in x+1 in
  app inc 1
{% endhighlight %}

We can now define a function, give it a name, and apply it to an actual parameter.

Additions to abstract syntax include new fields for `Lambda` and `App`.  The definitions follow immediately from what we defined in the concrete syntax.  The new abstract syntax for `FBAE` becomes:

{% highlight haskell %}
data FBAE where
  Num :: Int -> BAE
  Plus :: BAE -> BAE -> BAE
  Minus :: BAE -> BAE -> BAE
  Bind :: String -> BAE -> BAE -> BAE
  Lambda :: String -> TBAE -> BAE
  App :: BAE -> BAE -> BAE
  Id :: String -> BAE
{% endhighlight %}

### Formal Definition

The most basic definition for functions and their application to arguments comes from Church and the Lambda Calculus.  Hopefully you will have the opportunity to study Lambda Calculus later, but for now we will only borrow some basic concepts for our work.

A hint for our direction is the similarity between the concrete syntax for `bind` and `lambda`:

{% highlight text %}
bind x = 5 in x
lambda x in x
{% endhighlight %}

`bind` defines a new identifier, binds it to a value, and uses the new identifier in the body of the `bind`.  `lambda` is quite similar in that it defines a new identifier and uses the new identifier in the body of the `lambda`.  However, it does not bind the identifier to a particular value.  That binding is deferred until the `lambda` is applied.  `bind` and `lambda` are cousins in that both define new identifiers.  `bind` and `app` are similarly cousins in that both bind values to identifiers.  Regardless of the relationship, we should be able to use the tools for defining `bind` to similarly defined `app`.

The simplest definition for function application is called $\beta$-reduction and uses substitution to replace an identifier with its value.  In the $\beta$-reduction rule below, $i$ is the identifier defined by the $\mathsf{lambda}$, $b$ is the body of the $\llambda$ and $a$ is the argument the $\mathsf{lambda}$ is applied to:

$$\frac{}{(\aapp (\llambda i\; b) \; a) \rightarrow [i\mapsto a]b}[\beta-reduction]$$

The result of the application is $[i\mapsto a]b$, or replacing $i$ with $a$ in $b$.

## Exercises
1. Write an `eval` function for a language with only first-class functions using direct substitutions and the `subst` operation defined for `bind`
2. Modify your `eval` function for a language with only first-class functions to defer substitution using an environment that contains both functions and values.

## Notes
* Talk about Currying
* Talk about DeBrujin numbering
* Talk about name mangling
* Reference for the lambda calculus

An inc x = x + 1 == bind inc be (lambda x => x + 1) in