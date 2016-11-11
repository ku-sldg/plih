---
layout: frontpage
title: Adding Functions
use_math: true
categories: chapter ch1
---

# Functions

For those of us in the functional language community, functions are the essence of computation.  From Church's Lambda Calculus forward, the use of functions to encapsulate and apply computation  has been an essential part of computer science.

## Function Styles

### First-order Functions

*First-order functions* are functions that have a special representation that is not accessible to the programer.  They cannot take other functions as arguments or return functions, thus the name first-order.  Some languages with first-order functions do not even provide a syntax for defining functions, but most allow function definition of some type.

### Higher-order Functions

*Higher-order functions* take other functions as arguments and may return functions as values.  Our old friend the `map` function is a great example of a higher-order function:

{% highlight haskell %}
map :: (a -> b) -> [a] -> [b]
{% endhighlight %}

The first argument to `map` is a function mapping elements of type `a` to type `b`.  Similarly, function application may result in a function.  Currying the `map` function is a good example of this:

{% highlight haskell %}
map (+1)
{% endhighlight %}

In this case, the application of `map` to the function `(+1)` results in a new function of type `[int] -> [int]` that adds `1` to every element of an input list.

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

## Formal Definition

The most basic definition 

$$\frac{a \rightarrow v}{(\mathsf{App}\; (\mathsf{Lambda}\; i\; b) \; a) \rightarrow [i\mapsto v]b}$$

## Notes
* Talk about Currying
* Talk about DeBrujin numbering
* Talk about name mangling