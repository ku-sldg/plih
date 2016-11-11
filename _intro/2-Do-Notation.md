---
layout: frontpage
title: Do Notation
use\_math: true
categories: chapter ch1
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