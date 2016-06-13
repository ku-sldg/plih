---
layout: frontpage
title: Failure Is an Option
---

# Failure Is an Option

Our initial interpreter for arithmetic expressions has several nice properties.  All AE programs halt.  There is no mechanism for repeating execution, thus it should not be at all surprising that every AE program terminates and returns a value.  No AE programs crash.  If an AE program parses and can be constructed in the AE abstract syntax, it cannot crash during execution.  There is only one value type and the addition and subtraction operations are closed over that type.  Said differently, everything is an integer  and plus and minus are defined over any pair of integers.

Unfortunately, the same features of AE that make all programs terminate and not crash makes AE useless for programming.  We can't have multiple value types, we can't define variables, we can't define functions, and we can't loop.  To write real programs, we have to have at least some of these capabilities.

## Multiple Value Types

Let's address the first issue - a lack of diverse value types - by adding Boolean values and operations over those values.  Specifically, we will add _if_, _<=_, _&&_, and _isZero_ as well as the values _true_ and _false_.  Certainly this is not a complete set of Boolean operations, but is a representative sample.

We will start with _true_ and _false_, the Boolean constant values.  To represent these values in the abstract syntax, we will use a technique similar to numbers.  Specifically, the Haskell `True` and `False` values will be lifted into our AST using the constructor `Boolean`:

{% highlight haskell %}
	| Boolean Bool
{% endhighlight %}

While `True` and `False` are values in Haskell, `(Boolean True)` and `(Boolean False)` are values in our language.

Next we will add unary and binary operations over Boolean values.  These operations are no different than the binary and unary operations over integers:

{% highlight haskell %}
	| And BAE BAE
	| Leq BAE BAE
	| IsZero BAE BAE
{% endhighlight %}

Finally, we add an _if_ construct in the canonical fashion:

{% highlight haskell %}
	| If BAE BAE BAE
{% endhighlight %}

The resulting abstract syntax data structure is 

{% highlight haskell %}
{% endhighlight %}
