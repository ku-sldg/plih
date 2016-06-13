---
layout: frontpage
title: Adding Identifiers
---

# Adding Identifiers

The first addition we will make to our language is an _identifier_ using a `bind` expression.  The `bind` expression is like a _let_.  It defines a value to an identifier that may then be used in a subsequent expression.  Consider this trivial example defining `x` to be `5+2`:

{% highlight text %}
bind x = 5+2 in
  x+x-4
{% endhighlight %}

The `bind` expression creates the identifier, `x` and assigns it the value `5+2` using the concrete syntax `x = 5+2`.  The `bind` expression then defines a region where this definition of `x` can be used following the `in` keyword.  Specifically, the expression is `x+x-4`.

A general definition for `bind` would be `bind <id>:<v> in <b>` where `<id>` has the value `<v>` in `<b>` and returns the result of evaluating `<b>`.

Several `bind` expressions can be be nested in simple ways:

{% highlight text %}
bind x = 4 in
  bind y = 5 in
    x+y-4
{% endhighlight %}

where `x` is defined and given the value `4` that can be used in the nested `bind` expression that defines `y`.  The expression `x+y-4` refers to both the definition of `x` and the nested definition of `y`.

There are yet more interesting ways to nest the `bind` expression whose differences can be quite subtle.  The expression:

{% highlight text %}
bind y = 4 in
  y + bind x = y in
        bind x = x+2 in
          x+y-4
      + x
{% endhighlight %}

defines `y` and nests a definition of `x` in another definition of `x`. Reflecting on what traditional languages do, the inner `x` will shadow the outer `x` in it's defined region.  The outer value is used in the definition of the inner `x` value, but the inner `x` is used in its defined region.

Now a slight change placing parens around `x+y-4`.  What does this do?  How does it change the result of evaluating the `bind` expression?

{% highlight text %}
bind y = 4 in
  y + bind x = y in
        (bind x = x+2 in
          x+y-4)
      + x
{% endhighlight %}

## Binding and Scope

_Binding_ and _scope_ are concepts used throughout language specification to describe identifier definition and usage.

An identifier _instance_ is any time that identifier is used.  In the following simple example there are two instances of 'x' and one instance of 'y'.

{% highlight text %}
bind x = 5+2 in
  x+y-4
{% endhighlight %}

A _binding instance_ is where an identifier is declared and given a value.  There is one binding instance for `x` in the previous example:

{% highlight text %}
  x = 5+2
{% endhighlight %}

Generally, `bind` defines a binding instance immediately following the `bind` keyword of the form `<i> = <v>` where `<i>` is a new identifier and `<v>` is an expression providing the new identifier's value.

An identifier's _scope_ is the program region where an identifier can be used.  We say the scope is where the identifier is bound.  The `bind` expression defines the scope of an identifier following the `in` keyword.  In the previous example the scope of `x` is:

{% highlight text %}
  x+y-4
{% endhighlight %}

Thus, the `bind` expression:

{% highlight text %}
	bind <i> = <v> in <s>
{% endhighlight %}

creates a new identifier <i> with value <v> defined in scope <s>.

A _bound instance_ of an identifier is use of the identifier in the scope if its binding.  A _free instance_ of an identifier is use of the identifier outside the scope of its binding.  In the example above, `x` is bound in the body `x+y-4` while `y` is free.  There is a binding for `x`, but none for `y`.

## Nesting `bind`


## Evaluating `bind`

## The BAE Language

### Concrete and Abstract Syntax

The concrete syntax for this new language is:

{% highlight text %}
BAE = num | id | BAE + BAE | BAE - BAE | bind id=BAE in BAE
{% endhighlight %}

from which we can quickly define a Haskell data type for the abstract syntax:

{% highlight haskell %}
data BAE = Num Int
         | Plus BAE BAE
         | Minus BAE BAE
         | Bind String BAE BAE
         | Id String
           deriving Show
{% endhighlight %}

### Calculation

#### Immediate Substitution

`[x->v]s` is defined as _replace all free instances of `x` in `s` with `v`_ and is typically referred to as _substitution_.

#### Deferred substitution

{% highlight haskell %}
type Env = [(String,Int)]
{% endhighlight %}

{% highlight haskell %}
calc :: BAE -> Env -> Int
calc (Num x) env = x
calc (Plus l r) env = (calc l env) + (calc r env)
calc (Minus l r) env = (calc l env) - (calc r env)
calc (Mult l r) env = (calc l env) * (calc r env)
calc (Div l r) env = div (calc l env) (calc r env)
calc (Bind i v b) env =
  let v' = calc v env in
    calc b ((i,v'):env)
calc (Id id) env = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
{% endhighlight %}

### Interpretation
                     
{% highlight haskell %}
interp = calc . parseBAE
{% endhighlight %}
