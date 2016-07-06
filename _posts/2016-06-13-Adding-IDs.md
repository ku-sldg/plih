---
layout: frontpage
title: Adding Identifiers
use_math: true
category: chapter
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
$$

# Adding Identifiers

The first significant addition we will make to our language is an _identifier_ using a `bind` expression.  The `bind` expression is like a _let_.  It defines a value to an identifier that may then be used in a subsequent expression.  Consider this trivial example defining `x` to be `5+2`:

{% highlight text %}
bind x = 5+2 in
  x+x-4
{% endhighlight %}

The `bind` expression creates the identifier, `x`, assigns it the value `5+2` using the concrete syntax `x = 5+2`, and defines a region where `x` can be used using the `in` keyword, `x+x-4`.

A general definition for `bind` would be $\bbind i=v\; \iin b$ where $i$ has the value $v$ in $b$ and returns the result of evaluating $b$.


## Binding and Scope

Identifier _instance_, _binding_ and _scope_ are concepts used throughout language specification to describe identifier definition and usage.  An identifier _instance_ is any time that identifier is used.  In the following simple example there are two instances of 'x' and one instance of 'y'.

{% highlight text %}
bind x = 5+2 in
  x+y-4
{% endhighlight %}

A _binding instance_ is the instance where an identifier is declared and given a value.  There is one binding instance for `x` in the previous example following the `bind` keyword, `x = 5+2`  Generally, `bind` specifies a binding instance immediately following the of the form $i = v$ where $i$ is a new identifier and $v$ is an expression providing the new identifier's value.

An identifier's _scope_ is the program region where an identifier can be used.  We say the scope is where the identifier is bound.  The `bind` expression defines the scope of the identifier it creates as the expression following the `in` keyword.  In the previous example the scope of `x` is `x+y-4`

Putting everything together, the `bind` expression:

$$\bbind i = a\; \iin s$$

defines a new identifier $i$ with the value of $a$ defined in scope $s$.

A _bound instance_ of an identifier is use of the identifier in the scope if its binding.  A _free instance_ of an identifier is use of an identifier outside the scope of its binding.  In the example above, `x` is bound in the body `x+y-4` while `y` is free.  There is a binding instance for `x` following the `bind` keyword, but none for `y`. 

## Nesting bind

Nesting allows definition of multiple identifiers that will be simultaneously available.  Each `bind` creates a binding instance for a single identifier, thus the only way to have multiple identifiers is besting.  As expressions, `bind` can appear anywhere that a `BAE` expression is allowed.

First, `bind` expressions can be be nested in simple ways with expected results:

{% highlight text %}
bind x = 4 in
  bind y = 5 in
    x+y-4
{% endhighlight %}

Here the scope of `y` is the expression following `in`, `x+y-4`.  The scope of `x` is the expression following `in`, the nested `bind` expression.  Thus, `x` and `y` are in scope in `x+y-4`.

Bound variables may also be used in definitions of bound identifier values:

{% highlight text %}
bind x = 4 in
  bind y = 5+x in
    x+y-4
{% endhighlight %}

Here `x` is defined and given the value `4` that can be used in the nested `bind` expression that defines `y`.  The expression `5+x` defines the value of `y` using the previously defined `x`. The expression `x+y-4` refers to both the definition of `x` and the nested definition of `y`.

There are yet more interesting ways to nest the `bind` expression whose differences can be quite subtle.  The expression:

{% highlight text %}
bind y = 4 in
  y + bind x = y in
        bind x = x+2 in
          x+y-4
      + x
{% endhighlight %}

defines `y` and nests a definition of `x` in another definition of `x`. Reflecting on what traditional languages do, the inner `x` will shadow the outer `x` in its scope.  The outer value is used in the definition of the inner `x` value, but the inner `x` is used in its defined scope.

Now a slight change placing parens around `x+y-4`.  What does this do?  How does it change the result of evaluating the `bind` expression?

{% highlight text %}
bind y = 4 in
  y + bind x = y in
        (bind x = x+2 in
          x+y-4)
      + x
{% endhighlight %}

The parentheses close the scope of the inner `x` before the last line.  The body of the innermost `bind` changes from `x+y-4+x` to `x+y-4`.  The final x is associated with the outer `bind` that defines `x`.

This is not a good way to define the semantics of `bind`.  It is confusing and ambiguous.  We need an alternative definition that captures how `bind` evaluates in a precise manner.

## The BAE Language

We're now ready to define our new language of Arithmetic Expressions with Bindings (`BAE`).

### Concrete and Abstract Syntax

The concrete syntax for this new language is a simple extension of `AE` that includes identifiers and `bind`.  Note that we are back to `AE` without Booleans:

$$\begin{align*}
t ::=\; & \NUM \mid \ID \mid t + t \mid t - t \\
	  & \mid \bbind \ID=t\; \iin t \\
\end{align*}$$

From the concrete syntax we can quickly define a Haskell data type for the abstract syntax:

{% highlight haskell %}
data BAE where
  Num :: Int -> BAE
  Plus :: BAE -> BAE -> BAE
  Minus :: BAE -> BAE -> BAE
  Bind :: String -> BAE -> BAE -> BAE
  Id :: String -> BAE
{% endhighlight %}

The additions to `AE` that give us `BAE` are the `Bind` constructor and the `Id` constructor.  `Id` is defined over a string in the same way that `Num` is defined over numbers.  `Bind` accepts a string representing the new identifier, an expression representing the identifier value, and a scope for the identifier.

### Defining Evaluation

The answer to precise definition is the mathematics of inference rules.  To define `bind` evaluation we will add one new inference rule:

$$
\frac{\eval a == v}{\eval (\bbind\; i\; = a\;\iin s) == [i\mapsto v]s}\;[BindE]
$$

$BindE$ is not significantly different from earlier evaluation rules with the exception of a new notation.  The antecedent requires the argument, $a$, evaluate to $v$.  The notation $[i\mapsto v]s$ is a standard form for *substitution*.  Specifically, $[i\mapsto v]s$ means $s$ with all free instances of $i$ replaced by $v$.

### Evaluation

#### Immediate Substitution

`[x->v]s` is defined as _replace all free instances of `x` in `s` with `v`_ and is typically referred to as _substitution_.

{% highlight haskell %}

subst :: String -> BAE -> BAE -> BAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')
       
evals :: BAE -> Int
evals (Num x) = x
evals (Plus l r) = (evals l) + (evals r)
evals (Minus l r) = (evals l) - (evals r)
evals (Bind i v b) = (evals (subst i (Num (evals v)) b))
evals (Id id) = error "Undeclared Variable"
{% endhighlight %}

{% highlight haskell %}
interps = evals . parseBAE
{% endhighlight %}


#### Deferred substitution

{% highlight haskell %}
type Env = [(String,Int)]
{% endhighlight %}

{% highlight haskell %}
eval :: Env -> BAE -> Int
eval env (Num x) = x
eval env (Plus l r) = (eval env l) + (eval env r)
eval env (Minus l r) = (eval env l) - (eval env r)
eval env (Bind i v b) =
  let v' = eval env v in
    eval ((i,v'):env) b
eval env (Id id) = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
{% endhighlight %}
                                            
{% highlight haskell %}
interp = (eval []) . parseBAE
{% endhighlight %}
                     
## Definitions

* Instance - Any usage of an identifier
* Binding Instance - The instance of an identifier where it is defined and given a value
* Scope - The code region where an identifier bound
* Bound Instance - An instance of an identifier occurring the scope of a binding instance for that identifier
* Free - An instance of an identifier occurring outside the scope of a binding instance for that identifier
* Substitution - Replacing an identifier by its value.  The notation $[i\mapsto v]s$ means *replace free instances of i by v in s*.

## Exercises

## Source

Download [source]({{site.baseurl}}/haskell/bae.hs) for all interpreter code from this chapter.

## Notes
