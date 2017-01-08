---
layout: frontpage
title: Elaboration
use_math: true
categories: chapter ch2
---

# Elaboration and Derived Forms

*Enrichment* is the term we for the process of adding new features to an existing language.  What we've done in this text is learn a new concept by enriching an existing language with new features.  We started with an arithmetic expression language, added Booleans, added identifiers, and so forth until now we have a statically scoped language with recursion.

The technique we've used for enrichment is called *extension*.  When extending a language we:

1. add new concrete
2. add abstract syntax
2. add and identify new values
3. define evaluation rules for new constructs
4. define type rules for constructs

This in turn supports defining new interpreters, type checkers, and other analysis tools.

The second technique explored here is using a _derived form_.  When we add a new feature as a derived form we define the new feature in terms of existing language features.  We will still add new syntax and types, but we will add the language features themselves by translating those features into existing constructs.

Programs are just data structures.  _Elaboration_ is the process of transforming one expression into another that transforms one data structure into another.  A derived form defines a transformation while elaboration performs it.  Elaborators are quite similar in nature to evaluators and type checkers and optimizers.  This should not be at all surprising to you as everything we've written has a similar shape.

A variant of elaboration is the basis for building almost every sophisticated programming language.  A kernel language is defined and features added by incrementally defining them in terms of the previous language.  Think about building an onion from the inside out by adding layers.  The difference between true elaboration and this approach is there is no formal definition of the elaborator.  One layer is simply defined in terms of the prior layer.

## Simple Elaborator

We haven't played with `AE` in quite awhile, so lets define a simple elaborator that adds a new operation.  Specifically, let's add the unary operation `inc`.  `AE` has no functions, so this is the only way we can add a new expression.  We'll skip the parsing and concrete syntax and go straight to the original AST:

{% highlight haskell %}
data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  deriving (Show,Eq)
{% endhighlight %}

Note that `inc` is not defined in the original AST.  If we're going to add `inc` as a syntactic element, we need a different AST to include it.  Let's define `AEX` and add `inc`:

data AEX where
  NumX :: Int -> AEX
  PlusX :: AEX -> AEX -> AEX
  MinusX :: AEX -> AEX -> AEX
  MultX :: AEX -> AEX -> AEX
  DivX :: AEX -> AEX -> AEX
  IncX :: AEX -> AEX
  deriving (Show,Eq)

Note that we've renamed both the type and its constructors so we can tell them apart.  The elaborator translates AEX to AE:

{% highlight haskell %}
elab :: AEX -> AE
{% endhighlight %}

and it's rather simple to do.  Every syntactic form in `AEX` has a similar form in `AE` except `inc`.  That means no translation necessary.  Just copy the terms like this:

{% highlight haskell %}
elab (PlusX l r) = (Plus l r)
{% endhighlight %}

Not quite.  What if `l` and `r` are also complex expressions that contain `inc`?  They won't get elaborated.  Plus, the arguments to `Plus` are `AE`, not `AEX`.  Let's elaborate them as well:

{% highlight haskell %}
elab (PlusX l r) = (Plus (elab l) (elab r))
{% endhighlight %}

This will work and will work for every term other than `inc`:

{% highlight haskell %}
elab (NumX n) = (Num n)
elab (PlusX l r) = (Plus (elab l) (elab r))
elab (MinusX l r) = (Minus (elab l) (elab r))
elab (MultX l r) = (Mult (elab l) (elab r))
elab (DivX l r) = (Div (elab l) (elab r))
{% endhighlight %}

Now we need to take care of `IncX`, but we have a definition for it:

{% highlight text %}
inc x == x + 1
{% endhighlight %}

The case for `inc` becomes:

{% highlight haskell %}
elab (IncX t) = (Plus (elab t) (Num 1))
{% endhighlight %}

Now we're done.  `inc x` translates to `x+1`

To use the elaborator, we call it right before `eval`:

{% highlight haskell %}
interp t = eval . elab
{% endhighlight %}

Note that the name `AEX` is chosen because `AEX` is called the _eXternal_ language.  In contrast, `AE` is the _internal_ language.

## Bind as a Derived Form

Our first derived form is for `bind`.  As it turns out, `bind` can be rewritten in terms of `lambda`.  The derived form of `bind` is expressed formally as:

{% highlight text %}
bind x = t1 in t2 == (app (lambda x in t2) t1)
{% endhighlight %}

where `==` is the meta-language notion of equivalence or definition.

When we looked at `bind` originally we said it declares an identifier and gives the identifier a value.  In effect, it gives a value a name whose scope is the `bind` body.  In the `bind` expression:

{% highlight text %}
bind x = 5 in x + 1
{% endhighlight %}

The identifier `x` is bound to `5` in the bind body, `x+1`.

`lambda` is half of bind.  It defines a new identifier and its scope, but does not give it a value.  That will happen later when the `lambda` is applied. In the `lambda` expression:

{% highlight text %}
lambda x in x + 1
{% endhighlight %}

The identifier `x` is defined over `x+1`, but has no value - the definition half of `bind`.

The value half of `bind` is achieved with `app`.  When we apply a `lambda` to a value, the value is bound to the `lambda`'s formal parameter.  Thus:

{% highlight text %}
(app (lambda x in x + 1) 5)
{% endhighlight %}

causes `x` to have the value `5` in `x+1`.  Furthermore, this works for any `lambda` and any value.  Just like `let`.  From these observations we define the derived form for `bind` as:

{% highlight text %}
bind x = t1 in t2 == (app (lambda x in t2) t1)
{% endhighlight %}

## Pairs as a Derived Form

{% highlight text %}
pair t1 t2 == lambda x in if x then t1 else t1
fst t1 == app t1 true
snd t1 == app t1 false
{% endhighlight %}

### letrec as a Derived Form

$$letrec x:T=t_1 in t_2 == bind x = fix (\lambda x:T in t_1) in t_2$$

## Verification and Testing

## Discussion

## Definitions

* Enrichment - Adding features to a language
* Extension - Adding new features by modifying the evaluation function
* Derived Form - Defining new features in terms of existing expressions
* Elaboration - Translating one abstract syntax into another
* External Language - Language that is input to an elaborator
* Internal Language - Language that is output by an elaborator

## Exercises
* Write the elaborator for pairs
* Define `case` as a derived form using `if`.  You may use other language constructs if necessary