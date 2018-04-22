---
layout: frontpage
title: Elaboration
use_math: true
categories: chapter ch2
---

# Elaboration and Derived Forms

*Enrichment* is the term we for the process of adding new features to
an existing language.  What we've done in this text is learn a new
concept by enriching an existing language with new features.  We
started with an arithmetic expression language, added Booleans, added
identifiers, and so forth until now we have a statically scoped
language with recursion. 

The technique we've used for enrichment is called *extension*.  When
extending a language we: 

1. add new concrete
2. add abstract syntax
2. add and identify new values
3. define evaluation rules for new constructs
4. define type rules for constructs

This in turn supports defining new interpreters, type checkers, and
other analysis tools. 

The second technique explored here is using a _derived form_.  When we
add a new feature as a derived form we define the new feature in terms
of existing language features.  We will still add new syntax and
types, but we will add the language features themselves by translating
those features into existing constructs. 

Programs are just data structures.  _Elaboration_ is the process of
transforming one expression into another that transforms one data
structure into another.  A derived form defines a transformation while
elaboration performs it.  Elaborators are quite similar in nature to
evaluators and type checkers and optimizers.  This should not be at
all surprising to you as everything we've written has a similar
shape. 

## Simple Elaborator

We haven't played with `AE` in quite awhile, so lets define a simple
elaborator that adds a new operation.  Specifically, let's add the
unary operation `inc`.  `AE` has no functions, so this is the only way
we can add a new expression.  We'll skip the parsing and concrete
syntax and go straight to the original AST: 

{% highlight haskell %}
data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  deriving (Show,Eq)
{% endhighlight %}

Note that `inc` is not defined in the original AST.  If we're going to
add `inc` as a syntactic element, we need a different AST to include
it.  Let's define `AEX` and add `inc`: 

{% highlight haskell %}
data AEX where
  NumX :: Int -> AEX
  PlusX :: AEX -> AEX -> AEX
  MinusX :: AEX -> AEX -> AEX
  MultX :: AEX -> AEX -> AEX
  DivX :: AEX -> AEX -> AEX
  IncX :: AEX -> AEX
  deriving (Show,Eq)
{% endhighlight %}

Note that we've renamed both the type and its constructors so we can
tell them apart.  The elaborator translates AEX to AE: 

{% highlight haskell %}
elab :: AEX -> AE
{% endhighlight %}

and it's rather simple to do.  Every syntactic form in `AEX` has a
similar form in `AE` except `inc`.  That means no translation
necessary.  Just copy the terms like this: 

{% highlight haskell %}
elab (PlusX l r) = (Plus l r)
{% endhighlight %}

Not quite.  What if `l` and `r` are also complex expressions that
contain `inc`?  They won't get elaborated.  Plus, the arguments to
`Plus` are `AE`, not `AEX`.  Let's elaborate them as well: 

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

Note that the name `AEX` is chosen because `AEX` is called the
_eXternal_ language.  In contrast, `AE` is the _internal_ language. 

## Bind as a Derived Form

Our first derived form is for `bind`.  As it turns out, `bind` can be
rewritten in terms of `lambda`.  The derived form of `bind` is
expressed formally as: 

{% highlight text %}
bind x = t1 in t2 == (app (lambda x in t2) t1)
{% endhighlight %}

where `==` is the meta-language notion of equivalence or definition.

When we looked at `bind` originally we said it declares an identifier
and gives the identifier a value.  In effect, it gives a value a name
whose scope is the `bind` body.  In the `bind` expression: 

{% highlight text %}
bind x = 5 in x + 1
{% endhighlight %}

The identifier `x` is bound to `5` in the bind body, `x+1`.

`lambda` is half of bind.  It defines a new identifier and its scope,
but does not give it a value.  That will happen later when the
`lambda` is applied. In the `lambda` expression: 

{% highlight text %}
lambda x in x + 1
{% endhighlight %}

The identifier `x` is defined over `x+1`, but has no value - the
definition half of `bind`. 

The value half of `bind` is achieved with `app`.  When we apply a
`lambda` to a value, the value is bound to the `lambda`'s formal
parameter.  Thus: 

{% highlight text %}
(app (lambda x in x + 1) 5)
{% endhighlight %}

causes `x` to have the value `5` in `x+1`.  Furthermore, this works
for any `lambda` and any value.  Just like `let`.  From these
observations we define the derived form for `bind` as: 

{% highlight text %}
bind x = t1 in t2 == (app (lambda x in t2) t1)
{% endhighlight %}

## Pairs as a Derived Form

Another interesting example uses functions to implement product types,
or what you more likely think of as pairs.  Let's first think about
how we want pairs to behave.  The best way to do this is with
inference rules that describe their behavior: 

In honor of John McCarthy We'll use the classical syntax for pairs and
lists from Lisp.  `cons t1 t2` will create a pair containing the
results of evaluating `t1` and `t2`.  `car (cons t1 t2)` will return
`t1` and `(cdr (cons t1 t2))` will return `t2`.  First, lets define
the abstract syntax of the external language that contains pairs: 

{% highlight haskell %}
data FBAEX where
  NumX :: Int -> FBAEX
  ...
  ConsX :: FBAEX -> FBAEX -> FBAEX
  CarX :: FBAEX -> FBAEX
  CdrX :: FBAEX -> FBAEX
{% endhighlight %}

All we've done is added constructors for `cons`, `car`, and `cdr`
representing the newly added operations.  We will also need a new type
to represent pairs.  We will extend the external language type
representation with a product that contains two types: 

{% highlight haskell %}
data FBAEXTy where
  ...
  TProd :: FBAEXTy -> FBAEXTy -> FBAEXTy
{% endhighlight %}

We'll skip the concrete syntax as it tends to be uninteresting.
Particularly here.  Now the derived forms defining how we translate
pair operations into the internal language `FBAE`: 

{% highlight text %}
cons t1 t2 == lambda x in if x then t1 else t1
car t1 == app t1 true
cdr t1 == app t1 false
{% endhighlight %}

`cons` is implemented as a `lambda` over 1 argument created using the
two pair elments, `t1` and `t2`.  The encapsulated `if` will return
`t1` if the `lambda` input is `true` and `t2` if it is false.  `car`
and `cdr` apply a pair to `true` and `false` respectively.  `car` will
return the first argument to `cons` and `cdr` the second. 

Let's see if we can write the inference rules:

$$\frac{}{\eval car (cons t_1 t_2) = t_1}$$

$$\frac{}{\eval cdr (cons t_1 t_2) = t_1}$$

The only rules here are for `car` and `cdr`.  Where's the rule for
`cons`?  It turns out we don't need one.  The only thing we are about
when we implement pairs is exactly what we wrote.  The behavior of
`cons` is completely defined by the observers `car` and `cdr`. 

## Discussion

Using derived forms and elaboration to define new language features
has numerous advantages over extending a definition.  First, the
implementation is almost always simpler.  We are writing an elaborator
that is simpler than an interpreter that usually needs to work only
about the new feature. 

Second, the original language need not be verified again.  Tested
certainly, but because we are not truly adding anything to the
language except syntax all we need to worry about is the new
definitions and whether they implement the new feature correctly.

For this reason, a variant of elaboration is the basis for building
almost every sophisticated programming language.  A kernel language is
defined and features added by incrementally defining them in terms of
the previous language.  Think about building an onion from the inside
out by adding layers.  The difference between true elaboration and
this approach is there is no formal definition of the elaborator.  One
layer is simply defined in terms of the prior layer. 

## Definitions

* Enrichment - Adding features to a language
* Extension - Adding new features by modifying the evaluation function
* Derived Form - Defining new features in terms of existing expressions
* Elaboration - Translating one abstract syntax into another
* External Language - Language that is input to an elaborator
* Internal Language - Language that is output by an elaborator

## Exercises
* Write the elaborator for pairs
* Define `case` as a derived form using `if`.  You may use other
  language constructs if necessary 
* Define multi-argument functions as a derived form using currying.
