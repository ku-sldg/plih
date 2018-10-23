---
layout: frontpage
title: Adding Identifiers
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
\newcommand\lleq{\;\mathtt{\<=}\;}
\newcommand\ttrue{\;\mathsf{true}}
\newcommand\ffalse{\;\mathsf{false}}
\newcommand\tnum{\;\mathsf{TNum}}
\newcommand\tbool{\;\mathsf{TBool}}
$$

# Adding Identifiers

The first significant addition we will make to our language is _identifiers_ using a `bind` expression.  The `bind` expression is like a _let_.  It defines a value and binds it to an identifier that may then be used in a subsequent expression.  Consider this trivial example defining `x` to be `5+2`:

{% highlight text %}
bind x = 5+2 in
  x+x-4
{% endhighlight %}

The `bind` expression creates the identifier, `x`, assigns it the value `5+2` using the concrete syntax `x = 5+2`, and defines a region where `x` can be used using the `in` keyword, `x+x-4`.

A general definition for `bind` would be $\bbind i=v\; \iin b$ where $i$ has the value $v$ in $b$ and returns the result of evaluating $b$.


## Binding and Scope

Identifier _instance_, _binding_ and _scope_ are concepts used throughout language specification to describe identifier definition and usage.  An identifier _instance_ is any time that identifier is used.  In the following simple example there are two instances of `x` and one instance of `y`.

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

Nesting allows definition of multiple identifiers that will be simultaneously available.  Each `bind` creates a binding instance for a single identifier, thus the only way to have multiple identifiers is by nesting.  As expressions, `bind` can appear anywhere that a `BAE` expression is allowed.

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

## Concrete and Abstract Syntax

The concrete syntax for this new language is a simple extension of
`AE` that includes identifiers and `bind`.  Note that we are back to
`AE` without Booleans:

$$\begin{align\*}
t ::=\; & \NUM \mid \ID \mid t + t \mid t - t \\
	  & \mid \bbind \ID=t\; \iin t \\
\end{align\*}$$

From the concrete syntax we can quickly define a Haskell data type for
the abstract syntax:

{% highlight haskell %}
data BAE where
  Num :: Int -> BAE
  Plus :: BAE -> BAE -> BAE
  Minus :: BAE -> BAE -> BAE
  Bind :: String -> BAE -> BAE -> BAE
  Id :: String -> BAE
{% endhighlight %}

The additions to `AE` that give us `BAE` are the `Bind` constructor
and the `Id` constructor.  `Id` is defined over a string in the same
way that `Num` is defined over numbers.  `Bind` accepts a string
representing the new identifier, an expression representing the
identifier value, and a scope for the identifier.

## Parsing and Printing

Parsing `BAE` expressions is a simple extension of parsing  `AE`.  We
need only add parsers for the and `id` and `bind` cases:

{% highlight haskell %}
identExpr :: Parser BAE
identExpr = do i <- identifier lexer
	           return (Id i)

bindExpr :: Parser BAE
bindExpr = do reserved lexer "bind"
	          i <- identifier lexer
	          reservedOp lexer "="
	          v <- expr
	          reserved lexer "in"
	          e <- expr
	          return (Bind i v e)
{% endhighlight %}

Both cases are routine.  `identExpr` simply calls the built-in
`identifier` parser and returns the result lifted into the `BAE` AST
using `Id`.  `bindExpr` calls the identifier and `expr` parsers in
sequence then `Bind` to create the term AST. Both cases are integrated
into the main `term` parser in the same way as other expressions:

{% highlight haskell %}
term = parens lexer expr
	   <|> numExpr
	   <|> identExpr
	   <|> bindExpr
{% endhighlight %}

In the previous description, I used the term *lift* for the first
time.  When we lift a term we are transforming a term from the host
language into the external language.  The opposite of lift is
*lower*.

As one would expect, pretty printing `BAE` requires adding only two
cases to the `BAE` pretty printer:

{% highlight haskell %}
pprint (Num n) = show n
pprint (Bind n v b) = "(bind " ++ n ++ " = " ++ pprint v ++ " in " ++ pprint b ++ ")"
{% endhighlight %}

Testing the parser and pretty printer is done exactly as it was with
`BAE`.  We'll skip that here, but it is included in this chapter's
source code.

## Defining Evaluation

The answer to precise definition is the mathematics of inference
rules.  To define `bind` evaluation we will add one new inference
rule:

$$
\frac{\eval a = v}{\eval (\bbind\; i\; = a\;\iin s) = \eval [i\mapsto v]s}\;[BindE]
$$

$BindE$ is not significantly different from earlier evaluation rules
with the exception of a new notation for substitution.  The antecedent
requires the argument, $a$, evaluate to $v$.  The consequent uses the
standard notation $[i\mapsto v]s$ for specify that $i$ is replaced by
$v$ in $s$.  Specifically, $[i\mapsto v]s$ is defined as $s$ with all
free instances of $i$ replaced by $v$.  When evaluating `bind`, the
`bind` itself falls away and instances of its identifier are replaced
with its value.

## Substitution

Let's spend a bit of time to understand the definition of substitution
and why it is the operation we want.  The notation $[i\mapsto v]s$
says that $i$ is replaced by $v$ in $s$.  A simple derivation of the
value of `bind x=5 in x+7` works like this:

{% highlight text %}
eval bind x=5 in x+7
== eval [x->5]x+7 [BindE]
== eval 5+7 [Substitution]
== 12 [PlusE]
{% endhighlight %}

The $BindE$ rule transforms `bind` into a substitution by dropping the
`bind` and performing substitution.  Dropping the `bind` and binding
instance makes any bound instance free.  In the previous example, `x`
is a bound instance in the `bind` expression.  When `bind` is dropped,
`x` becomes free.  Substituting `[x->5]x+7` now replaces free `x`
instances with 5 resulting in `5+7`.

Let's try another example with a nested `bind`:

{% highlight text %}
eval bind x=5 in
	   x + bind x=7 in x
== eval [x->5]x + bind x=7 in x [BindE]
== eval 5 + bind x=7 in x [Substitution]
== eval 5 + eval bind x=7 in x [PlusE]
== eval 5 + eval [x->7]x [BindE]
== eval 5 + eval 7 [substitution]
== 5 + 7 [NumE]
== 12 [PlusE]
{% endhighlight %}

Once again the outermost `bind` is dropped and substitution performed
over the term `x + bind x=7 in x`.  Only the first instance of `x` is
free - the second instance is in the scope of the nested `bind`.  If
we didn't include the condition that identifiers must be free when
replaced, the second `x` would be replaced with the first even though
it is a different identifier instance.

A third example uses the bound identifier in the definition of a
nested identifier:

{% highlight text %}
eval bind x=5 in
	   x + bind y=7+x in y
== [x->5]x + bind y=7+x in y
== 5 + bind y=7+5 in y
== 5 + [y->12]y
== 5 + 12
== 17
{% endhighlight %}

Here the `x` used in defining a value for `y` becomes free when the
binding instance for `x` is dropped.  Thus, the free `x` in the
defining expression is replaced by 5.

## Using Substitution

To define `eval` for `BAE` we need to first define substitution.  The
function `subst x v s` will implement the substitition
$[x\mapsto v]s$.  Once again we treat our program as a data structure
and define `subst` over the `BAE` data type.

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
{% endhighlight %}

The cases for numbers and binary operations are trivial.  Substitution
has no effect on numbers as they represent constant values.  For
addition and subtraction, we simply call substitution recursively on
the two expression terms.

Substituting over an identifier compares the identifier name with the name being substituted for.  If the names are the same, the identifier is replaced with the substituted value.  If the names are not the same, the identifier is left alone.

{% highlight haskell %}
subst i v (Id i') = if i==i'
	                then v
	                else (Id i')
{% endhighlight %}

How do we determine if an identifier is bound or free?  This is
determined by where the identifier occurs and not the identifier
itself.  Then `Bind` case takes care of this by turning off
substitution for the binding instance it creates.  If the identifier
being replaced is the same as the bound instance, then no substitution
is performed in the `bind` body.  If the identifier being replace is
not the same as the bound instance, substitution is performed on the
bound body.  Substitution is always performed on the binding
instance's value expression.

{% highlight haskell %}
subst i v (Bind i' v' b') = if i==i'
	                        then (Bind i' (subst i v v') b')
	                        else (Bind i' (subst i v v') (subst i v b'))
{% endhighlight %}

Note that the scope of an identifier bound by `bind` is the expression
*following* the `in` keyword.  The value expression is explicitly not
a part of the scope.  Aside from impacting substitution, this means
that a bound instance cannot be used in its own definition. For
example:

{% highlight haskell %}
let x=x+x in x
{% endhighlight %}

will not evaluate because the `x` in the value for `x` is free.  It
has no binding instance and will never be replaced during evaluation.

With `subst` defined we can easily define `evals`, an evaluator that
performs substitution is specified by inference rules.  Cases for
`Num`, `Plus` and `Minus` are unchanged from `AE`.  The case for
`Bind` is implemented by substitution.  Specifically, the defined
identifier is replaced by the value expression in the `bind` body.

{% highlight haskell %}
evals :: BAE -> Maybe BAE
evals (Num x) = return (Num x)
evals (Plus l r) = do { l' <- (evals l) ;
                        r' <- (evals r) ;
                        return (liftNum (+) l' r') }
evals (Minus l r) = do { l' <- (evals l) ;
                         r' <- (evals r) ;
                         return (liftNum (-) l' r') }
evals (Bind i v b) = do { v' <- (evals v) ;
						  (evals (subst i v' b)) }
evals (Id id) = Nothing
{% endhighlight %}

The interesting case is for `Id` where an error is raised for an
undeclared variable.  Why is this?  In this interpreter, identifiers
are replaced immediately following their definition.

An interpreter composes the `BAE` parser with our evaluator:

{% highlight haskell %}
interps = evals . parseBAE
{% endhighlight %}

## Generating Test Cases

To test our interpreters, we need to extend the generator use for `AE`
to include `bind` and identifiers.

Generating random names can be done in several ways.  Rather than
generating arbitrary length strings, let's generate single character
identifier names using `choose` to select a character and return the
character converted to a string:

{% highlight haskell %}
genName =
  do i <- choose ('a','z')
	 return [i]
{% endhighlight %}

`genName` will select a name from the range a to z and return it as a string.

Generating an ID from a name is done in the same way we generated
numbers from integers.  Simply take a string, `n` and return `(Id n)`:

{% highlight haskell %}
genId e =
  do n <- choose ('a')
	 return (Id n)

`genID` now generates completely arbitrary identifier names.

The final term type is `Bind` and we'll generate it much the same way
as other multi-argument forms.  `genBind` generates a name for the
bound id, an expression for the bound id value, and an expression for
the body.  The `Bind` constructor puts the elements together to
generate an arbitrary `Bind` construction:

genBind n e =
  do i \<- genName
	 v <- genBAE n
	 b <- genBAE n
	 return (Bind i v b)
{% endhighlight %}

We can integrate both the `Id` and `Bind` generators into the
arbitrary expression generator by adding them to the `oneof` argument
lists for the base case and inductive case respectively.

Now for a QuickCheck test:

{% highlight haskell %}
testEvals n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interps $ pprint t) == (evals t))
{% endhighlight %}

What happens is an almost immediate testing failure resulting from an
undefined identifier.  An example `BAE` instance that causes such a
failure is:

{% highlight haskell %}
(Plus (Id "x") (Num 3))
{% endhighlight %}

It is almost impossible to get QuickCheck to find an arbitrary term
without a free identifier.  Our arbitrary `Id` generator produces `Id`
instances without regard to whether the `Id` is a bound instance.  No
term will evaluate unless its identifiers are all bound.  What we want
is a generator for arbitrary *bound* instances.

Thankfully this is not a difficult feature to add.  What we will do is
input a list of bound identifiers to the `BAE` term generator:

{% highlight haskell %}
genBAE :: Int -> [String] -> Gen BAE
genBAE 0 e =
  do term <- oneof (case e of
	                  [] -> [genNum]
	                  _ -> [genNum
	                       , (genId e)])
	 return term
genBAE n e =
  do term <- oneof [genNum
	               , (genPlus (n-1) e)
	               , (genMinus (n-1) e)
	               , (genBind (n-1) e)]
	 return term
{% endhighlight %}

`genBind` will still generate an arbitrary symbol as its binding
instance name.  When `genBind` calls `genBAE` to generate its body,
the new identifier is added to a list of previously bound
identifiers.

{% highlight haskell %}
genBind n e =
  do i <- genName
	 v <- genBAE n e
	 b <- genBAE n (i:e)
	 return (Bind i v b)
{% endhighlight %}

Now when `genId` is called it is given a list of bound identifiers.
Rather than using `choose` over a range of characters, the `elements`
generator is used to select an arbitrary element of the bound
identifier list:

{% highlight haskell %}
genId e =
  do n <- elements e
	 return (Id n)
{% endhighlight %}

Our new arbitrary term generator produces only terms that include
bound identifiers.  We can QuickCheck `evals` to determine if it
crashes as we did earlier versions of `eval`:

{% highlight haskell %}
testEvals :: Int -> IO ()
testEvals n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interps $ pprint t) == (evals t))
{% endhighlight %}

## Discussion

Let's assume the interpreter defined by `interps` is a faithful
implementation of the `BAE` language and examine how it executes. Each
time a `bind` is encountered, `evals` executes `subst` on the body of
the `bind` expression.  Although this is technically correct following
precisely what the semantics require, it is not efficient.

As an example, consider this intentionally obnoxious code fragment
from `BAE`:

{% highlight text %}
let w=5 in
  let x=7+w in
	let y=14+x+w in
	  let z=5+x+w+y in
	    w+x+y+z
{% endhighlight %}

To execute this code, `evals` would start by replacing `w` with 5 throughout the body of the outer `let`.  This results in:

{% highlight text %}
let x=7+5 in
  let y=14+x+5 in
	let z=5+x+5+y in
	  5+x+y+z
{% endhighlight %}

Each `bind` body and value expression are visited and `w` replaced
with `5`.  Now we evaluate the outer `bind` body the same way,
replacing `x` by 12.

{% highlight text %}
let y=14+12+5 in
  let z=5+12+5+y in
	5+12+y+z
{% endhighlight %}

Do you see the problem?  Every inner `bind` body and value expression
are visited a second time to perform the second replacement.  The
process now repeats for `y` and `z` causing the innermost body to be
processed 4 times and the innermost `bind` value expression 3 times.
For this tiny code fragment, there is no problem.  For complex code,
imagine visiting the entire executable program once for each
identifier evaluated!  We must find a more efficient way.

As inefficient as they are, `interps` and `evals` as are still
useful. `interps` and `evals` define a *normative* interpreter for
`BAE`.  Normative in the sense that the interpreter is correct, but is
not optimized or made efficient in any way.  It serves as an ideal
model for evaluating `BAE`.  In some communities normative
implementations are called *golden* and used to evaluate other
implementations.  We'll see that at work when we test our
interpreters.

## Definitions

* Instance - Any usage of an identifier
* Binding Instance - The instance of an identifier where it is defined
  and given a value
* Scope - The code region where an identifier bound
* Bound Instance - An instance of an identifier occurring the scope of
  a binding instance for that identifier
* Free - An instance of an identifier occurring outside the scope of a
  binding instance for that identifier
* Substitution - Replacing an identifier by its value.  The notation
  $[i\mapsto v]s$ means *replace free instances of i by v in s*.
* Lift - Moving a term from the host language to the external language
* Lower - Moving a term from the external language into the host language.

## Exercises

1. Write a version of `evals` called `evalsErr` that uses the `Either`
   construct to return either a `BAE` construct or an error message.
   How many different error messages need we return?

## Source

Download [source](http://ku-sldg.github.io/plih/haskell/bae.hs) for all interpreter code from this chapter.
