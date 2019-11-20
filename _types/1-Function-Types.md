---
layout: frontpage
title: Function Types
use_math: true
categories: chapter ch3
---

$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeofM}\;}
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

In prior chapters we have defined an interpreter over a language with
only numbers, added Booleans, and showed how to handle and avoid type
errors.  With functions, we don't need to add Booleans to see the same
problem emerge again.  Consider the following expression:

```text
bind inc = lambda x in x + 1 in
  (inc inc)
```

This expression simply applies `inc` to itself, something that made
sense when we discussed untyped recursion.  However, in this context
trying to increment an increment function makes little sense.  The
value of `inc` is a lambda specifying an increment function over
numbers.  In the body of the `bind`, `inc` is applied to itself.
There is of course nothing wrong with applying a function to itself,
but in this case the body of `inc` attempts to add 1 to its argument.
Adding 1 to a function is not defined and causes any of our
interpreters to crash.

We can fix this problem as we have in the past - add run-time or
predictive type checking.  Said using terminology from our discussion
of scoping, we can add *dynamic* or *static* type checking.  As usual,
dynamic performed at run-time and static performed before run-time.

## Typing Bind

Before diving into types of functions, let's take a quick aside into
finding the type of a `bind` remembering that like `lambda` and application,
`bind` defines new identifiers and associates them with values.  To
start with, let's examine a trivial bind that creates an identifier,
binds it to a value, and returns the resulting identifier:

```text
bind x = 1 in
  1+1
```

As always, `bind` evaluates its body and returns the result.  The type
of that result in turn becomes the type of the `bind`.  If we can find
the type of `1+1` we can find the type of what evaluating `bind`
returns.  In this case, that's quite simple using typing rules we
already have.  Specifically, any term of the form `t1+t2` is of type
`TNum` if both `t1` and `t2` are also of type `TNum`.  The type of the
`bind` is clearly `TNum`.

Things are much more interesting if we use the bound identifier in the
body of the `bind`.  Defining new identifiers and giving them values
is what `bind` is all about.  Let's change the body of the `bind` to
use the identifier:

```text
bind x = 1 in
  x+1
```

The same type rule applies to the body.  If `x:TNum` and `1:TNum` then
`x+1:TNum`.  The second term is simple as `1:TNum` by definition.  The
first term requires some thought, but is still rather obvious.  By
definition `x` is bound to `1` in the body of the `bind` and `1` is of
type `TNum`.  It follows that in the body of the `bind` the identifier
`x` has type `TNum`.  We will implement this intuition using a
_context_ that behaves much like an environment does in an evaluator.

Thinking back to how environments work reveals the following simple
annotation for the `bind`:

```text
bind x = 1 in     [(x,1)]
  x+1
```

Following the `x = 1` binding instance the environment contains a
binding of `x` to `1`.  When `x` appears in the `bind` body, it is
looked up in the environment when evaluated.  A _context_ will serve
the same role, but for types rather than values.  In the same way an
environment stores identifiers and values during evaluation, a context
will store identifiers and types during type checking.

The context is maintained exactly like an environment.  When an
identifier is bound, the type of the bound value is associated with
the identifier in the context.  Using a similar notation, we can
decorate the type inference process for `bind`:

```text
bind x = 1 in     [(x,TNum)]
  x+1
```

Now when `x` is used in the term `x+1`, its type is looked up in the
context maintained by the `typeofM` function. Thus, `x+1` is known to
have type `TNum` because both `x` and `1` are type `TNum`.

Let's define the necessary type operations used by the `typeofM`
implementation that will perform the type inference operation.  First,
the case for an identifier:

```haskell
typeofM cont (Id id) = (lookup id cont)
```

The code is virtually identical to looking up the value of an
identifier in an environment.  The function uses the identifier's name
to look up the identifier's type in the context, `cont`. Conveniently,
`lookup` returns a `Maybe`, so we don't need to do anything with its
return value.

The type of a `bind` instance is again similar to evaluation:

```haskell
typeofM cont (Bind i v b) = do { v' <- typeof cont v ;
                                typeof ((i,v'):cont) b }
```

The type of the bound value, `v`, is first determined using the
current context, `cont`.  Then type of the `bind` body is determined
using a new context resulting from the original context with `i` bound
to the bound value's type.  This looks just like evaluation, except
we're generating types rather than values.  Hold that thought.

Now the formal rules.  First for identifiers:

$$\frac{(i,T)\in \Gamma}{(\Gamma\vdash i:T}$$

If $(i,T)$ is in the current context, then type of $i$ is $T$.  This
is a trivial definition, but necessary for completeness.  Now the rule
for `bind`:

$$\frac{(\Gamma\vdash v:D\;\;\;(i,D):\Gamma\vdash b)=R}{\Gamma\vdash (\bbind i=v\;\iin\; b) =R}$$

Finding the type of `bind` gives us all the tools needed to talk about
the type of `lambda` and application.  We've defined what a context is and
how to add an identifier to the context in the scope of a `bind`.  As
we move on to functions, keep these definitions in mind as finding
their types will use quite similar techniques.

## Typing Functions

After a diversion into `bind`, let's think about how to determine the
type of a `lambda`.  A function is at its essence a mapping from an
input value to an output value.  For example:

```text
lambda x in x+1
```

takes a value and returns that value plus 1.  It is also true that
this function will only work correctly on numbers.  Applying `(lambda
x in x+1)` to a Boolean or another function always results in an
error.  We can express this by saying that `x` must be of type `TNum`
using the notation `x:TNum`.  When applied to an argument of the
correct type this `lambda` should result in a good value.  For
example:

```text
((lambda x in x+1) 5)
```

Furthermore, the type of the resulting value can be predicted.  In
this case, the application results in a value of type `TNum`.  Putting
everything together, we can say that this `lambda` expression applied
to any `TNum` will result in `TNum` should it terminate.  To represent
this we use a _function type_.  Specifically, we extend our current
type definition with a new function type in much the same way we
defined binary operators over values like `+` or `-` by using a
recursive construction:

$$\begin{align*}
T ::=\; & \tnum \mid T \rightarrow T \\
\end{align*}$$

This new definition is quite different from our other types.  It allows constructing new types form existing types such as $\tnum \rightarrow \tnum$, $\tnum
\rightarrow (\tnum \rightarrow \tnum)$, and $(\tnum \rightarrow \tnum)
\rightarrow \tnum$.  An informal name for the $\rightarrow$ operation
is a type former or a type function that takes two types and generates
a new type.  Just like `+` takes two numbers and generates a new
number, $\rightarrow$ will take two types and generate a new type.
The left type is called the _domain_ and the right type the _range_ of
the function type.  If no parentheses are included, we assume that
$\rightarrow$ is right associative.  We will often use `D` for the
domain type and the `R` for the range type.

Now that we have a means for defining function types, we can find
types for `lambda` expressions like we did `bind`.  Let's try to
define type inference iteratively by thinking about how we derive the
type of a trivial `lambda`.  Let's start with the simple increment
function and see if we can find its type.  Specifically, we would like
to find `T` in the following expression:

```text
lambda x in x+1 : T
```

All lambda expressions must have a function type, thus `T` must have
the form `D->R` where `D` is the domain type and `R` is the range
type.  We don't know what either `D` or `R` is yet, but we do know
that this particular function should have a function type:

```text
lambda x in x+1 : D->R
```

Now we need to find `D` and `R` and will appeal to how we calculated
the type of `bind`.  Like the `bind`,  `x` is the newly boudn
identifier and `x+1` is the body where `x` is bound.  Just like `bind`
if we can find the type of `x+1` we will know `R`, but to find `R` we
must know the type of `x`.  `bind` uses a context to track defined
identifiers and their types during type checking like evaluation uses
an environment to track identifiers and their values during execution.
If we can add a binding of `x` to its type to the context, its type
will be known when we calculate tye type of `x+1`.  Specifically using
the same notation to track context:

```text
lambda x in    [(x,D)]
  x+1
```

Unfortunately, we have no way of inferring `D` in the general case.
Look carefully at a similar `bind`:

```text
bind x = 1 in    [(x,TNum)]
  x+1
```


Here we do know `D` because `x` is given a value that has an
associated type.  We do know know what value might be given to `x`
until the `lambda` is applied to an argument.  In the specific case of
`x+1` we can know `D` because `+` takes two numbers.  But consider
this trivial `lambda` instance:

```text
lambda x in x
```

There is no way to infer `D` without knowing the value for `x` and
that value will not be known until application evaluates the `lambda`.  It
simply isn't possible to infer the input type in every case.

The way forward is changing the `lambda` definition and _ascribing_ a
type to the function's input parameter declaring what type `D` should
be. In this case, `TNum`:

```text
lambda (x:TNum) in x+1 : TNum->R
```

The notation `(x:TNum)` declares that `x` must be of type `TNum` and
thus `D` becomes `TNum`.  Because we now know the type of `x` we can
find `R` by adding `(x,TNum)` to the context and finding the type of
`x+1`.  When calculating the type of `x+1` using the definition for
the type of `+` we simply look up `x` in the current context.
Finally, type for the `lambda` becomes:

```text
lambda (x:TNum) in x+x : TNum->TNum
```

Generalizing this example, the case for funding the type of a `lambda`
in `typeof` looks like this:

```haskell
typeofM cont (Lambda x t b) = do { tyB <- typeofM ((x,t):cont) b ;
                                  return (t :->: tyB) }
```

`D` is given by the `lambda` declaration.  Find `R` by adding `(x,D)`
to the context and finding the type of `b`.  Given `D` and `R`, the
type of the `lambda` is `D->R`.

The formal type rule can be written:

$$\frac{\typeof(t,(t:D):c)=R}{\typeof((\llambda (x:D)\;\ t),c)=T_d\rightarrow R}$$

## Typing Applications

Knowing how to type `lambda`, we now turn to application.  The two arguments
to application should be a function and an argument to the function.  Let's
consider applying the increment function from the `lambda` example and
finding the resulting type:

```text
((lambda (x:TNum) in x+1) 5) : T
```

The first argument to application must be a fucntion or it cannot be applied
to anything.  Thus we know that it must have the form `D->R`.  In this
example, `(lambda (x:TNum) in x+1)` has type `TNum -> TNum` and both
`D` and `R` are `TNum`.  For the function application to be
successful, we also know the the argument type must be `D`.  If we are
going to evaluate `(f a)`, then `a` must be of a type that `f`
accepts.  In this case, `5` must have type `TNum` as it does.  The
type of the application itself is then `R` because the function type `D->R`
says the output will be of type `R` if the input is of type `D`.  In
this case, the type of the application is `TNum` because the type of `x+1`
in the context of `x:TNum` is `TNum`.

Let's make this way simpler.  Given some `f:D->R` and `a:D`, then
`(f a):R`.  Simple as that.  Here's the type rule that captures
this:

$$\frac{\typeof(f,c)=D \rightarrow R, \typeof(a,c)=D}{\typeof((f\; a),c) = R}$$

What does this say about the problem that motivated the chapter?
Specifically, what is the type of:

```text
bind inc = lambda x in
              x + 1 in
  (inc inc)
```

Let's walk through the type derivation.  First, we add a type to the
function's formal parameter.  It has to be `TNum`.  (Think carefully
about why.)  The expression now becomes:

```text
bind inc = lambda (x:TNum) in
              x + 1 in
  (inc inc)
```

To find the type of `inc`, we must find the type of the `lambda` bound
to it.  `D` is given to be `TNum`, thus we add `(D,TNum)` to the
context and determine the type of `x + 1` to get `R`:

```text
bind inc = lambda (x:TNum) in     [(x,TNum)]
              x + 1 in
  (inc inc)
```

`x` is `TNum` thus `x+1` is `TNum` by the definition of the type of
`t1+t2`.  The type of the `lambda` now becomes `TNum -> TNum` giving
the type for `inc`.  Now add the type of `inc` to the context and look
at the type of the application, noting that the type of `x` drops from the
context when the `lamdba`'s scope closes:

```text
bind inc = lambda (x:TNum) in     [(x,TNum)]
              x + 1 in            [(inc,TNum->TNum)]
  (inc inc)
```

Finding the type of an application requires finding the type of `inc` to be
`TNum -> TNum` by looking it up in the context.  Now we apply a
function of type `TNum -> TNum` to an argument of type `TNum -> TNum`.
Clearly this is a problem because the type of the argument and the
type of function's domain do not match.  The type inference function
will throw and error and we will know _before_ runtime that this
program will crash.

## Static Type Checking

Now let's build `evalM` and `typeofM` for `FBAE`.  First thing we need to do is update the concrete syntax to include a parameter type for `lambda` and a new type constructor for function types:

$$\begin{align*}
t ::=\; & \NUM \mid \ID \mid t + t \mid t - t \\
	  & \mid \bbind \ID=t\; \iin t \\
	  & \mid \llambda (\ID:T)\; \iin t \\
	  & \mid t \; t \\
v ::=\; & \NUM \mid \llambda (\ID:T)\; \iin t \\
T ::=\; & \tnum \mid T \rightarrow T \\
\end{align*}$$

No news here, we simply need to make new constructs available in the concrete syntax.

The abstract syntax follows from the concrete syntax, again adding a type parameter to `Lambda`:

```haskell
data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  deriving (Show,Eq)
```

and a function type constructor to `FBAETy':

```haskell
data TFBAE where
  TNum :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)
```

Note the constructor for function types uses the Haskell infix
constructor notation allowing them to have the form `T :->: T` similar
to how they are written in rules.  Just a bit of Haskell trickery,
nothing significant.

Let's write the `typeofM` function first using `Maybe` and `do` to
construct cases for each term type.  The signature of `typeofM` is a
context and term to a type:

```haskell
typeofM :: Cont -> FBAE -> (Maybe TFBAE)
```

There is nothing new in how types for constants and operations:

```haskell
typeofM cont (Num x) = return TNum
typeofM cont (Plus l r) = do { TNum <- (typeofM cont l) ;
                              TNum <- (typeofM cont r) ;
                              return TNum}
typeofM cont (Minus l r) = do { TNum <- (typeofM cont l) ;
                               TNum <- (typeofM cont r) ;
                               return TNum }
typeofM cont (Bind i v b) = do { v' <- typeofM cont v ;
                                typeofM ((i,v'):cont) b }
typeofM cont (Id id) = (lookup id cont)
typeofM cont (If c t e) = do { TNum <- (typeofM cont c) ;
                              t' <- (typeofM cont t) ;
                              e' <- (typeofM cont e) ;
                              if t'==e'
                              then return t'
                              else Nothing }
```

After all our work defining type rules, the new cases for `typeofM`
follow quickly.  For `Lambda`, `typeofM` is called on the `Lambda` body
with the argument name bound to its type added to the original
context.  `D` is known from the `lambda` and `R` is learned by calling
`typeofM`:

```haskell
typeofM cont (Lambda x t b) = do { tyB <- typeofM ((x,t):cont) b ;
                                  return (t :->: tyB) }
```

The final type becomes `td :->: tr` as defined by our previous type rule.

`App` is a bit more involved, but nothing too dramatic.  First, the
`App`'s argument type is found and bound to and `tyY` respectively.  A
`case` statement finds the type of the `App`'s function element.  If
it is a function type, the domain type is compared to the argument
type and the range type returned if they match.  If they don't, a
mismatch occurs and an error results:

```haskell
typeofM cont (App x y) = do { tyXd :->: tyXr <- typeofM cont x ;
                             tyY <- typeofM cont y ;
                             if tyXd==tyY
                             then return tyXr
                             else Nothing }
```

If the type of the `App`'s function argument is anything but a
function, an error is thrown immediately.

That's it for `typeof` over `FBAE`.  Do we now need a new `eval`?
Other than accounting for the new abstract syntax for `lambda`, no
changes are needed for the `eval` function for `FBAE`.  For brevity,
we'll skip that small update.

## Dynamic Type Checking

The simplest way to do dynamic type checking on this new language with
function types is to do nothing.  Literally.  We have an interpreter
that performs dynamic checking on terms other than `lambda` and application.
While we could use the argument type from `lambda`, the simplest thing
is to do what Lisp variants to and perform substitution and throw
errors when expressions are evaluated.

## Discussion

In our discussion of static type checking for `FBAE` we did not
discuss scoping.  Specifically, now that we've identified dynamic and
static scoping as different approaches, do we need different type
inference capabilities for each?

The difference between static and dynamic scoping is whether the
static declaration environment or the runtime environment is used to
find symbol values.  Obviously, we don't know the runtime environment
until, well, runtime.  Do you see the problem?  If we don't know what
symbol is being referenced until runtime, we can't statically check
it's type.  Look at this definition that slightly modifies an example
from our discussion of static and dynamic scoping:

```text
bind n = 1 in
  bind f = (lambda x in x + n) in
    bind n = true in
      (f 1)
```

Using static scoping, we know the `n` referenced in the `lambda` is
the first `n` that is a number.  Using dynamic scoping, the second `n`
gets used and it is a Boolean.  Delete that binding, and dynamic
scoping matching static scoping.  The same function called in
different places has different types, one legit and one not.

It may be too strong to say we can't statically check this expression.
However, we would need to do something akin to evaluation statically.
That makes no sense.  Regardless, this is yet another argument against
using dynamic scoping.

An important observation is that `typeofM` is simply an evaluation
function that returns types rather than values.  Or, types _are_
values in this particular interpretation.  Programs are simply data
structures and writing interpreters for them - regardless of the
interpretation type - has the same form.  A great exercise to do now
is rewrite the interpreter to use the values `odd` and `even` rather
than numbers.  You will discover that you can reuse almost all your
code from `evalM` or `typeofM` targeting these new values.

## Definitions
* Context - A list of identifiers and their types currently in scope.
* Static Type Checking - Type checking performed before interpretation.
* Dynamic Type Checking - Type checking performed at run-time.

## Exercises
