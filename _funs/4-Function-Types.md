---
layout: frontpage
title: Function Types
use_math: true
categories: chapter ch3
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
\newcommand\tnum{\;\mathsf{TNum}}
\newcommand\tbool{\;\mathsf{TBool}}
$$

# Function Types

In prior chapters we have defined an interpreter over a language with only numbers, added Booleans, and showed how to handle and avoid type errors.  With functions, we don't need to add a thing to see this problem emerge again.  Consider the following expression:

{% highlight text %}
bind inc = lambda x in x + 1 in
  app inc inc
{% endhighlight %}

What this expression does is apply `inc` to itself.  The value of `inc` is a lambda specifying an increment function over numbers.  In the body of the `bind`, `inc` is applied to itself.  There is of course nothing wrong with applying a function to itself, but in this case the body of `inc` attempts to add 1 to its argument.  Adding 1 to a function is not defined and causes any of our interpreters to crash.

We can fix this problem as we have in the past - add run-time or predictive type checking.  Said using terminology from our discussion of scoping, we can add *dynamic* or *static* type checking.  Dynamic performed at run-time and static performed before run-time.

## Function Types

A function can be viewed as a mapping from an input value to an output value.  If the input value has a type and the output value has a type, then it makes sense that a function type is a mapping from one type to another.  Specifically we extend available types with a function type:

$$\begin{align*}
T ::=\; & \tnum \mid T \rightarrow T \\
\end{align*}$$

This allows types such as $\tnum \rightarrow \tnum$, $\tnum
\rightarrow (\tnum \rightarrow \tnum)$, and $(\tnum \rightarrow \tnum)
\rightarrow \tnum$.  An informal name for the $\rightarrow$ operation is a type former or a type function that takes two types and generates a new type.  The left type is called the _domain_ and the right type the _range_ of the function type.  If no parentheses are included, we assume that $\rightarrow$ is right associative.

Now that we have function types, we can assign them to lambda expressions by defining type rules.  Let's try to define the type rule iteratively by thinking about how we derived the type of a lambda.  Let's start with a simple function that doubles its input and see if we can find a type:

{% highlight text %}
lambda x in x+x : T
{% endhighlight %}

The first thing we know is that `T` must be a function type of the form `Td->Tr` where `Td` is the domain type and `Tr` is the range type.  We don't know what either type is yet, but we do know:

{% highlight text %}
lambda x in x+x : Td->Tr
{% endhighlight %}

Now we need to find `Td` and `Tr`.  We know the type of `x` is `Td` as `x` is the only input to the function.  Given this, we can infer the type of `Tr` by adding `x:Td` to the context and calling `typeof` as normal.

Unfortunately, we have no way of inferring `Td` in the general case.  In the case of `x+x` we can because `+` takes two numbers.  Although there will be numerous cases like this, it isn't possible to always infer the input type.  The way around this is decorating the function's input parameter with a type.  In this case, `TNum`:

{% highlight text %}
lambda (x:TNum) in x+x : TNum->Tr
{% endhighlight %}

We can find `Tr` by adding `x` to the context.  The type rule can be written:

$$\frac{\typeof(t,(t:T_d):c)=T_r}{\typeof((\llambda (x:T_d)\;\ t),c)=T_d\rightarrow T_r}$$

and the type for the previous `lambda` becomes:

{% highlight text %}
lambda (x:TNum) in x+x : TNum->TNum
{% endhighlight %}

The other new term to find types for is `app`. The two arguments to `app` must be a function and argument.  The function argument must be of type `Td->Tr` or it cannot be applied to anything.  For the function application to be successful, the argument type must be `Td`.  If we are going to evaluate `app f a`, then `a` must be of a type that `f` accepts.  The type of the `app` itself is then `T_r` because the function type `Td->Tr` says the output will be of type `Tr` if the input is of type `Td`.

Here's the type rule that captures this:

$$\frac{\typeof(f,c)=T_d \rightarrow T_r, \typeof(a,c)=T_d}{\typeof((\aapp f\; a),c) = T_r}$$

What does this say about the problem that motivated the chapter?  Specifically, what is the type of:

{% highlight text %}
bind inc = lambda x in x + 1 in
  app inc inc
{% endhighlight %}

First, we have to add a type to the function's formal parameter.  It has to be `TNum`.  (Think carefully about why.)  The expression now becomes:

{% highlight text %}
bind inc = lambda (x:TNum) in x + 1 in
  app inc inc
{% endhighlight %}

The first rule allows us to find the type of `lambda` as `TNum -> TNum` by the rule for function types.  `x` is `TNum` thus `x+x` is `TNum` by the type rule for `+`.  Add the type of `f` to the context and now look at the type of `app`.  First find type of `inc` to be `TNum -> TNum` by looking it up in the context.  Hopefully it's clear the type of `app` cannot be found because `inc` expects a number, but will get a function.  If we let this run, it will crash.

## Static Type Checking

Let's build `eval` and `typeof` for `FBAE`.  First thing we need to do is update the concrete syntax to include a parameter type for `lambda` and a new type constructor for function types:

$$\begin{align*}
t ::=\; & \NUM \mid \ID \mid t + t \mid t - t \\
	  & \mid \bbind \ID=t\; \iin t \\
	  & \mid \llambda (\ID:T)\; \iin t \\
	  & \mid \aapp t \; t \\
v ::=\; & \NUM \mid \llambda (\ID:T)\; \iin t \\
T ::=\; & \tnum \mid T \rightarrow T \\
\end{align*}$$

No news here, we simply need to make new constructs available in the concrete syntax.

The abstract syntax follows from the concrete syntax, again adding a type parameter to `Lambda`:

{% highlight haskell %}
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
{% endhighlight %}

and a function type constructor to `FBAETy':

{% highlight haskell %}
data TFBAE where
  TNum :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)
{% endhighlight %}

Note the constructor for function types uses the Haskell infix constructor notation allowing them to have the form `T:->T` similar to how they are written in rules.  Just a bit of Haskell trickery, nothing significant.

Let's write the `typeof` function first using old-fashioned `let` notation to construct cases for each term type.  The signature of `typeof` is a context and term to a type:

{% highlight haskell %}
typeof :: Cont -> FBAE -> TFBAE
{% endhighlight %}

There is nothing new in how types for constants and operations:

{% highlight haskell %}
typeof cont (Num x) = TNum
typeof cont (Plus l r) = let l' = (typeof cont l)
                             r' = (typeof cont r)
                         in if l'==TNum && r'==TNum
                            then TNum
                            else error "Type Mismatch in +"
typeof cont (Minus l r) = let l' = (typeof cont l)
                              r' = (typeof cont r)
                          in if l'==TNum && r'==TNum
                             then TNum
                             else error "Type Mismatch in -"
typeof cont (Bind i v b) = let v' = typeof cont v in
                             typeof ((i,v'):cont) b
typeof cont (Id id) = case (lookup id cont) of
                        Just x -> x
                        Nothing -> error "Varible not found"
typeof cont (If c t e) = if (typeof cont c) == TNum
                            && (typeof cont t)==(typeof cont e)
                         then (typeof cont t)
                         else error "Type mismatch in if"
{% endhighlight %}

After all our work defining type rules, the new cases for `typeof` follow quickly.  For `Lambda`, `typeof` is called on the `Lambda` body with the argument name bound to its type added to the original context.  `td` is known from the `lambda` and `tr` is learned by calling `typeof`:

{% highlight haskell %}
typeof cont (Lambda x td b) = let tr = typeof ((x,t):cont) b
                             in td :->: tr
{% endhighlight %}

The final type becomes `td :->: tr` as defined by our previous type rule.

`App` is a bit more involved, but nothing too dramatic.  First, the  `App`'s argument type is found and bound to and `tyY` respectively.  A `case` statement finds the type of the `App`'s function element.  If it is a function type, the domain type is compared to the argument type and the range type returned if they match.  If they don't, a mismatch occurs and an error results:

{% highlight haskell %}
typeof cont (App x y) = let tyY = typeof cont y
                        in case typeof cont x of
                             tyXd :->: tyXr ->
                               if tyXd==tyY
                               then tyXr
                               else error "Type mismatch in app"
                             _ -> error "First argument not lambda in app"

If the type of the `App`'s function argument is anything but a function, an error is thrown immediately.

That's it for `typeof` over `FBAE`.  Do we now need a new `eval`?  Other than accounting for the new abstract syntax for `lambda`, no changes are needed for the `eval` function for `FBAE`.  For brevity, we'll skip that small update.

## Dynamic Type Checking

The simplest way to do dynamic type checking on this new language with function types is to do nothing.  Literally.  We have an interpreter that performs dynamic checking on terms other than `lambda` and `app`.  While we could use the argument type from `lambda`, the simplest thing is to do what Lisp variants to and perform substitution and throw errors when expressions are evaluated.

## Discussion

In our discussion of static type checking for `FBAE` we did not discuss scoping.  Specifically, now that we've identified dynamic and static scoping as different approaches, do we need different type inference capabilities for each?

The difference between static and dynamic scoping is whether the static declaration environment or the runtime environment is used to find symbol values.  Obviously, we don't know the runtime environment until, well, runtime.  Do you see the problem?  If we don't know what symbol is being referenced until runtime, we can't statically check it's type.  Look at this definition that slightly modifies an example from our discussion of static and dynamic scoping:

{% highlight text %}
bind n = 1 in
  bind f = (lambda x in x + n) in 
    bind n = true in
      app f 1
{% endhighlight %}

Using static scoping, we know the `n` referenced in the `lambda` is the first `n` that is a number.  Using dynamic scoping, the second `n` gets used and it is a Boolean.  Delete that binding, and dynamic scoping matching static scoping.  The same function called in different places has different types, one legit and one not.

It may be too strong to say we can't statically check this expression.  However, we would need to do something akin to evaluation statically.  That makes no sense.  Regardless, this is yet another argument against using dynamic scoping.

## Definitions
* Static Type Checking - Type checking performed before interpretation
* Dynamic Type Checking - Type checking performed at run-time.

## Exercises
