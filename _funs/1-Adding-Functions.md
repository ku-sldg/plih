---
layout: frontpage
title: Adding Functions
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
\newcommand\lleq{\;\mathtt{<=}\;}
\newcommand\ttrue{\;\mathsf{true}}
\newcommand\ffalse{\;\mathsf{false}}
\newcommand\tnum{\;\mathsf{TNum}}
\newcommand\tbool{\;\mathsf{TBool}}
\newcommand\llambda{\mathsf{lambda}\;}
\newcommand\aapp{\mathsf{app}\;}
$$

# Functions

For those of us in the functional language community, functions are the essence of computation.  From Church's Lambda Calculus forward, the use of functions to encapsulate and apply computation  has been an essential part of computer science.

As an example of where we want to go, consider a Haskell function `inc`  that simply increments the value of its argument and returns the result:

```haskell
inc x = x + 1
```

Applying `inc` to an actual parameter value substitutes that parameter with the value in the function body:

```haskell
inc 3
== 3 + 1
== 4
```

Like mathematics, the argument to `inc` can be a complex expression:

```haskell
inc ((5+1) - 3)
== inc 6 - 3
== inc 3
== 3 + 1
== 4
```

or alternatively:

```haskell
inc ((5+1) - 3)
== ((5+1) - 3) + 1
== 3 + 1
== 4
```

The common concept here is substitution of an identifier by an expression in the body of the function.  This will form the basis of function application.

Before we go further, let's establish a few definitions.  In the definition of `inc x` we refer to `x` as a _formal parameter_.  When we apply `inc` to an expression as in `inc ((5+1) - 3)` we refer to `((5+1) - 3)` as an _actual parameter_ or an _argument_.  We refer to the function applied to an expression such as `inc ((5+1) - 3)` as an _application_ of `inc`.  Finally, we refer to the expression that defines `inc`, `x+1` as the _body_ of inc and the _scope_ of `x` as the body of `inc`.

Given all these nice definitions we can describe the application of `inc` to any actual parameter as substituting its formal parameter, `x`, with the actual parameter from the body of `inc`.  We can be a just little more formal and general.  _The application of any function to an actual parameter is the substitution of its formal parameter with its actual parameter in its body.  Looking back to `inc 3` in light of this definition, applying `inc` to `3` is substituting `x` with `3` in `x+1`.

## Defining Functions

Having defined informally what functions do, lets talk briefly about how they are defined before going into a formal definition of their use.

### First-order Functions

*First-order functions* are functions that have a special representation that is not accessible to the programer.  They cannot take other functions as arguments or return functions, thus the name first-order.  Some languages with first-order functions do not even provide a syntax for defining functions, but most allow function definition of some type.

If the definition for `inc` above were done in a first order language, the resulting definition would add a function named `inc` to the environment that can then be called.  Specifically:

```haskell
inc x = x + 1
(inc 10)
```

defines `inc` and calls he resulting function on `10`.  Because `inc` is first order, it is not possible to define functions like `map` that would apply `inc` to every element of a list.  No function can be used as an argument to another.  Furthermore, no function can return a function as its return value.  While this may seem an odd thing to do right not, it is a powerful concept that allows such things as currying and partial evaluation that will come later.

### Higher-order Functions

_Higher-order functions_ take other functions as arguments and may return functions as values.  Our old friend the `map` function is a great example of a higher-order function:

```haskell
map :: (a -> b) -> [a] -> [b]
```

The first argument to `map` is a function mapping elements of type `a` to type `b`.  Similarly, function application may result in a function.  Currying the `map` function is a good example of this:

```haskell
map inc
```

In this case, the application of `map` to the function `inc` results in a new function of type that adds `1` to every element of an input list.

### First-class Functions

_First-class functions_ are values treated like any other value in a language.  In addition to being applied to actual parameters, they can be created statically, created at run-time, assigned to variables, passed as parameters, and returned as values.  In many languages they can even be reflected upon and examined.

Not stopping at higher-order functions, Haskell has first-class functions as does Scheme and ML.  A function value is created using a lambda or other special form.  In Haskell the following produces a function value that adds `1` to an input value:

```haskell
(\x -> x+1)
```

The backslash is used to resemble the lambda from lambda calculus and is easy to remember for that reason.  We can bind a function value to a name just like any other value:

```haskell
inc = (\x -> x+1)
```

`inc` is an identifier bound to the function `(\x -> x+1)` and behaves just like any other identifier.  Function values such as these are frequently referred to as *anonymous functions* because they do not have explicit names.  In the Haskell expression:

```haskell
map inc l
```

the value for `inc` is found just like the value for `l`.
The use of function values in programming languages dates back to Lisp and is finding its way into many mainstream languages. It is an elegant solution to defining functions that requires little syntax and few semantic extensions.

{% comment %}

## Implementing First Order Functions

### Concrete and Abstract Syntax

$$\begin{align*}
t ::=\; & \NUM \mid \ID \mid t + t \mid t - t \\
	  & \mid \bbind \ID=t\; \iin t \\
	  & \mid \llambda \ID\; \ID\; t\; t \\
	  & \mid \aapp \ID \; t \\
\end{align*}$$

The $\mathsf{lambda}$ term defines a new function named by its first $\ID$ argument.  The second $\ID$ names the formal parameter and $t$ is the body.  The final $t$ is the scope over where the function is defined.  In this sense, `lambda` behaves like a `bind` defining a function over an expression.

$\aapp$ is the application of a function to an actual parameter.  $\ID$ is the name of the function and $t$ is the argument.

We would define our old friend `inc` and use it in an expression as follows:

```haskell
lambda inc x x+1 in
  (app inc (app inc 7))
```

The result is applying `inc` twice to `7`

The associated abstract syntax is a transformation of the concrete syntax to an abstract data type:

```haskell
data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> String -> FBAE -> FBAE
  App :: String -> FBAE -> FBAE
  Id :: String -> FBAE
```

### Evaluation

What does it mean to apply a first-order function?

$$\frac{}{(\aapp (\llambda f\; i\; b) \; a) \rightarrow [i\mapsto a]b}[\beta-reduction]$$

The details of implementing `eval` for first-order functions is left as an exercise.

{% endcomment %}

## Implementing First-Class Functions

### Concrete and Abstract Syntax

Our next language will implement first-class functions as an extension of `BAE`.  The concrete syntax for `FBAE` is a simple extension of `BAE` to include function definitions and applications.  Once again we are dropping Booleans for the time being:

$$\begin{align*}
t ::=\; & \NUM \mid \ID \mid t + t \mid t - t \\
	  & \mid \bbind \ID=t\; \iin t \\
	  & \mid \llambda \ID\; \iin t \\
	  & \mid \aapp t \; t \\
\end{align*}$$

The third and fourth lines introduce the new syntax.  Functions are defined with the $\llambda$ keyword following by an argument and function body.  The value of our `inc` function in `FBAE` becomes:

```haskell
lambda x in x+1
```

and we can give it the name `inc` using a `bind`:

```haskell
bind inc = (lambda x in x+1) in ...
```

where the ellipsis represents the remainder of the program using `inc`.  Applying inc is done using an `app` as in:

```haskell
bind inc = lambda x in x+1 in
  app inc 1
```

We can now define a function, give it a name, and apply it to an actual parameter.

Additions to abstract syntax include new fields for `Lambda` and `App`.  The definitions follow immediately from what we defined in the concrete syntax.  The new abstract syntax for `FBAE` becomes:

```haskell
data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
```

### Formal Definition

The most basic definition for functions and their application to arguments comes from Church and the Lambda Calculus.  Hopefully you will have the opportunity to study Lambda Calculus later, but for now we will only borrow some basic concepts for our work.

A hint for our direction is the similarity between the concrete syntax for `bind` and `lambda`:

```haskell
bind x = 5 in x
lambda x in x
```

`bind` defines a new identifier, binds it to a value, and uses the new identifier in the body of the `bind`.  `lambda` is quite similar in that it defines a new identifier and uses the new identifier in the body of the `lambda`.  However, it does not bind the identifier to a particular value.  That binding is deferred until the `lambda` is applied.  `bind` and `lambda` are cousins in that both define new identifiers.  `bind` and `app` are similarly cousins in that both bind values to identifiers.  Regardless of the relationship, we should be able to use the tools for defining `bind` to similarly defined `app`.

The simplest definition for function application is called $\beta$-reduction and uses substitution to replace an identifier with its value.  In the $\beta$-reduction rule below, $i$ is the identifier defined by the $\mathsf{lambda}$, $b$ is the body of the $\llambda$ and $a$ is the argument the $\mathsf{lambda}$ is applied to:

$$\frac{}{(\aapp (\llambda i\; b) \; a) \rightarrow [i\mapsto a]b}[\beta-reduction]$$

The result of the application is $[i\mapsto a]b$, or replacing $i$ with $a$ in $b$.  This is exactly how we expect functions to behave. When applied, their formal parameter is replaced with an actual parameter and the result becomes the function application's value.

### Implementation

Let's think about how to implement evaluating `App` and `Lambda` using the substitution operator defined for `bind`.  We established that `bind` both defines and replaces an identifier while function definition defines an identifier and application replaces it.  Let's review the substitution performed for `bind` in the `evalS` function:

```haskell
evalS (Bind i v b) = do { v' <- evalS v ;
                          (evalS (subst i v' b)) }
```

The `subst` function gets is identifier from the `bind` syntax as well as the value it is substituting, `v`.  The body, `b`, is also found in the `bind` expression.  Thus, the `subst` application is simple.  We can understand the evaluation of `App` by looking for the arguments to `subst` in the `lambda` and its actual parameter.

Before evaluating `App`, lets evaluate both its arguments. Specifically, `f` to get a lambda value and `a` to get a value for the actual parameter:

```haskell
evalS (App f a) = do { (Lambda i b) <- (evalS f) ;
                       a' <- (evalS a) ;
                       evalS (subst i a' b) }
```

Now we have the pieces needed to apply `subst`.  `i` is the identifier defined by the `Lambda`.  The argument to the substitution is provided by the `App` as `a'`.  We evaluate it first before passing it into the substitution.  Finally, the body of the function, `b`, is the expression substituted into.  The result is the following application of `subst`:

```haskell
evalS (subst i a' b) }
```

and embedding the substitution into the case for `App` gives us the following:

```haskell
evalS (App f a) = do { (Lambda i b) <- (evalS f) ;
                       a' <- (evalS a) ;
                       evalS (subst i a' b) }
```

And there we have it.  Substitute the argument in for the identifier in the body of the function.

What about the `Lambda` case for `evalS`?  As it turns out, `lambda`s are values.  Just like `True` or `(Num 0)`, `(Lambda x (Plus (Id x) (Num 1)))` is a value and is not evaluated further.  Thus, the evaluation of `Lambda` is trivial:

```haskell
evalS (Lambda i b) = return (Lambda i b)
```

Putting the whole thing together with evaluation of remaining terms gives us a substituting interpreter for `FBAE`:

```haskell
evalS :: FBAE -> (Maybe FBAE)
evalS (Num x) = (Just (Num x))
evalS (Plus l r) = do { (Num l') <- (evalS l) ;
                        (Num r') <- (evalS r) ;
                        return (Num (l' + r')) }
evalS (Minus l r) = do { (Num l') <- (evalS l) ;
                         (Num r') <- (evalS r) ;
                         return (Num (l' - r')) }
evalS (Bind i v b) = do { v' <- evalS v ;
                          (evalS (subst i v' b)) }
evalS (Lambda i b) = return (Lambda i b)
evalS (App f a) = do { (Lambda i b) <- (evalS f) ;
                       a' <- (evalS a) ;
                       evalS (subst i a' b) }
evalS (If c t e) = do { (Num c') <- (evalS c) ;
                        if c'==0 then (evalS t) else (evalS e) }
evalS (Id id) = Nothing
```

## Efficiency

The new interpreter for functions has the same problem as our original interpreter for `bind`.  Substitutions are performed immediately throughout an expression.  Thankfully, we can use an environment again to solve the same problem.  Recall how we used the environment variable to pass a list of defined variables and their values to implement `bind`:

```haskell
evalM env (Bind i v b) = do { v' <- evalM env v ;
                              evalM ((i,v'):env) b }
```

We can do a similar thing with function application.  In the same way we replaced the call to `subst` with an environment update in `bind`, we can literally do the same thing with function application:

```haskell
evalM env (App f a) = do { (Lambda i b) <- (evalM env f) ;
                           a' <- (evalM env a) ;
                           evalM ((i,a'):env) b }
```

The identifier and its value are added to the environment before evaluating the function body.  Whenever the identifier appears, the value is found in the environment and used.  Virtually the same implementation as bind.

The complete code for the interpreter implementing an environment is:

```haskell
type Env = [(String,FBAE)]

evalM :: Env -> FBAE -> (Maybe FBAE)
evalM env (Num x) = return (Num x)
evalM env (Plus l r) = do { (Num l') <- (evalM env l) ;
                            (Num r') <- (evalM env r) ;
                            return (Num (l'+r')) }
evalM env (Minus l r) = do { (Num l') <- (evalM env l) ;
                             (Num r') <- (evalM env r) ;
                             return (Num (l'-r')) }
evalM env (Bind i v b) = do { v' <- evalM env v ;
                              evalM ((i,v'):env) b }
evalM env (Lambda i b) = return (Lambda i b)
evalM env (App f a) = do { (Lambda i b) <- (evalM env f) ;
                           a' <- (evalM env a) ;
                           evalM ((i,a'):env) b }
evalM env (Id id) = do { v <- (lookup id env) ;
                         return v }
evalM env (If c t e) = do { (Num c') <- (evalM env c) ;
                            if c'==0 then (evalM env t) else (evalM env e) }
```

## Testing

We now have two interpreters for what we hope is the same untyped system with functions.  We can use QuickCheck to help our testing, but we have immediate problems that can easily be seen.

### Function Values

Whether you realize it or not, we have introduced a new kind of value to our interpreter.  Clearly when we added Booleans to our first interpreter, there were new values to deal with. It's less obvious how we have new values in this language, but remember that `lambda` creates a function value.  `lambda` expressions are values just like `1` or `true`.  What this means is expression like this one:

```haskell
bind f = (lambda x in x+1) in
  app f f
```

will crash.  `f` can take any argument, but its expression `x+1` will only operate on numbers.

### Scoping

Let's consider the following code snippet:

```haskell
bind f = (lambda x in x + n) in
  app f 1
```

The `bind` defines `f` whose value is the function that returns its input plus `n`.  This looks quite a bit like `inc`, but instead adds `n` to the argument rather than `1`.  Where does `n` come from?  In this case, nowhere.  Both `evalS` and `eval` throw an error when they cannot find the value of `n`.  Let's now add an `n` with `f` in its scope:

```haskell
bind n = 1 in
  bind f = (lambda x in x + n) in
	app f 1
```

The outer `bind` gives `n` the value `1`  while the inner `bind` gives `f` the lambda value that returns its input argument plus `n`.  We just defined `f` in the context of `n`.  When we evaluate the new `bind` bith `evalS` and `eval` give us the value `2` as expected.  So far so good.

Lets take this one step further by defining multiple, nested values of `n` by making the body of `f` a `bind` expression like this:

```haskell
bind n = 1 in
  bind f = (lambda x in x + n) in
    bind n = 2 in
      app f 1
```

What do you expect to get when evaluating this expression?  `evalS` gives us the value `2`.  However, `eval` gives us the value `3`. What's happening here?  Whatever it is, its bad because our two interpreters for the same language are now giving different results with `evalS` and `eval`.  Same language using deferred substitution rather than immediate substitution giving different values.

## Discussion

We have successfully implemented an untyped function system by adding to our original `BAE` interpreter.  We demonstrated that we can implement direct substitution and deferred substitution just as we did with `bind` expressions.  However, we've run into two problems associated with new function values and an odd difference in the way `evalS` and `eval` process expressions.  As you might guess, we will deal with the new function values by predicting failure using types. We will deal with the difference between immediate and deferred substitution by looking at _static_ and _dynamic_ scoping.

## Exercises
1. Write an `eval` function for a language with only first-class functions using direct substitutions and the `subst` operation defined for `bind`
2. Modify your `eval` function for a language with only first-class functions to defer substitution using an environment that contains both functions and values.

{% comment %}
## Notes
* Reference for the lambda calculus
{% endcomment %}
