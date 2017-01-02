---
layout: frontpage
title: Reader Monad
use_math: true
categories: chapter
---

# Reader Monad

We saw how the `Either` monad captures a computational feature that captures threading messages through an expression evaluation.  The  `Reader` monad similarly captures a feature that threads a read-only environment through a computation.  Recall the signature of the current `eval` function for `FBAE`:

{% highlight haskell %}
eval :: Env -> FBAE -> FBAE
{% endhighlight %}

where an environment is explicitly passed as an argument.  While this works fine, we typically don't pass environments to interpreters to pragmatic evaluation functions.  The environment exists as an ephemeral data structure the interpreter is aware of implicitly.  The `Reader` gives us exactly this capability for maintaining an environment during evaluation.

The `Reader` is a datatype with a single constructor whose argument is a function from environment to value.

{% highlight haskell %}
data Reader e a = Reader (e -> a)
{% endhighlight %}

`runR` pulls the function out of the monad encapsulation and executes it on a specific environment:

runR :: Reader e a -> e -> a
runR (Reader f) e = f e

Given some `R` of type `Reader`, `runR e` extracts the function and applies it to a specific environment, `e`.  We'll construct the `Reader` instance that encapsulates a function, then extract and execute the function.

So far, this looks nothing like `Either` in form or function.  Let's look at the `Monad` instance for `Reader` and see how `return` and `bind` are implemented.  Following is the `Monad` instance for `Reader`:

{% highlight haskell %}
instance Monad (Reader e) where
  return x = Reader $ \e -> x
  g >>= f = Reader $ \e -> runR (f (runR g e)) e
{% endhighlight %}

The `return` function takes a value, creates a function from an environment, and wraps it up in the `Reader` constructor.  The function returns the argument to `return` regardless of the input environment.  In effect, `return` creates a constant function from environment to value.  Let's see it at work with `runR`:

{% highlight haskell %}
runR (return 5) []
== 5
{% endhighlight %}

`return 5` creates the `Reader` value `(Reader \e -> 5)`.  `runR` extracts  `\e -> 5` and applies it to the second argument, `[]`, resulting in `5`. A second example shows the same result with an alternative environment:

{% highlight haskell %}
runR (return 5) [6,7,8]
== 5
{% endhighlight %}

`return` encapsulates a the simplest computation - returning a constant value.

In `Either`, `>>=` passed through a `Left` value and performed a specified operation on a `Right` value.  In `Reader`, `>>=` will perform a computation and pass the result to a subsequent computation.  It is, in effect, a sequence operator.  For reference, the type from the `Monad` class  and the `bind` instance for `Reader` are:

{% highlight haskell %}
  g >>= f :: M a -> (a -> M b) -> M b
  g >>= f = Reader $ \e -> runR (f (runR g e)) e
{% endhighlight %}

Before diving into `bind` in depth, look carefully at executing the function it creates from the inside out.  If we think of `runR` as `eval`, we first evaluate `g` with the result used as input to `f`.  At its essence, `Reader` sequences the execution of `g` and `f`. The role of `e` is as an environment for both.  We can define arbitrary numbers of functions, sequence them, and provide an environment.

Let's look at a simple example of adding 1 to an input value:

{% highlight haskell %}
runR ((return 5) >>= (\x -> (return (x + 1)))) []
== 6
{% endhighlight %}

`g` is `return 5`, the `Reader` that simple returns `5`.  `f` is the function that takes a value and produces a `Reader` that returns the result of adding `1` to the input.  Looking at executing `>>=`, `runR` runs `(return 5)` resulting in `5`. It then applies `f` to `5` resulting in a `Reader` that simply returns `6`.  Finally, `runR` evaluates `(return 6)` resulting in `6`.

Because the result of `runR` is a number, we can add other operations to the sequence:

{% highlight haskell %}
runR ((return 5)
      >>= (\x -> (return (x + 1)))
      >>= (\x -> (return (x - 3)))
      >>= (\x -> (return (x `mod` 2)))) []
== 1

Now we can sequence operations.  But given nothing else, the environment is constant across all the `runR` executions.  Worse yet, there's no way to use or modify it.  Let's look at how to look at and change the environment locally.  `ask` simply returns the environment:

{% highlight haskell %}
ask :: Reader a a
ask = Reader $ \e -> e
{% endhighlight %}

It's important to realize that `ask` is not a function, but a `Reader` instance.  Let's run it and see what it does:

{% highlight haskell %}
runR ask 5
== 5
{% endhighlight %}

`ask` returns the environment.  If the result of `ask` is the environment value, then `ask >>= f` should use the environment as the input to `f`:

{% highlight haskell %}
runR (ask >>= (\x -> (return (x+1)))) 5
== 5
{% endhighlight %}

That's exactly what happens here.  The environment is used in subsequent calculations following `ask`.

Similarly, `asks` will apply a function to the environment and return the result:

{% highlight haskell %}
asks :: (e -> a) -> Reader e a
asks f = ask >>= \e -> (return (f e))
{% endhighlight %}

`asks` is not a `Reader`, but instead a function from environment to value to `Reader`.  It builds a `Reader` using `bind`.  For example, `asks (\x -> head x)` is an operation that takes the first element of an environment an returns it.  Assuming of course, the environment is a list.  Let's try it out:

{% highlight haskell %}
runR ((asks (\e -> head e)) >>= (\x -> (return x))) [4,5,6]
== 4
{% endhighlight %}

Here `asks` runs and pulls the first element off the environment list.  The result is passed to a simple function that returns its input.  `asks` applies a function to the environment and `return` produces it as output.  We can now ignore the environment, return the environment, and apply a function to the environment.

`local` makes things interesting and starts showing off the `Reader` at work by making changes to the local environment.  Like `asks`, local is a function that creates a `Reader` that can be used in a `bind` sequence:

{% highlight haskell %}
local :: (e -> t) -> Reader t a -> Reader e a
local f r = ask >>= \e -> return (runR r (f e))
{% endhighlight %}

`local`'s two arguments are a function on the environment and a `Reader`.  The function is applied to the environment in a similar manner as `asks`.  The `Reader` is a monad that will be run with the modified environment.  `r` is a `Reader` that will be run in the environment created by `f e`.  How does `local` do this?

`local` first executes `ask` to get the environment.  The result is then passed as input to the second `bind` argument as `e`.  Remember, the second argument to `bind` is a function from an environment to a `Reader`.  Look carefully at the returned `Reader`.  `return` of course returns a value.  That value is obtained by running the `Reader` passed to `local` using `f e` as the environment.  The `Reader` passed to local as `t` is run _inside_ another `Reader` with a new environment.  Thus the name `local`.  When the nested `Reader` runs, the created environment is lost and the original environment restored.  This is key as it is exactly the behavior our environment exhibits.

Take a step back and think about what we've done in a different way.  All `Reader` instances are encapsulated computations wrapped up in a datatype.  `runR` executes those computations.  `return` encapsulates single, atomic computations.  `bind` sequences computations allowing results from prior computations to flow to later computations.  `Reader` adds and environment, but all instances of `Monad` do roughly the same thing.  Encapsulate and sequence computations.

## Reader and Evaluation

How can we use the `Reader` to implement an interpreter?  In earlier versions of interpreters with an environment, we passed the environment as an argument to `eval`.  Each expression is evaluated by recursive calls to `eval` on subterms.  This is long established.  Evaluating some subterms cause changes to the environment, an issue not explored thoroughly.

Let's start through the definition of `evalM`, a monadic evaluator for FBAE.  The signature is:

{% highlight haskell %}
evalM :: FBAE -> Reader Env FBAE
{% endhighlight %}

The return result is a `Reader`.  Remember that to get a value from the `Reader` we must run `runR` on the result.  We'll define an `eval` function later that does just this.

Thinking about expressions in `FBAE`, we can divide them up into two groups based on how they use the environment.  Specifically, there are three sets:

1. No direct reference
2. Lookup environment entries
3. Locally modify entries

The first set includes returning constants and evaluating mathematical expressions.  None of them require accessing the environment directly:

{% highlight haskell %}
evalM (Num n) = return (Num n)
evalM (Plus l r) = do
  l' <- (evalM l)
  r' <- (evalM r)
  return (numPlus l' r')
evalM (Minus l r) = do
  l' <- (evalM l)
  r' <- (evalM r)
  return (numMinus l' r')
{% endhighlight %}

A couple of utility functions are useful for lifting addition and subtraction into the `FBAE` type:

{% highlight haskell %}
liftNum :: (Int -> Int -> Int) -> FBAE -> FBAE -> FBAE
liftNum f (Num t1) (Num t2) = (Num (f t1 t2))

numPlus = liftNum (+)
numMinus = liftNum (-)
{% endhighlight %}

Evaluating `Id` requires accessing the environment to find the value of an identifier.  This is easily done using `ask` to get the environment and using a lookup function to find the needed environment record.

{% highlight haskell %}
evalM (Id id) = do
  env <- ask
  return (case (lookupVar id env) of
            Just x -> x
            Nothing -> error "Variable not found")
{% endhighlight %}

`lookupVar` returns a `Maybe`, thus we use a case statement to extract the return value or throw an error message.  For completeness, `lookupVar` is simply a call to `lookup` that treats its argument as a list of pairs:

{% highlight haskell %}
lookupVar :: String -> Env -> Maybe FBAE
lookupVar = lookup
{% endhighlight %}

The last two expressions require adding information to the environment.  `local` does exactly what we need.  Evaluating both `Bind` and `App` requires adding a variable binding to the environment:

{% highlight haskell %}
evalM (Bind i v b) = do
  v' <- evalM v
  local (addVar i v') (evalM b)
evalM (Lambda i b) = return (Lambda i b)
evalM (App f v) = do
  (Lambda i b) <- evalM f
  v' <- evalM v
  local (addVar i v') (evalM b)
{% endhighlight %}

Both `Bind` and `App` use `local` exactly the same way.  The value associated with the added identifier is calculated first and use with the identifier to partially instantiate `addVar`.  When supplied with an environment, `addVar` will result in a new environment with the addition.  `evalM b` evaluates `b` in the context of the environment created by `addVar`.

Again for completeness, the definition of `addVar` is:

{% highlight haskell %}
addVar :: String -> FBAE -> Env -> Env
addVar s i e = (s,i):e
{% endhighlight %}

