---
layout: frontpage
title: More Reader Monad
use_math: true
categories: chapter
---

## Reader and Type Inference

As you might have guessed, the `Reader` is also quite effective at
type checking.  What is particularly interesting is the similarly
between the type checker and evaluator. 

For completeness, the context type is defined as a list of string/type pairs:

{% highlight haskell %}
type Cont = [(String,FBAETy)]
{% endhighlight %}

{% highlight haskell %}
lookupVarTy = lookup
addVarTy :: String -> FBAETy -> Cont -> Cont
addVarTy s i e = (s,i):e
{% endhighlight %}

The signature for our new type inference function is roughly the same as the evaluator, except that we return a `Reader` that encapsulates types.  We will still need to use `runR` to evaluate the result of called `typeofM`:

{% highlight haskell %}
typeofM :: FBAE -> Reader Cont FBAETy
{% endhighlight %}

The type of number constants is simply `TNum`.  Just return it:

{% highlight haskell %}
typeofM (Num n) = return TNum
{% endhighlight %}

The binary operations on numbers are identical modulo error messages.  Both find the types of their arguments and make sure both are numbers.  If they are, return `TNum` as the type of the operation.  If not, throw an error:

{% highlight haskell %}
typeofM (Plus l r) = do
  l' <- (typeofM l)
  r' <- (typeofM r)
  return (if (l'==TNum && r'==TNum) then TNum else error "Type error in +")
typeofM (Minus l r) = do
  l' <- (typeofM l)
  r' <- (typeofM r)
  return (if (l'==TNum && r'==TNum) then TNum else error "Type error in -")
{% endhighlight %}

`bind` adds bindings to the context when type checking.  `typeofM` uses the `Reader` to pass along the context rather than the environment, but the operations are almost identical.  `typeofM` for `bind` first uses `ask` to get the current context.  It calculates the type of the identifier being added, and then uses `local` in the same way as `evalM` to add the binding to the local context:

{% highlight haskell %}
typeofM (Bind i v b) = do
  con <- ask
  v' <- typeofM v
  local (addVarTy i v') (typeofM b)
{% endhighlight %}

To perform static type checking, we need to use the `lambda` variant that carries a type for its argument.  `(i,t)` is added to the context and `typeofM b` used to get `r'`, the range type, that is the typeof the function body.  The type of the `Lambda` becomes `(TFun t r')`:

{% highlight haskell %}
typeofM (Lambda i t b) = do
  r' <- local (addVarTy i t) (typeofM b)
  return (TFun t r')
{% endhighlight %}

The `App` case uses `typeofM` to get the type of the function and its argument.  The function type provides the domain and range of the associated function.  If the type of the argument is the domain type, then the `app` is the range type.  If they do not match, then `typeofM` throws an error.  The `if` expression is where all the work for this function is performed:

{% highlight haskell %}
typeofM (App f v) = do
  (TFun i b) <- typeofM f
  v' <- typeofM v
  return (if i==v' then b else error "Type Error in app")
{% endhighlight %}

Finally finding the type of an identifier is simply looking it up on the context.  `ask` returns the context, a lookup is performed, and either a type is returned or an error message is thrown:

{% highlight haskell %}
typeofM (Id id) = do
  ask >>= \env -> return (case (lookupVarTy id env) of
                            Just x -> x
                            Nothing -> error "Variable not found")
{% endhighlight %}

Like other functions, we can made the monadic version look like a traditional version with a quick definition:

{% highlight haskell %}
typeof x = runR (typeofM x) []
{% endhighlight %}

## Discussion

The `Reader` is an exceptionally powerful and useful programming pattern.  Utility functions like `ask`, `asks`, and `local` are just  few samples of what kinds of operations can be defined on the environment.  Even the function `useClosure` could be rewritten as a  custom operation rather than using `local`:

{% highlight haskell %}
explicit :: e -> Reader t a -> Reader e a
explicit e r = return (runR r e)
{% endhighlight %}

In our work thus far we have used our own `Reader`.  The standard Haskell libraries contain a `Reader` implementation.  However, when learning how to use the `Reader` it is far better to have visibility into the implementation than simply try to use the `Reader` interface.  Monad type signatures are not enough to understand their utility.

It is worth spending time with a good Haskell tutorial and learning the `Reader` well.

## Definitions

## Exercises
