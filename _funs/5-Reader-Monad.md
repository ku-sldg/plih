---
layout: frontpage
title: Reader Monad
use_math: true
categories: chapter
---

# Reader Monad

We saw how the `Maybe` monad captures a computational feature that implements a kind of simple exception handling.  The `Reader` monad similarly captures a computational feature that threads a read-only environment through a computation.  Recall the signature of the current `eval` function for `FBAE`: 

```haskell
evalM :: Env -> FBAE -> (Maybe FBAE)
```

where `Env` is defined as a list of string, value pairs:

```haskell
type Env = [ (string,FBAEValue) ]
```

When using `evalM`, an environment is explicitly passed as an argument.  The environment is updated by `bind` and `app`, then passed to subsequent calls to `evalM`.  Even when a term does not require an environment for evaluation, it still appears in the `evalM` signature.  While this works fine, we typically don't pass environments to interpreters to evaluation functions.  For example, one does not pass an environment to the Haskell or Racket evaluator. Instead, the environment exists as an _ephemeral data structure_ the interpreter is aware of implicitly.   It is there in the background, ready to be used when needed.

The `Reader` monad gives us exactly this capability for maintaining an environment during evaluation.  Our next interpreter will use the `Reader` to manage the environment during execution, making it available without explicitly passing it around as a parameter.  It will be there when needed and disappear from most terms in the `evalM` definition.

The `Reader` is a datatype with a single constructor whose argument is a function from environment to value:

```haskell
data Reader e a = Reader (e -> a)
```

Recall that`Maybe` is also a data type with two constructors.  As `Just` is parameterized over the the type it encasulates, `Reader` is parameterized over its environment type, `e` and value type `a`.  `Reader` can use anything as an environment and return value, but we will use `Env` and `FBAEValue`.

`Maybe` captures the difference between a value and a failed computation.  `evalM` uses `bind` and `return` to manage evaluation results that represent successful and failed computations. `Reader` values represent _computations_.  The value encapsulated by `Reader` is a function that takes an  environment and produces a value.  It does not input an expression, but instead represents a computation that evaluates a specific expression.  This is important.  `Reader` values represent computations that have not yet been performed.

`runR` is an auxiliary function that performs the computation a `Reader` represents.  `runR` is a rather trivial function that pulls the function representing a computation out of the `Reader` encapsulation and executes it on a specific environment.  For this reason `runR` is frequently read as “run reader”.  The definition of `runR` is really quite simple:

```haskell
runR :: Reader e a -> e -> a
runR (Reader f) e = f e
```

Given some `R` of type `Reader`, `runR e` extracts the function representing its computation and applies it to a specific environment resulting in a value.  We use the `Reader` by constructing a`Reader` instance then extracting and executing its computation using `runR` and an environment. 

So far this looks nothing like `Maybe` in either form or function.  If it is a monad, then we know `return` and `bind` must be defined.  To understand `Reader`, let's look at the `Monad` instance for `Reader` and see how `return` and`bind` are implemented.  Following is the `Monad` instance for `Reader`: 

```haskell
instance Monad (Reader e) where
  return x = Reader $ \e -> x
  g >>= f = Reader $ \e -> runR (f (runR g e)) e
```

There is a bit of Haskell-foo in this definition that needs explanation.  The shorthand `f $ v` is the same as `(f v)`.  It gets used quite often to reduce the number of parenthesis to parse mentally.  The following two expressions result in the same value:

```haskell
Reader $ \e -> x
(Reader \e -> x)
```

The notation `\n -> n + 1` is an example of Haskell’s anonymous function definition mechanism.  Evaluating `\n -> n + 1` results in the increment function.  The following two definitions result in the same definition of `Inc`:

```haskell
inc x = x + 1
inc = \x -> x + 1
```

Now we’re set to go.

The `return` function creates trivial comutations that simply return values.  Looking at its definition, `return` takes a value, `x`, creates a function from an environment, `e` to that value, and wraps it up in the `Reader` constructor.  This
function returns the argument to `return` regardless of the input
environment.  `return` creates a constant function from an environment to a specific value.  Let's see it at work with `runR`: 

```haskell
runR (return 5) []
== runR (Reader \e -> 5) []
== (\e -> 5) []
== 5
```

`return 5` creates the `Reader` value `(Reader \e -> 5)`.  `runR`
extracts  `\e -> 5` and applies it `[]` resulting in `5`. A second example shows the same result with an alternative environment: 

```haskell
runR (return 5) [6,7,8]
== runR (Reader \e -> 5) [6,7,8]
== (\e -> 5) [6,7,8]
== 5
```

In fact, running `(return 5)` will always result in `5` regardless of its input value.  `return` encapsulates the most trivial computation - returning a constant value.  Foreshadowing, the `return` will be used at the end of strings of computations.

In `Maybe`, `bind` passed through a `Nothing` value and performed a specified operation on a `Just` value.  In `Reader`, `bind` will always perform a computation and pass the result to a subsequent computation. It is, in effect, a sequence operator.  For reference, the type from the `Monad` class  and the `bind` instance for `Reader` are: 

```haskell
  g >>= f :: M a -> (a -> M b) -> M b
  g >>= f = Reader $ \e -> runR (f (runR g e)) e
```

It’s quite simple to think that `g >>= f` runs `f` and uses its output as an input for running `g`.  Looking at the inside of the definition we even see a term:

```haskell
runR (f (runR g e)) e
```

that looks exactly like what we want.

Unfortunately, this is wrong.  Remember, `g >>= f` returns _a monad_ not a value.  In the case of `Reader` the monad encapsulates a function that will be run later with an environment.  `g >>= f` needs to create _a function_ and encapsulate that function in `Reader`for `runR` to use later.  Looking more carefully, the actual argument to the `Reader` constructor is the function:

```haskell
\e -> runR (f (runR g e)) e
```

`runR` is not executed when the `Reader` value is created, but deferred until the function is evaluated with some `e` as input.  Note also that both `f` and `g` are evaluated with `e` as their environment argument.  The same argument for both that is input when `runR` evaluates the monad.  In fact, we can bind as many functions together as we want and the same `e` will always be the environment argument and will never vary.  Thus the name `Reader`.  The environment allows passing in data to `runR`, but is constant over the entire `Reader` evaluation.

Let's look at at some simple examples. First, let's start with a computation than returns `5` and bind it to a computation that adds `1`.  The computation that returns `5` is a simple application of `return`.  Remember, return simply creates a `Reader` value that returns a constant value for any environment input:

```haskell
(return 5)
```

Now let's create a computation that adds `1` to the result of the previous computation.  Remember the signature for `f` is `f::a -> M b`.  `f` will take a value and return a monad that uses that value in its computation.  At first glance, a function that returns its argument plus 1 is simply `\x -> x+1`.  But we need the function to return a monad, not a value.  Our friend `return` will help us out here:

```haskell
\x -> (return (x + 1))
```

There is an important concept working here.  The computation resulting in `x` sets up the computation that follows.  Evaluating `\x -> (return (x + 1))` will replace `x` with a value in the computation created by `return`.  Instead of calculating, this function creates a computation using the result of the previous computation.  The `Reader` builds up a computation then `runR` performs that computation.

Our monad now becomes:

```haskell
((return 5) >>= (\x -> (return (x + 1))))
```

To evaluate our monad we simply call `runR` and specify an environment:

**Example under development**

```haskell
runR ((return 5) >>= (\x -> (return (x + 1)))) []
== runR (Reader $ \e -> runR ((\x -> (return (x + 1))) (runR (return 5) e)) e) []
== \e -> runR ((\x -> (return (x + 1))) (runR (return 5) e)) e) []
== runR ((\x -> (return (x + 1))) (runR (return 5) [])) [])
== runR ((\x -> (return (x + 1))) 5) []
== runR (return (5 + 1)) []
== 6
```

`g` is `return 5`, the computation that simple returns `5`.  `f` is the function that takes a value and produces a `Reader` that returns the result of adding `1` to the input.  Looking at executing `>>=`, `runR` runs `(return 5)` resulting in `5`. It then applies `f` to `5` resulting in a `Reader` that simply returns `6`.  Finally, `runR` evaluates `(return 6)` resulting in `6`. 

Because the result of `runR` is a number, we can add other operations to the sequence: 

```haskell
runR ((return 5)
      >>= (\x -> (return (x + 1)))
      >>= (\x -> (return (x - 3)))
      >>= (\x -> (return (x `mod` 2)))) []
== 1
```

Now we can sequence operations.  But given nothing else, the environment is constant across all the `runR` executions.  Worse yet, there's no way to use or modify it.  Let's look at how to look at and change the environment locally.  `ask` simply returns the environment: 

```haskell
ask :: Reader a a
ask = Reader $ \e -> e
```

It's important to realize that `ask` is not a function, but a `Reader` instance.  Let's run it and see what it does: 

```haskell
runR ask 5
== 5
```

`ask` returns the environment.  If the result of `ask` is the environment value, then `ask >>= f` should use the environment as the input to `f`: 

```haskell
runR (ask >>= (\x -> (return (x+1)))) 5
== 5
```

That's exactly what happens here.  The environment is used in subsequent calculations following `ask`. 

Similarly, `asks` will apply a function to the environment and return the result: 

```haskell
asks :: (e -> a) -> Reader e a
asks f = ask >>= \e -> (return (f e))
```

`asks` is not a `Reader`, but instead a function from environment to value to `Reader`.  It builds a `Reader` using `bind`.  For example, `asks (\x -> head x)` is an operation that takes the first element of an environment an returns it.  Assuming of course, the environment is a list.  Let's try it out: 

```haskell
runR ((asks (\e -> head e)) >>= (\x -> (return x))) [4,5,6]
== 4
```

Here `asks` runs and pulls the first element off the environment list. The result is passed to a simple function that returns its input. `asks` applies a function to the environment and `return` produces it as output.  We can now ignore the environment, return the environment, and apply a function to the environment. 

`local` makes things interesting and starts showing off the `Reader` at work by making changes to the local environment.  Like `asks`, local is a function that creates a `Reader` that can be used in a `bind` sequence: 

```haskell
local :: (e -> t) -> Reader t a -> Reader e a
local f r = ask >>= \e -> return (runR r (f e))
```

`local`'s two arguments are a function on the environment and a
`Reader`.  The function is applied to the environment in a similar manner as `asks`.  The `Reader` is a monad that will be run with the modified environment.  `r` is a `Reader` that will be run in the environment created by `f e`.  How does `local` do this? 

`local` first executes `ask` to get the environment.  The result is then passed as input to the second `bind` argument as `e`.  Remember, the second argument to `bind` is a function from an environment to a `Reader`.  Look carefully at the returned `Reader`.  `return` of course returns a value.  That value is obtained by running the `Reader` passed to `local` using `f e` as the environment.  The `Reader` passed to local as `t` is run _inside_ another `Reader` with a new environment.  Thus the name `local`.  When the nested `Reader` runs, the created environment is lost and the original environment restored.  This is key as it is exactly the behavior our environment exhibits. 

Take a step back and think about what we've done in a different way. All `Reader` instances are encapsulated computations wrapped up in a datatype.  `runR` executes those computations.  `return` encapsulates single, atomic computations.  `bind` sequences computations allowing results from prior computations to flow to later computations. `Reader` adds and environment, but all instances of `Monad` do roughly the same thing.  Encapsulate and sequence computations. 

## Reader and Evaluation

How can we use the `Reader` to implement an interpreter?  In earlier versions of interpreters with an environment, we passed the environment as an argument to `eval`.  Each expression is evaluated by recursive calls to `eval` on subterms.  This is long established. Evaluating some subterms cause changes to the environment, an issue not explored thoroughly. 

Let's start through the definition of `evalM`, a monadic evaluator for FBAE.  The signature is: 

```haskell
evalM :: FBAE -> Reader Env FBAE
```

The return result is a `Reader`.  Remember that to get a value from the `Reader` we must run `runR` on the result.  We'll define an `eval` function later that does just this. 

Thinking about expressions in `FBAE`, we can divide them up into two groups based on how they use the environment.  Specifically, there are three sets: 

1. No direct reference
2. Lookup environment entries
3. Locally modify entries

The first set includes returning constants and evaluating mathematical expressions.  None of them require accessing the environment directly: 

```haskell
evalM (Num n) = return (Num n)
evalM (Plus l r) = do { (Num l') <- (evalM l) ;
	                     (Num r') <- (evalM r) ;
                        return (Num l'+r') }
evalM (Minus l r) = do { (Num l') <- (evalM l) ;
                         (Num r') <- (evalM r) ;
                         return (Num l'-r') }
```

Evaluating `Id` requires accessing the environment to find the value of an identifier.  This is easily done using `ask` to get the environment and using a lookup function to find the needed environment record. 

```haskell
evalM (Id id) = do { env <- ask ;
                     return (case (lookupVar id env) of
                              Just x -> x
                              Nothing -> error "Variable not found") }
```

`lookupVar` returns a `Maybe`, thus we use a case statement to extract the return value or throw an error message.  For completeness, `lookupVar` is simply a call to `lookup` that treats its argument as a list of pairs:

```haskell
lookupVar :: String -> Env -> Maybe FBAE
lookupVar = lookup
```

The last two expressions require adding information to the
environment.  `local` does exactly what we need.  Evaluating both
`Bind` and `App` requires adding a variable binding to the
environment: 

```haskell
evalM (Bind i v b) = do { v' <- evalM v ;
                          local (addVar i v') (evalM b) }
evalM (Lambda i b) = return (Lambda i b)
evalM (App f v) = do { (Lambda i b) <- evalM f ;
                       v' <- evalM v ;
                       local (addVar i v') (evalM b) }
```

Both `Bind` and `App` use `local` exactly the same way.  The value associated with the added identifier is calculated first and use with the identifier to partially instantiate `addVar`.  When supplied with an environment, `addVar` will result in a new environment with the addition.  `evalM b` evaluates `b` in the context of the environment created by `addVar`. 

Again for completeness, the definition of `addVar` is:

```haskell
addVar :: String -> FBAE -> Env -> Env
addVar s i e = (s,i):e
```

Does this monadic interpreter implement static or dynamic scoping? How can you tell?  I'll give you a hint and say we'll look at a statically scoped interpreter next and forever leave dynamically scoped languages. 

## Reader and Evaluation (Redux)

To implement static scoping we add closures that record the
environment where a function is defined.  We've done this once already and will simply repeat the process hear using the `Reader`.  First, the abstract syntax: 

```haskell
data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> FBAETy -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  deriving (Show,Eq)
```

Nothing changed other than adding the argument type to `Lambda`.
We'll see if we need this for evaluation, but we will certainly need it for type checking.  Still the same language, just with that small addition. 

With types references from the abstract syntax, we need to include the datatype for `FBAETy`: 

```haskell
data FBAETy where
  TNum :: FBAETy
  TFun :: FBAETy -> FBAETy -> FBAETy
  deriving (Show,Eq)
```

For completeness again we include the type for values introduced
earlier for static scoping.  The important bit is the inclusion of closures for recording the static environment: 

```haskell
data FBAEVal where
  NumV :: Int -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)
```

Finally, the environment type:

```haskell
type Env = [(String,FBAEVal)]
```

Now we can define `evalM`, the monadic evaluator using the `Reader` monad.  `evalM` accepts an abstract syntax value and returns a `Reader` that we'll evaluate with `runR`.  The only change here is the `Reader` encapsulates a function of type `Env -> FBAEVal` rather than `Env -> FBAE` used previously: 

```haskell
evalM :: FBAE -> Reader Env FBAEVal
```

Now the interpreter.  Same song, second verse.  Everything is
identical until we get to the `Lambda`: 

```haskell
evalM (Num x) = return (NumV x)
evalM (Plus l r) = do { (NumV l') <- (evalM l) ;
                        (NumV r') <- (evalM r) ;
                        return (NumV (l'+r')) }
evalM (Minus l r) = do { (NumV l') <- (evalM l) ;
                         (NumV r') <- (evalM r) ;
                         return (NumV (l'-r')) }
evalM (Bind i v b) = do { v' <- evalM v ;
                          local (addVar i v') (evalM b) }
```

When evaluating `Lambda` we need to grave the environment when it is defined.  When `env` was a parameter, this was easy.  Using `ask` it still is.  We simply execute `ask` to return a copy of the environment and bind the result to `env`.  Then creating the closure is exactly as it was in the non-monadic statically scoped interpreter. 

```haskell
evalM (Lambda i b) = do { env <- ask ;
                        return (ClosureV i b env) }
```

The returned closure contains the environment obtained with `ask` when the `lambda` is evaluated.  Note that we are evaluating a `lambda`, not an application.  That comes next. 

Evaluating `App` is where the environment from the closure is actually used.  In effect, when we evaluate the app we need to start with the environment from the closure rather than the enviornment maintained by the `Reader` to that point.  We don't want to add to the enviroment. Instead we want to replace it. 

When evaluating the `App` we first evaluate `f` and `a` to get the closure and argument value.  For dynamic scoping we added a pair to the result of `ask` and replaced the environment with `local`.  We need to do the same thing again, but using the closure environment rather than the `Reader` environment.  We'll accomplish this by passing a new function to `local`.  Specifically: 

```haskell
useClosure :: String -> FBAEVal -> Env -> Env -> Env
useClosure i v e _ = (i,v):e
```

Look below how `useClosure` is used.  The first three arguments are instantiated with the identifier name, value and the environment from the closure.  The result is a function of type `Env -> Env`, exactly what `local` needs.  This particular function ignores that argument and produces a new environment using `e` from the closure: 

```haskell
evalM (App f a) = do { (ClosureV i b e) <- (evalM f) ;
                       a' <- (evalM a) ;
                       local (useClosure i a' e) (evalM b) }
```

Bingo.  `useClosure` creates a new environment by adding the new binding needed for evaluating `App` to the environment from the closure.  `local` plugs that in and we're now good go go. 

Now that we know how to build an environment from `app` and `bind`, it's time to evaluate identifiers by looking them up.  `ask` returns the environment that is in turn bound to `env`.  A `lookup` is performed to find `id` in `env`.  If it's there, return it.  If it's not, throw an error. 

```haskell
evalM (Id id) = do { env <- ask ;
                     case (lookup id env) of
                       Just x -> return x
                       Nothing -> error "Varible not found" }
```

It's useful to redefine `eval` using `evalM` so the interpreter
operates in the same way as previous interpreters: 

```haskell
eval x = runR (evalM x) []
```

Now we have a statically scoped interpreter for `FBAE` using a `Reader`.

## Reader and Type Inference

As you might have guessed, the `Reader` is also quite effective at type checking.  What is particularly interesting is the similarly between the type checker and evaluator. 

For completeness, the context type is defined as a list of string/type pairs:

```haskell
type Cont = [(String,FBAETy)]
```

```haskell
lookupVarTy = lookup
addVarTy :: String -> FBAETy -> Cont -> Cont
addVarTy s i e = (s,i):e
```

The signature for our new type inference function is roughly the same as the evaluator, except that we return a `Reader` that encapsulates types.  We will still need to use `runR` to evaluate the result of called `typeofM`: 

```haskell
typeofM :: FBAE -> Reader Cont FBAETy
```

The type of number constants is simply `TNum`.  Just return it:

```haskell
typeofM (Num n) = return TNum
```

The binary operations on numbers are identical modulo error messages.  Both find the types of their arguments and make sure both are numbers.  If they are, return `TNum` as the type of the operation.  If not, throw an error:

```haskell
typeofM (Plus l r) = do {
	l' <- (typeofM l) ;
    r' <- (typeofM r) ;
    return (if (l'==TNum && r'==TNum) then TNum else error "Type error in +") }
typeofM (Minus l r) = do {
  l' <- (typeofM l) ;
  r' <- (typeofM r) ;
  return (if (l'==TNum && r'==TNum) then TNum else error "Type error in -") }
```

`bind` adds bindings to the context when type checking.  `typeofM` uses the `Reader` to pass along the context rather than the environment, but the operations are almost identical.  `typeofM` for `bind` first uses `ask` to get the current context.  It calculates the type of the identifier being added, and then uses `local` in the same way as `evalM` to add the binding to the local context: 

```haskell
typeofM (Bind i v b) = do {
  con <- ask ;
  v' <- typeofM v ;
  local (addVarTy i v') (typeofM b) }
```

To perform static type checking, we need to use the `lambda` variant that carries a type for its argument.  `(i,t)` is added to the context and `typeofM b` used to get `r'`, the range type, that is the typeof the function body.  The type of the `Lambda` becomes `(TFun t r')`: 

```haskell
typeofM (Lambda i t b) = do {
  r' <- local (addVarTy i t) (typeofM b) ;
  return (TFun t r') }
```

The `App` case uses `typeofM` to get the type of the function and its argument.  The function type provides the domain and range of the associated function.  If the type of the argument is the domain type, then the `app` is the range type.  If they do not match, then `typeofM` throws an error.  The `if` expression is where all the work for this function is performed: 

```haskell
typeofM (App f v) = do }
  (TFun i b) <- typeofM f ;
  v' <- typeofM v ;
  return (if i==v' then b else error "Type Error in app") }
```

Finally finding the type of an identifier is simply looking it up on the context.  `ask` returns the context, a lookup is performed, and either a type is returned or an error message is thrown: 

```haskell
typeofM (Id id) = do
  ask >>= \env -> return (case (lookupVarTy id env) of
                            Just x -> x
                            Nothing -> error "Variable not found")
```

Like other functions, we can made the monadic version look like a
traditional version with a quick definition: 

```haskell
typeof x = runR (typeofM x) []
```

## Discussion

The `Reader` is an exceptionally powerful and useful programming
pattern.  Utility functions like `ask`, `asks`, and `local` are just few samples of what kinds of operations can be defined on the environment.  Even the function `useClosure` could be rewritten as a custom operation rather than using `local`: 

```haskell
explicit :: e -> Reader t a -> Reader e a
explicit e r = return (runR r e)
```

In our work thus far we have used our own `Reader`.  The standard
Haskell libraries contain a `Reader` implementation.  However, when learning how to use the `Reader` it is far better to have visibility into the implementation than simply try to use the `Reader` interface. Monad type signatures are not enough to understand their utility. 

It is worth spending time with a good Haskell tutorial and learning the `Reader` well. 

## Definitions

## Exercises
