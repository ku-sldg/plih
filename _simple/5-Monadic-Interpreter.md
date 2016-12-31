---
layout: frontpage
title: Monadic Interpreter
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
$$

# Monadic Interpreters

The `Either` type class used in both the interpreter and type inference routine for `ABE` is built in to Haskell.  We used `Either` as a construction for pairs using the convention that constructions with `Right` contain values and constructions with `Left` contain error messages.  This made it trivial to use a `case` expression to determine what kind of thing is returned by `eval` or `typeof`.

`Either` is also an instance of class `Monad`.  Our original `eval` definition does not take advantage of this, but we can write tighter functions by taking advantage of `Monad` properties.

## Either and bind

To understand how the `Either` monad is used, let's take a quick look at the monad instance for `Either`:

{% highlight haskell %}
instance Monad (Either e) where
        return = Right
        Right m >>= k = k m
        Left e  >>= _ = Left e
{% endhighlight %}

Recall that all monads define `return` and `>>=`, then infix representation for `bind`.  This is precisely what the above `instance` does.

## Monadic eval

{% highlight haskell %}
evalM :: ABE -> Either String ABE
evalM (Num x) = (Right (Num x))
evalM (Boolean x) = (Right (Boolean x))
evalM (Plus t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Num v1) -> case t2' of
                  (Num v2) -> (Right (Num (v1+v2)))
                  (Boolean _) -> (Left "Type Error in +")
    (Boolean _) -> (Left "Type Error in +")
evalM (Minus t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Num v1) -> case t2' of
                  (Num v2) -> (Right (Num (v1-v2)))
                  (Boolean _) -> (Left "Type Error in -")
    (Boolean _) -> (Left "Type Error in -")
evalM (And t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Boolean v1) -> case t2' of
                  (Boolean v2) -> (Right (Boolean (v1 && v2)))
                  (Num _) -> (Left "Type Error in &&")
    (Num _) -> (Left "Type Error in &&")
evalM (Leq t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Num v1) -> case t2' of
                  (Num v2) -> (Right (Boolean (v1 <= v2)))
                  (Boolean _) -> (Left "Type Error in <=")
    (Boolean _) -> (Left "Type Error in +")
evalM (IsZero t) = do
  t' <- (evalM t)
  case t' of
    (Num v) -> (Right (Boolean (v == 0)))
    (Boolean _) -> (Left "Type Error in isZero")
evalM (If t1 t2 t3) = do
  t1' <- (evalM t1)
  case t1' of
    (Num v) -> (Left "Type Error in if")
    (Boolean v) ->  if v then (evalM t2) else (evalM t3)

interpM = evalM . parseABE
{% endhighlight %}

## Monadic typeof

{% highlight haskell %}
testEvalM :: Int -> IO ()
testEvalM n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (evalM t)==(evalM t))
{% endhighlight %}

{% highlight haskell %}
typeofM :: ABE -> Either String TABE
typeofM (Num x) = (Right TNum)
typeofM (Plus l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if (l'==TNum && r'==TNum) then (Right TNum) else (Left "Type Error in +")
typeofM (Minus l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if (l'==TNum && r'==TNum) then (Right TNum) else (Left "Type Error in -")
typeofM (Boolean b) = (Right TBool)
typeofM (And l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if (l'==TBool && r'==TBool) then (Right TBool) else (Left "Type Error in &&")
typeofM (Leq l r) = do
  l' <- typeof l ;
  r' <- typeof r ;
  case l' of
    TNum -> case r' of
              TNum -> (Right TBool)
              _ -> (Left "Type mismatch in <=")
typeofM (IsZero v) = do
  v' <- (typeof v) ;
  if v' == TNum then (Right TBool) else Left "Type mismatch in IsZero"
typeofM (If c t e) = do
  c' <- (typeof c) ;
  t' <- (typeof t) ;
  e' <- (typeof e) ;
{% endhighlight %}
