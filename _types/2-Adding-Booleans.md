---
layout: frontpage
title: Adding Booleans (Redux)
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

# Adding Booleans

Having added function types, let's revisit adding Boolean types back
into the mix.  Very little changes, thus we will review this
implementation as an exercise rather than work through it in great
detail.  What we will see is a more complete language developed by
combining type inference and interpretation.

The rest of this chapter will be posted following submission of
Project 3. 

{% comment %}

## AST

The abstract data type for our new language AST is as follows:

{% highlight haskell %}
data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  deriving (Show,Eq)
{% endhighlight %}

Note that functions now include a type for the input parameter necessary for type inference.

## Types

The abstract data type for the type elements of our new language is as follows:

{% highlight haskell %}
data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)
{% endhighlight %}

Nothing particularly new here other than adding the new constructor representing function types from the previous chapter.

## Evaluation

### Dynamic Scoping

{% highlight haskell %}
type Env = [(String,FBAE)]
type Cont = [(String,TFBAE)]
{% endhighlight %}

First a dynamically scoped interpreter for this new language FBBAE:

{% highlight haskell %}         
eDyn :: Env -> FBAE -> (Either String FBAE)
eDyn env (Num x) = (Right (Num x))
eDyn env (Plus l r) = do { (Num l') <- (eDyn env l) ;
                           (Num r') <- (eDyn env r) ;
                           return (Num (l'+r'))
                         }
eDyn env (Minus l r) = do { (Num l') <- (eDyn env l) ;
                            (Num r') <- (eDyn env r) ;
                            return (Num (l'-r')) }
eDyn env (Mult l r) = do { (Num l') <- (eDyn env l) ;
                           (Num r') <- (eDyn env r) ;
                           return (Num (l'*r')) }
eDyn env (Div l r) = do { (Num l') <- (eDyn env l) ;
                          (Num r') <- (eDyn env r) ;
                          return (Num (div l' r')) }
eDyn env (Bind i v b) = do { v' <- eDyn env v ;
                             eDyn ((i,v'):env) b }
eDyn env (Lambda i t b) = return (Lambda i t b)
eDyn env (App f a) = do { (Lambda i t b) <- (eDyn env f) ;
                          a' <- (eDyn env a) ;
                          eDyn ((i,a'):env) b }
eDyn env (Id id) = case (lookup id env) of
                     Just x -> (Right x)
                     Nothing -> (Left "Varible not found")
eDyn env (Boolean b) = return (Boolean b)
eDyn env (And l r) = do { (Boolean l') <- (eDyn env l) ;
                          (Boolean r') <- (eDyn env r) ;
                          return (Boolean (l' && r')) }
eDyn env (Or l r) = do { (Boolean l') <- (eDyn env l) ;
                         (Boolean r') <- (eDyn env r) ;
                         return (Boolean (l' || r')) }
eDyn env (Leq l r) = do { (Num l') <- (eDyn env l) ;
                         (Num r') <- (eDyn env r) ;
                         return (Boolean (l' <= r')) }
eDyn env (IsZero v) = do { (Num v') <- (eDyn env v) ;
                           return (Boolean (v' == 0)) }
eDyn env (If c t e) = do { (Boolean c') <- (eDyn env c) ; 
                            if c' then (eDyn env t) else (eDyn env e) }
{% endhighlight %}

### Static Scoping

{% highlight haskell %}
type EnvS = [(String,FBAEVal)]
type ContS = [(String,TFBAE)]
{% endhighlight %}

{% highlight haskell %}
data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> TFBAE -> FBAE -> EnvS -> FBAEVal
  deriving (Show,Eq)
{% endhighlight %}

{% highlight haskell %}
eSta :: EnvS -> FBAE -> (Either String FBAEVal)
eSta env (Num x) = (Right (NumV x))
eSta env (Plus l r) = do { (NumV l') <- (eSta env l) ;
                           (NumV r') <- (eSta env r) ;
                           return (NumV (l'+r')) }
eSta env (Minus l r) = do { (NumV l') <- (eSta env l) ;
                            (NumV r') <- (eSta env r) ;
                            return (NumV (l'-r')) }
eSta env (Mult l r) = do { (NumV l') <- (eSta env l) ;
                           (NumV r') <- (eSta env r) ;
                           return (NumV (l'*r')) }
eSta env (Div l r) = do { (NumV l') <- (eSta env l) ;
                          (NumV r') <- (eSta env r) ;
                          return (NumV (div l' r')) }
eSta env (Bind i v b) = do { v' <- eSta env v ;
                             eSta ((i,v'):env) b }
eSta env (Lambda i t b) = return (ClosureV i t b env)
eSta env (App f a) = do { (ClosureV i t b e) <- (eSta env f) ;
                          a' <- (eSta env a) ;
                          (eSta ((i,a'):e) b) }
eSta env (Id id) = case (lookup id env) of
                     Just x -> (Right x)
                     Nothing -> (Left "Varible not found")
eSta env (Boolean b) = return (BooleanV b)
eSta env (And l r) = do { (BooleanV l') <- (eSta env l) ;
                          (BooleanV r') <- (eSta env r) ;
                          return (BooleanV (l' && r')) }
eSta env (Or l r) = do { (BooleanV l') <- (eSta env l) ;
                         (BooleanV r') <- (eSta env r) ;
                         return (BooleanV (l' || r')) }
eSta env (Leq l r) = do { (NumV l') <- (eSta env l) ;
                          (NumV r') <- (eSta env r) ;
                          return (BooleanV (l' <= r')) }
eSta env (IsZero v) = do { (NumV v') <- (eSta env v) ;
                           return (BooleanV (v' == 0)) }
eSta env (If c t e) = do { (BooleanV c') <- (eSta env c) ;
                           (if c' then (eSta env t) else (eSta env e)) }
{% endhighlight %}

## Type Checking

{% highlight haskell %}
typeof :: Cont -> FBAE -> (Either String TFBAE)
typeof cont (Num x) = return TNum
typeof cont (Plus l r) = do { l' <- (typeof cont l) ;
                              r' <- (typeof cont r) ;
                              if l'==TNum && r'==TNum
                              then return TNum
                              else (Left "Type Mismatch in +")}
typeof cont (Minus l r) = do { l' <- (typeof cont l) ;
                               r' <- (typeof cont r) ;
                               if l'==TNum && r'==TNum
                               then return TNum else Left "Type Mismatch in -" }
typeof cont (Mult l r) = do { l' <- (typeof cont l) ;
                              r' <- (typeof cont r) ;
                              if l'==TNum && r'==TNum
                              then return TNum
                              else Left "Type Mismatch in *" }
typeof cont (Div l r) = do { l' <- (typeof cont l) ;
                             r' <- (typeof cont r) ;
                             if l'==TNum && r'==TNum
                             then return TNum
                             else Left "Type Mismatch in /" }
typeof cont (Bind i v b) = do { v' <- typeof cont v ;
                                typeof ((i,v'):cont) b }
typeof cont (Id id) = case (lookup id cont) of
                        Just x -> (Right x)
                        Nothing -> (Left "Varible not found")
typeof cont (Lambda x t b) = do { tyB <- typeof ((x,t):cont) b ;
                                  return (t :->: tyB) }
typeof cont (App x y) = do { tyXd :->: tyXr <- typeof cont x ;
                             tyY <- typeof cont y ;
                             if tyXd==tyY
                             then return tyXr
                             else Left "Type mismatch in app" }
typeof cont (Boolean b) = return TBool
typeof cont (And l r) = do { l' <- (typeof cont l) ;
                             r' <- (typeof cont r) ;
                             if l'== TBool && r' == TBool
                             then return TBool
                             else Left "Type mismatch in &&" }
typeof cont (Or l r) = do { l' <- (typeof cont l) ;
                            r' <- (typeof cont r) ;
                            if l' == TBool && r' == TBool
                            then return TBool
                            else Left "Type mismatch in ||" }
typeof cont (Leq l r) = do { l' <- (typeof cont l) ;
                             r' <- (typeof cont r) ;
                             if l'== TNum && r' == TNum
                             then return TBool
                             else Left "Type mismatch in <=" }
typeof cont (IsZero v) = do { v' <- (typeof cont v) ;
                              if v' == TNum
                              then return TBool
                              else Left "Type mismatch in IsZero" }
typeof cont (If c t e) = do { c' <- (typeof cont c) ;
                              t' <- (typeof cont t) ;
                              e' <- (typeof cont e) ;
                              if c' == TBool && t'==e'
                              then return t'
                              else (Left "Type mismatch in if") }
{% endhighlight %}

{% endcomment %}
