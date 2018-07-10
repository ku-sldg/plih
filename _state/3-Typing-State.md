---
layout: frontpage
title: Typing Storage
use_math: true
categories: chapter ch2
---

# Typing Storage

Let’s start thinking about type inference with mutable state by going back to type inference for the BFAE 

In `evalM` from the previous chapter we included types in `lambda` defintions but igored them after creating the AST.  Let’s go back and think about types and how we might statically predict the type of an FBAE expression.

First let’s take care of locations.  We added a location value to FBAE and now much have a type for those locations.  The simplest resonse is to add a new type, `TLoc`, to the set of type values.  The resulting data type now has the following form:

```haskell
data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  TLoc :: TFBAE
  deriving (Show,Eq)
```

As seen previously, existing type rules do not change.  Arithmetic operations still require natural numbers, function application requires its first argument to be a fucntion and the actual parameter type to match the function’s domain type, and Boolean operations require Boolean operands.  For now let’s skip those operations and look at typing rules for the new storage operators and sequence.  First let’s look at `new`:

```haskell
typeofM cont (New t) = do { (typeofM cont t) ;
                            return TLoc }
```

Recall that `new` always returns a fresh location.  Because all locations are of type `TLoc`, `new t` is always of type `TLoc` regardless of input.  The type of `t` is still checked to ensure `t` is well-typed, but it is always discarded and `TLoc` returned.

`set l v` expects `l` to be a location and `v` be an expression defining the stored value.  Recall that `set` returns the value after updating location `l` in memory.  If `l` is a location, then the type of `set l v` is the type of `v`:

```haskell
typeofM cont (Set l v) = do { TLoc <- (typeofM cont l) ;
															(typeofM cont v) }
```
  
Once again we don’t care about the type of `l` as long as it is `TLoc`.  Pattern matching in the bind causes a failure if `(typeofM cont l)` is not a location.

So far so good.

`deref l` expects `l` to be a location and returns the thing stored at location `l`.  Unfortunately, this presents a problem.  How do we know the type of the value at location `l`?  For example, in `(deref l) + 1` the first term must be a number.  All we know is that `l` is a location.  Nothing more.  The type of the value at `l` can only be known by performing the `deref` operation at run-time or by keeping track of what is stored at `l` each time `set l` occurs.

Maybe we can cheat and simply return `TLoc` to indicate a location is involved:

```haskell
typeofM cont (Deref l) = do { TLoc <- (typeofM cont l) ;
                              return TLoc}
```

This will cause the `(deref l) + 1` to fail type inference regardless of what is stored in `l` because `+` always expects a number and not a location.  Defaulting to a number or a boolean type causes the same error.

Lets try another tactic by introducing a new type, `TTop` with concrete syntax `Top`.  This new type will be returned whenever a `deref` operation should return a type:

```haskell
typeofM cont (Deref l) = do { TLoc <- (typeofM cont l) ;
                              return TTop }
```

`TTop` will have the special property that all other types are compatible with `TTop`.  Specifically, `TTop` can replace `TNum`, `TBool` or `T:->:T` in any typechecking situation.  Now our `(deref l)+1` is of type `TNum` as it should be, but it is always of type `TNum` regardless of what is stored at `l`.

What have we done?  First, our type inference system will no longer predict all static typing errors.  In that respect, we’ve broken our type system.  However, we still catch errors that are not associated with the contents of storage.

What we’ve described is very much like what languages like C do.  If you know C you should recall that `malloc` returns a pointer to a collection of bytes that are referenced by a _pointer_.  When dereferenced, the result is _caste_ to the needed type.  Said differently, the set of bytes allocated by `malloc` can be anything we caste them to.  You store a number and retrieve a function.  You store a function and retrieve a Boolean.  What I stored in memory becomes whatever you tell it to be.  While there are very good reasons to do this in system programming, for other tasks many errors are missed by bad castes or worse no explicit castes.

***

```haskell
typeofM cont (Seq l r) = do { (typeofM cont l) ;
                              (typeofM cont r) }
```

```haskell
data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  TLoc :: TFBAE -> TFBAE
  deriving (Show,Eq)
```

```haskell
typeofM :: Cont -> FBAE -> (Maybe TFBAE)
```

```haskell
typeofM cont (Num x) = return TNum
typeofM cont (Boolean b) = return TBool
```

```haskell
typeofM cont (Plus l r) = do { TNum <- (typeofM cont l) ;
                               TNum <- (typeofM cont r) ;
                               return TNum }
typeofM cont (Minus l r) = do { TNum <- (typeofM cont l) ;
                                TNum <- (typeofM cont r) ;
                                return TNum }
typeofM cont (Mult l r) = do { TNum <- (typeofM cont l) ;
                               TNum <- (typeofM cont r) ;
                               return TNum }
typeofM cont (Div l r) = do { TNum <- (typeofM cont l) ;
                              TNum <- (typeofM cont r) ;
                              return TNum }
```

```haskell
typeofM cont (Bind i v b) = do { v' <- typeofM cont v ;
                                 typeofM ((i,v'):cont) b }
typeofM cont (Id id) = (lookup id cont)
typeofM cont (Lambda x t b) = do { tyB <- typeofM ((x,t):cont) b ;
                                   return (t :->: tyB) }
typeofM cont (App x y) = do { tyXd :->: tyXr <- typeofM cont x ;
                              tyY <- typeofM cont y ;
                              if tyXd==tyY
                              then return tyXr
                              else Nothing }
```

```haskell
typeofM cont (And l r) = do { TBool <- (typeofM cont l) ;
                              TBool <- (typeofM cont r) ;
                              return TBool }
typeofM cont (Or l r) = do { TBool <- (typeofM cont l) ;
                             TBool <- (typeofM cont r);
                             return TBool }
typeofM cont (Leq l r) = do { TNum <- (typeofM cont l) ;
                              TNum <- (typeofM cont r) ;
                              return TBool }
typeofM cont (IsZero v) = do { TNum <- (typeofM cont v) ;
                               return TBool }
typeofM cont (If c t e) = do { TBool <- (typeofM cont c) ;
                               t' <- (typeofM cont t) ;
                               e' <- (typeofM cont e) ;
                               if t' == e'
                               then return t'
                               else Nothing }
```

```haskell
typeofM cont (New t) = do { t' <- (typeofM cont t) ;
                            return (TLoc t') }
typeofM cont (Set l v) = do { (TLoc l') <- (typeofM cont l) ;
                              v' <- (typeofM cont v) ;
                              if l'==v'
                              then return v'
                              else Nothing }
typeofM cont (Deref l) = do { (TLoc l') <- (typeofM cont l) ;
                              return l'}
typeofM cont (Seq l r) = do { (typeofM cont l) ;
                              (typeofM cont r) }
```


## Discussion

## Definitions

## Exercises
