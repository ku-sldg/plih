---
layout: frontpage
title: Typing Storage
use_math: true
categories: chapter ch2
---

# Typing Storage

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
