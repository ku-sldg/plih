---
layout: frontpage
title: Locations and Storage
use_math: true
categories: chapter ch2
---

# Locations and Storage

Define a new type called `TyLoc`.

Define a new type called `Sto`

```haskell
type Sto = location -> FBAEVal
```

Update `FBAEVal` to include `location`:

```haskell
data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> FBAETy -> FBAE -> Env -> FBAEVal
  LocV :: Int -> FBAEVal
```

$$\begin{align*}
t ::= & \; \mathsf{new}\; s\; t \\
  & \mid \mathsf{deref}\; s\; l \\
  & \mid \mathsf{set}\; s\; l\; t \\
l ::= & t \\
s ::= & \Sigma
\end{align*}$$

```text
new t
== l
```

`l` is a new location and the store is updated with `evalM t` stored at location `l`.

```text
deref l
== v
```

`l` is a location defined in the store and `v` is the value in the store at location `l`.

```text
set l t
== v
```

`l` is a location defined in the store, `l` is a loction, `v` is equal to `evalM t`, and the store is updated with `evalM t` in the store at location `l`.

Some example usage of our new operations.

Create a new location and store `5` in it.  Then dereference the new location:

```text
deref (new l 5) = 5
```
The result will be the stored value `5`.

Create a new location and store `5` in it.  Then set the new location to `6` and dereference:
```text
deref (set (new 5) 6) = 6
```

The result is the new value stored in the new location.

Let's now use `bind` to store a location and perform some operations on it.  First, let's create a location and stored `5`, but this time store the new location in `loc`.  With `loc` named in the enviornment, we can do various things with it.

In this example we dereference `loc`, add `1`, and set the location to the result and return what's in the location:

```text
bind loc = new 5 in
  (set loc (deref loc) + 1) ; deref loc
```

This is a clever implementation of `loc := loc + 1`.  Because the store is emphemeral, it is updated in subsequent statements.  `deref loc` returns the new value.

`new :: Sto -> FBAEVal -> FBAEVal`
`deref :: Sto -> FBAEVal -> FBAEVal`
`set :: Sto -> FBAEVal -> FBAEVal -> FBAEVal`
