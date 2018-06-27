---
layout: frontpage
title: Sequencing
use_math: true
categories: chapter ch2
---

# Sequencing

## Statements

Each of the terms defined thus far returns some value computed during their exeution.  Constants return values:

```haskell
eval 5
== 5
```

Mathematical expressions calculate and return values:

```text
eval 5+2
== 7
```

```text
eval 5<=3
== false
```

`lambda` and `app` both return values;

```text
lambda x:nat in x+1
== lambda x:nat in x+1
```

```text
(app (lambda x:nat in x+1) 4)
== 5
```

Even `bind` that creates and uses local bindings returns a value upon execution:

```text
bind x=4 in x + 1
== 5
```

They all return values upon evaluation.  Because of this property, we call these terms _expressions_.  Given a well-formed expression, evaluation always results in a value.  For example, we studied the `if` expression and include it in all our languages.  Given three terms, evaluting the first determines whether the second or third should be evaluated and returned.  The `if` expression always returns a value.

_Statements_ are programming language constructions that order execution rather than calculate values.  Statements are structures such as loops, `if` statements, assignment and sequence.  Unlike expressions, none of these terms must return values.  The `if` statement does not return a value, but instead chooses and executes a nested statement.  The assignment statement alters a variable value, but does not itself produce a value.  Sequential execution of two statements does not result in a value.

## The Unit Type

So far we have introduced three types - `nat`, `boolean`, and function types of the form `T->T`.  Here we introduce the most trivial of types, the _Unit_.  The unit type is named `unit` and nas a single value, `u`.  Unit has no associated operations and no properties other than having one and only one value.  Because `u` is a value, it has no associated evaluation rule.  Because it is a constant value, it's typing rule is trivial:

```text
u:unit
```

`unit` has a number of special properties that we'll skip for now.  All we need is a trival type that conveys no infomration.

## Sequence Statement

Everyone who has gotten this far should know what sequential execution means.  It happens so often in our code that we cease to think about.

```text
seq t1 t2 == 
```

## Sequence Derived Forms

```text
seq t1 t2 == (app (lambda x:U t2) t1)
```
