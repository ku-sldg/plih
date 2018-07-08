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

## Sequencing Statements

Executing statements sequentially is so common in imperative languages that we often overlook it as a paradigm.

The concrete syntax for sequential execution is two terms separated by a semicolon:

$$\begin{align*}
t ::= & t\;;\;t \\
\end{align*}$$

We've grown to view the semicolon as a line terminator, but in langauges like Pascal and formalisms like CSP it is an operator just like `while` or `if`.  The notation `t0 ; t1` indicates that term `t0` is executed immediately followed by `t1`.  Combining the sequence operation with our existing language allows us to create operator sequences such as:

```text
bind f = (lambda x:nat in x+1) in
  app f 1 ; app f 2
```
or

```text
lambds x:nat in x<=0 ; gtz x
```

Adding the abstract syntax term `seq` includes sequence in FBAE:

```text
Seq t1 t2
```

With syntax defined, lets think about what sequencing operations actually does.  As we've done in the past, we'll start by thinking about untyped sequencing and move to sequencing in a typed language.

### Sequencing Untyped Terms

Sequencing term execution is quite simple using  a `Maybe` or `Reader` monad. In fact, we saw the `Reader` at work sequencing execution for addition and subtraction.  Recall the interpreter statement for the addition term `(Plus l r)`:

```haskell
evalM (Plus l r) = do { (NumV l') <- (evalM l) ;
                        (NumV r') <- (evalM r) ;
                        return (NumV (l'+r')) }
```

`l` and `r` are evaluated in sequence, the results added together, and finally returned.  This is exactly what we want!  Except of course that we can't return the sum of evaluation results.

What should sequence return?  In this untyped case let's remember what Racket and Common Lisp do - return the last value calculated.  This would make our evaluation case for `Seq` the following:

```haskell
evalM (Seq l r) = do { evalM l ;
                       evalM r }
```

What happened to the bind symbols?  `l'` and `r'` are gone.  All this means is the values resulting from evaluating `l` and `r` are not bound to variables.  The result of `evalM l` is discarded while the result of `evalM r` is returned by the `do` form.  This is exactly what we want - evaluate `l`, evaluate `r` and return the result.

If we want to sequence more than two operations, such as `t0 ; t1 ; t2`, nesting the sequnce operation works quite nicely:

```haskell
(Seq t0 (Seq t1 t2))
```

and our monadic implementaiton serves us well.

By the way, which monad did we use in the implementation above?  Is it `Maybe` or `Reader`?

### Untyped Sequence Derived Form

Another mechanism for implementing sequence is to define it as a derived for using `app` and `lambda`.  If we want to execute `t1` followed by `t2` we can translated `(Seq t1 t2)` in the following manner:

```text
Seq l r == (app (lambda x in r) l)
```

Assuming call-by-value semantics, `app` evalutes the argument to its function, binds that value to the function's formal parameter, and evaluates the function's body.  Here the parameter is named `x` and `evalM l` gets bound to `x`.  As long as `x` does not appear free in `r`, this derived form works just fine.  However, `x` _must not_ be free in `r` or we will run into all kinds of type problems at runtime.  It is simple enough to check this property statically or we can use a wildcard parameter:

```text
Seq t1 t2 == (app (lambda _ in r) l)
```

Here `x` is replacec by the wildcard `_`.  This symbol is not a variable and is ignored by `app`.  No value is bound to it eliminating any messiness due to the result type of `evalM r`.

## Sequencing Typed Statements

What happens when we add static typing to sequencing?  As it turns out, not much and quite alot.  Let's look again at the impelementation of `Seq` evaluation:

```haskell
evalM (Seq l r) = do { evalM l ;
                       evalM r }
```
Because `l` does not interact with `r` during evaluation, the type resulting from `evalM l` makes no difference.  Furthermore, the type of a sequence operation is easily determined to be the type of `r`:

```haskell
typeofM (Seq l r) = typeofM r
```

Life is more intresting if we defined sequence using a derived form as we did earlier.  Recall the untyped definition:

```haskell
Seq l r == (app (lambda x in r) l)
```

and note that `x` has no type.  In a statically typed language the lambda's formal parmeter must have an ascribed type.  There are several ways around this, but the most common is to make the type of `x` the `unit` type described earlier:

```haskell
Seq l r == (app (lambda x:unit in r) l)
```

This forces `l` to return a `u` in every case and thus be of type `unit`.

This sounds restrictive, but it is true to the concept that a statement need not return a value.  Returning `unit` is exactly that - returning a value that conveys no information.  For completeness, the wildcard variable should be used rather than `x`:

```haskell
Seq l r == (app (lambda _:unit in r) l)
```

## Discussion

We're going to skip the full implementation of sequence for now.  It is quite important in our upcoming discussion of state, but as of right now sequence doesn't do a thing.  Literally.  We can sequence FBAE terms as much as we want and the execution result will never differ from simply executing the last operation in the sequence.  Why is this?  By definition the execution of the first term in a sequence never passes anything to the second.  The `Monad` implementation drops the first term on the floor.  The elaboration implementation makes sure the first result cannot be used in the second computation buy using a wildcard.  So what is sequence for?  When we add global state you will recognize its importance quickly.

## Exercises

1. Can `Seq l r` be elaborated to a `bind`?  If so, implement it.  If not, explain why.  Assume you can use the `_` symbol.
