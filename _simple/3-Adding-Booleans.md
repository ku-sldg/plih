---
layout: frontpage
title: Adding Booleans
use_math: true
categories: chapter ch1
---

$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeof}\;}
\newcommand\interp{\mathsf{interp}\;}
\newcommand\eval{ \Downarrow }
\newcommand\NUM{\mathsf{NUM}\;}
\newcommand\iif{\mathsf{if}\;}
\newcommand\tthen{\;\mathsf{then}\;}
\newcommand\eelse{\;\mathsf{else}\;}
\newcommand\iisZero{\mathsf{isZero}\;}
\newcommand\aand{\;\mathsf{\&\&}\;}
\newcommand\lleq{\;\mathtt{<=}\;}
\newcommand\ttrue{\;\mathsf{true}}
\newcommand\ffalse{\;\mathsf{false}}
$$

# Adding New Values

Our initial interpreter for arithmetic expressions has several nice
properties. All AE programs halt.  There is no mechanism for repeating
execution, thus it should not be at all surprising that every AE
program terminates and returns a value.  No AE programs crash.  If an
AE program parses and can be constructed in the AE abstract syntax, it
cannot crash during execution.  There is only one value type and the
addition and subtraction operations are closed over that type.  Said
differently, everything is an integer  and plus and minus are defined
over any pair of integers.

Unfortunately, the same features of AE that make all programs
terminate and not crash makes AE useless for programming.  We can't
have multiple value types, we can't define variables, we can't define
functions, and we can't loop.  To write real programs, we have to have
at least some of these capabilities.


## Multiple Value Types

Let's address the first issue - a lack of diverse value types - by
adding Boolean values and operations over those values.  Specifically,
we will add `if`, `<=`, `&&`, and `isZero` as well as the values
`true` and `false`.  Certainly this is not a complete set of Boolean
operations, but is a representative sample.


## Concrete Syntax

Adding concrete syntax for Boolean values and operations is a simple
matter of adding Boolean constant representations and representations
for the various operations.  The new concrete syntax for a language we
will call `ABE` (*Arithmetic and Boolean Expressions*) is

$$\begin{align*}
t ::= & \NUM \mid t + t \mid t - t \\
      & \mid \ttrue \mid \ffalse \mid \iif t \tthen t \eelse t \\
      & \mid t \lleq t \mid t \aand t \mid \iisZero t \\
\end{align*}$$

We also need to update our concept of a value to include $\ttrue$ and $\ffalse$:

$$\begin{align*}
v := \NUM \mid \ttrue \mid \ffalse \\
\end{align*}$$

The need to add new values foreshadows interesting times ahead.

## Inference Rules

Let's start with our inference rules for `AE` and extend them to
include new rules for `ABE` constructs.  Here are the original
evaluation rules for `AE`: 

$$\frac{}{\underline{v} \eval v}\; [NumE]$$

$$\frac{t_1\eval v_1,\; t_2\eval v_2}{t_1 \underline{+} t_2\eval v_1+v_2}\; [PlusE]$$

$$\frac{t_1\eval v_1,\; t_2\eval v_2}{t_1 \underline{-} t_2 \eval v_1-v_2}\; [MinusE]$$

Boolean values are just like numerical values.  So much so that we do
no not need another rule for values.  However, we will rename the
$NumE$ rule to reflect that it now covers all values: 

$$\frac{}{\underline{v} \eval v}\; [ValueE]$$

Rules for $\aand$ and $\lleq$ follow the same pattern as rules for $+$
and $-$: 

$$\frac{t_1 \eval v_1,\; t_2 \eval v_2}{t_1 \aand t_2 \eval v_1 \wedge v_2}\; [AndE]$$

$$\frac{t_1 \eval v_1,\; t_2 \eval v_2}{t_1 \lleq t_2 \eval v_1\leq v_2}\; [LeqE]$$

The rule for $\iisZero$ is only modestly different because it is a
unary operation.  Unsurprisingly it has only one antecedent: 

$$\frac{t \eval v}{\iisZero t\eval v==0}\; [isZeroE]$$

Finally, lets deal with $\iif$.  Thinking of $\iif$ as simply an
operation with three arguments, we can follow our previous pattern
giving us this rule: 

$$\frac{t_0 \eval \ttrue\; t_1 \eval v_1}{\iif t_0 \tthen t_1 \eelse t_2 \eval v_1}\;[IfTrueE]$$

$$\frac{t_0 \eval \ffalse\; t_2 \eval v_2}{\iif t_0 \tthen t_1 \eelse t_2 \eval v_2}\;[IfFalseE]$$

The first rule only applies when $t_0$ evaluates to $\ttrue$
while the second applies when $t_0$ evaluates to $\ffalse$. 

## Abstract Syntax

Before we extend our `AE` parser to `ABE`, we need to extend our
abstract syntax for `AE` to handle new constructs.  We will start with
`true` and `false`, the Boolean constant values.  To represent these
values in the abstract syntax, we will use a technique similar to
numbers.  Specifically, the Haskell `True` and `False` values will be
lifted into our AST using the constructor `Boolean`: 


```haskell
Boolean :: Bool -> ABE
```

While `True` and `False` are values in Haskell, `(Boolean True)` and `(Boolean False)` are values in our language.

Next we will add unary and binary operations over Boolean values.  These operations are no different than the binary and unary operations over integers:

```haskell
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
```

Finally, we add an `if` construct in the canonical fashion as if it were simply a three-argument function.

```haskell
  If :: ABE -> ABE -> ABE -> ABE
```

The resulting complete AST structure is now:

```haskell
data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)
```

## Interpreter

Finally.  We have abstract syntax generated from concrete syntax and can now write our interpreter.  This involves extending the `AE` `eval` function to include new cases for the new Boolean operations.  The initial definition is:

```haskell
eval :: ABE -> Maybe ABE
eval (Num x) = return (Num x)
eval (Plus t1 t2) = do { v1 <- (eval t1)
                         v2 <- (eval t2)
                         return (Num (liftNum (+) v1 v2)) }
eval (Minus t1 t2) = do { v1 <- (eval t1)
                          v2 <- (eval t2)
                          return (Num (liftNum (-) v1 v2)) }
```

The additional cases are largely as one would anticipate.  `And` `Leq`
and `IsZero` each evaluate their arguments and return an appropriate
result.  The only real change is operations can now return types that
differ from their argument types.  This is not a big change, but
operations are no longer closed. 

The `If` construct differs in that not all arguments are evaluated
before the `If`.  The condition is evaluated and the Haskell `if`
expression is used to evaluate the appropriate `then` or `else`
expression.  Note that in both `ABE` and Haskell `if` is an expression
that returns a value when calculated.  This is in contrast to
languages like C or Java where `if` is a command that sequences
execution.  We'll revisit this concept later. 

```haskell
eval (Boolean b) = (Just (Boolean b))
eval (And t1 t2) = do { r1 <- (eval t1) ;
                        r2 <- (eval t2) ;
                        return (liftBool (&&) r1 r2) }
eval (Leq t1 t2) = do { r1 <- (eval t1) ;
                        r2 <- (eval t2) ;
                        return (liftNum2Bool (<=) r1 r2) }
eval (IsZero t) = do { r <- (eval t)
                       return (liftNum2Bool (==) r (Num 0)) }
eval (If t1 t2 t3) = do { (Boolean v) <- (eval t1)
                          (if v then (eval t2) else (eval t3)) }
```

Finally, we'll combine the `eval` and `parseABE` functions into a single `interp` function just like we did before:

```haskell
interp = eval . parseABE
```

## Testing

Testing the `ABE` `interp` function reveals a big change in our new interpreter.  Examples such as:

```haskell
interp 3+5
== (Just (Num 8))
interp if true then 5 else 10
== (Just (Num 6))
interp 5<=3
== (Just (Boolean false))
```

all work as anticiapted, calculating the correct value and returning it as abstract syntax embedded in a `Maybe` monad.

The big change is this interpreter crashes.  Our `AE` interpreter did not crash.  No well-formed term would cause the interpreter to belly up.  For `ABE` there are many cases that crash.  For example:

```haskell
interp false + 5
interp if 3 then true else 7
interp true <= 7
```

all cause the interpreter to crash into Haskell.  It should be quite clear why the crashes occur.  Specifically, plus is not defined over `false`, `if`'s first argument must be Boolean and `true` cannot be compared with `7`.  If we allow these expressions to be interpreted, there is no proper result.  However, if we can *predict* failure before interpretation we can gracefully fail rather than drop out of our interpreter to Haskell.

## Discussion

`ABE` is only moderately less silly than `AE`.  We simply extended `AE` to include Boolean values and several new operations that both consume and produce Boolean values.  By doing this, we now have an interpreter that crashes for some inputs.

Working through `ABE` does have value.  We extended `AE` to include Boolean values and terms, then extended the `AE` parser, pretty printer, evaluator, and generator.  In subsequent chapters we'll see that the techniques we used generalize to other languages.  For now, we must do something about failure before moving on.

## Exercises

1. Add disjunction and negation to the `ABE` language.  Define concrete syntax and abstract syntax.  Update the parser, pretty printer, evaluator, and QuickCheck term generator.
2. Add multiplication and division to the `ABE` language.  Define concrete syntax and abstract syntax.  Update the parser, pretty printer, evaluator, and QuickCheck term generator.
3. Write a function that walks the `ABE` AST and counts the number of Boolean operations.

## Source

* Download [source]({{site.baseurl}}/haskell/abe.hs) for all interpreter code from this chapter.
* Download [source]({{site.baseurl}}/haskell/parserUtils.hs) for the parser utilities used by the interpreters.

## Notes

[^1]: If you execute `testEval` in the same way, you will almost certainly generate a different counterexample due to the arbitrary nature of the generator.
