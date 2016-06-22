---
layout: frontpage
title: Failure Is an Option
use_math: true
category: chapter
---

$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeof}\;}
\newcommand\interp{\mathsf{interp}\;}
\newcommand\eval{\mathsf{eval}\;}
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

# Failure Is an Option

Our initial interpreter for arithmetic expressions has several nice properties.  All AE programs halt.  There is no mechanism for repeating execution, thus it should not be at all surprising that every AE program terminates and returns a value.  No AE programs crash.  If an AE program parses and can be constructed in the AE abstract syntax, it cannot crash during execution.  There is only one value type and the addition and subtraction operations are closed over that type.  Said differently, everything is an integer  and plus and minus are defined over any pair of integers.

Unfortunately, the same features of AE that make all programs terminate and not crash makes AE useless for programming.  We can't have multiple value types, we can't define variables, we can't define functions, and we can't loop.  To write real programs, we have to have at least some of these capabilities.

## Multiple Value Types

Let's address the first issue - a lack of diverse value types - by adding Boolean values and operations over those values.  Specifically, we will add `if`, `<=`, `&&`, and `isZero` as well as the values `true` and `false`.  Certainly this is not a complete set of Boolean operations, but is a representative sample.

## Concrete Syntax

Adding concrete syntax for Boolean values and operations is a simple matter of adding Boolean constant representations and representations for the various operations.  The new concrete syntax for a language we will call `ABE` (*Arithmetic and Boolean Expressions*) is

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

Let's start with our inference rules for `AE` and extend them to include new rules for `ABE` constructs.  

$$\frac{}{\eval v = v}\; [NumE]$$

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 + t_2 = v_1+v_2}\; [PlusE]$$

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 + t_2 = v_1-v_2}\; [MinusE]$$

Boolean values are just like numerical values.  So much so that we do no not need another rule for values.  However, we will rename the $NumE$ rule to reflect that it now covers all values:

$$\frac{}{\eval v = v}\; [ValueE]$$

Rules for $\aand$ and $\lleq$ follow the same pattern as rules for $+$ and $-$:

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 \aand t_2 = v_1 \wedge v_2}\; [AndE]$$

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 \lleq t_2 = v_1\leq v_2}\; [LeqE]$$

The rule for $\iisZero$ is only modestly different because it is a unary operation.  Unsurprisingly it has only one antecedent:

$$\frac{\eval t = v}{\eval \iisZero t = v==0}\; [isZeroE]$$

Finally, lets deal with $\iif$.  Thinking of $\iif$ as simply an operation with three arguments, we can follow our previous pattern giving us this rule:

$$\frac{\eval t_0 = v_0,\;\eval t_1 = v_1,\;\eval t_2 = v_2}{\eval \iif t_0 \tthen t_1 \eelse t_2 = \iif v_0 \tthen v_1 \eelse v_2}\;[IfE]$$

Is there a problem with this rule?  The antecedents calculate values for the conditional ($t_0$), the then case ($t_1$), and the  else case ($t_2$) and translates that into a traditional `if` statement.  Thus, no matter the value of the conditional, *both options are evaluated*.  For now, this is not a problem because we are dealing only with expressions that have no state.  Specifically, neither option impacts the other when it is evaluated.  This will not be true for long.  If we added a `print` statement or variable assignment, we cannot evaluate both if options.

It turns out there is an easy fix that uses multiple rules to evaluate if:

$$\frac{\eval t_0 = \ttrue,\; \eval t_1 = v_1}{\eval \iif t_0 \tthen t_1 \eelse t_2 = v_1}\;[IfTrueE]$$

$$\frac{\eval t_0 = \ffalse,\; \eval t_2 = v_2}{\eval \iif t_0 \tthen t_1 \eelse t_2 = v_2}\;[IfFalseE]$$

The first rule only applies when $\eval t_0$ evaluates to $\ttrue$ while the second applies when $\eval t_0$ evaluates to $\ffalse$.  Right now this is overkill, but it is a good idea to start thinking in these terms.

## Abstract Syntax

Before we extend our `AE` parser to `ABE`, we need to extend our abstract syntax for `AE` to handle new constructs.  We will start with `true` and `false`, the Boolean constant values.  To represent these values in the abstract syntax, we will use a technique similar to numbers.  Specifically, the Haskell `True` and `False` values will be lifted into our AST using the constructor `Boolean`:

{% highlight haskell %}
  Boolean :: Bool -> ABE
{% endhighlight %}

While `True` and `False` are values in Haskell, `(Boolean True)` and `(Boolean False)` are values in our language.

Next we will add unary and binary operations over Boolean values.  These operations are no different than the binary and unary operations over integers:

{% highlight haskell %}
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
{% endhighlight %}

Finally, we add an `if` construct in the canonical fashion as if it were simply a three-argument function.

{% highlight haskell %}
  If :: ABE -> ABE -> ABE -> ABE
{% endhighlight %}

The resulting complete AST structure is now:

{% highlight haskell %}
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
{% endhighlight %}

## Parsing

{% highlight haskell %}
expr :: Parser ABE
expr = buildExpressionParser operators term

operators = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            , [ inFix "&&" And AssocLeft ]
            , [ inFix "<=" Leq AssocLeft ]
            , [ preFix "isZero" IsZero ]
            ]

numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

term = parens lexer expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr

-- Parser invocation

parseABE = parseString expr

parseABEFile = parseFile expr
{% endhighlight %}

## Interpreter

{% highlight haskell %}
eval :: ABE -> ABE
eval (Num x) = (Num x)
eval (Plus l r) = let (Num l') = (eval l)
                      (Num r') = (eval r)
                      in (Num (l'+r'))
eval (Minus l r) = let (Num l') = (eval l)
                       (Num r') = (eval r)
                       in (Num (l'-r'))
eval (Boolean b) = (Boolean b)
eval (And l r) = let (Boolean l') = (eval l)
                     (Boolean r') = (eval r)
                 in (Boolean (l' && r'))
eval (Leq l r) = let (Num l') = (eval l)
                     (Num r') = (eval r)
                 in (Boolean (l' <= r'))
eval (IsZero v) = let (Num v') = (eval v)
                  in (Boolean (v' == 0))
eval (If c t e) = let (Boolean c') = (eval c)
                  in if c' then (eval t) else (eval e)
{% endhighlight %}
