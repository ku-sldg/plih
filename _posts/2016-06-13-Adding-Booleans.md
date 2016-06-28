---
layout: frontpage
title: Adding Booleans
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

# Adding New Values

Our initial interpreter for arithmetic expressions has several nice properties. All AE programs halt.  There is no mechanism for repeating execution, thus it should not be at all surprising that every AE program terminates and returns a value.  No AE programs crash.  If an AE program parses and can be constructed in the AE abstract syntax, it cannot crash during execution.  There is only one value type and the addition and subtraction operations are closed over that type.  Said differently, everything is an integer  and plus and minus are defined over any pair of integers.

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

Updating the `AE` parser to operate on `ABE` demonstrates a pattern we will use extensively.  The definition of `expr` does not change - it will still be our parser and is built from opTable and terms.

{% highlight haskell %}
expr :: Parser ABE
expr = buildExpressionParser opTable term
{% endhighlight %}

The addition of new operators allows us to look at more functionality in the operator specification.  You see first the definitions for plus and minus from `AE` together in a list indicating they have the same precedent.  Then you see definitions for `isZero` and `<=` in a list immediately below.  These operators have precent immediately lower than `+` and `-`.  For example, `isZero x - y` will be parsed as `(isZero (x-y))` because `-` has higher precedence than `isZero`.  Finally, `&&` is in its own list following the numeric operations indicating that it has yet lower precedence.  So, `true && isZero (x-y)` parses as `(true && (isZero (x-y))).

{% highlight haskell %}
opTable = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            , [ inFix "<=" Leq AssocLeft
	          , preFix "isZero" IsZero ]
            , [ inFix "&&" And AssocLeft ]
            ]
{% endhighlight %}

One final addition is the first appearance of the `preFix` operator function.  As you might guess, this creates a prefix operation in a similar manner as `inFix`.  There is also a `postFix` function should you find a need for it.

If you haven't bought in to the Parsec approach, hopefully the next extension will start to convert you.  Here we define parsers for each individual term that operators can operate over.  In `AE` this was only integers, but in `ABE` we add Boolean values and `if`.  The first three parsers operate over number and boolean constants.  We're using the `integer` parser that is built in Parsec for numbers.  For `true` and `false` we use the `reserved` parser that operates over reserved words that are enumerated in `PaserUtils`.  `trueExpr` parses the reserved word `true` and returns the abstract syntax `(Boolean True)` representing the appropriate constant.  Similarly for `falseExpr`.  Anytime you need to parse a reserved work or constant, these built-in parsers can be modified.  Remember however that `lexer` is defined in `ParserUtils` and must be modified if you want to include keywords that are not already included.

{% highlight haskell %}
numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)
{% endhighlight %}

The parser for `if` is a hint of Parsec's power.  It uses the Haskell `do` notation to compose a collection of smaller parsers.  Within the `do` notation, expressions are evaluated sequentially and results bound to variables using `<-`.  The `ifExpr` parser executes the following parsers in sequence:

1. Parse an "if" using `reserved`
2. Parse an expression and store the result in `c`
3. Parse a "then" using `reserved`
4. Parse an expression and store the result in `t`
5. Parse an "else" using reserved
6. Parse an expression and store the result in `e`
7. Return the AST result `(if c t e)`

Here's the actual code:

{% highlight haskell %}
ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)
{% endhighlight %}

Now we put the whole thing together to define `term`.  The `<|>` notation should be interpreted as or.  This the `term` parser looks for an expression in parenthesis, a number, a true or false, or an  if expression.  The `parens` parser is another built-in parser that puts things in parenthesis.  So `parens lexer expr` looks for `(expr)`.  The other parsers are what we built above.

{% highlight haskell %}
term = parens lexer expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
{% endhighlight %}

Looking back at the definition of `expr` it defines a complete parser using `term` and `opTable`.  Expressions can be used in operations and operations in expressions.  Plus, we've now got a parsing infrastructure that can be easily extended without much discussion moving forward.

We'll invoke our new parser the same as always:

{% highlight haskell %}
parseABE = parseString expr

parseABEFile = parseFile expr
{% endhighlight %}

## Interpreter

Finally.  We have abstract syntax generated from concrete syntax and can now write our interpreter.  This involves extending the `AE` `eval` function to include new cases for the new Boolean operations.  The initial definition is:

{% highlight haskell %}
eval :: ABE -> ABE
eval (Num x) = (Num x)
eval (Plus t1 t2) = let (Num v1) = (eval t1)
                        (Num v2) = (eval t2)
                    in (Num (v1+v2))
eval (Minus t1 t2) = let (Num v1) = (eval t1)
                         (Num v2) = (eval t2)
                     in (Num (v1-v2))
{% endhighlight %}

The additional cases are largely as one would anticipate.  `And` `Leq` and `IsZero` each evaluate their arguments and return an appropriate result.  The only real change is operations can now return types that differ from their argument types.  This is not a big change, but operations are no longer closed.

The `If` construct differs in that not arguments are evaluated before the `If`.  The condition is evaluated an the Haskell `if` expression used to evaluate the appropriate `then` or `else` expression.  Note that in both `ABE` and Haskell `if` is an expression that returns a value when calculated.  This is in contrast to languages like C or Java where `if` is a command that sequences execution.  We'll revisit this concept later.

{% highlight haskell %}
eval (Boolean b) = (Boolean b)
eval (And t1 t2) = let (Boolean v1) = (eval t1)
                       (Boolean v2) = (eval t2)
                   in (Boolean (v1 && v2))
eval (Leq t1 t2) = let (Num v1) = (eval t1)
                       (Num v2) = (eval t2)
                   in (Boolean (v1 <= v2))
eval (IsZero t) = let (Num v) = (eval t)
                  in (Boolean (v == 0))
eval (If t1 t2 t3) = let (Boolean v) = (eval t1)
                     in if v then (eval t2) else (eval t3)
{% endhighlight %}

Finally, we'll combine the `eval` and `parseABE` functions into a single `interp` function just like we did before:

{% highlight haskell %}
interp = eval . parseABE
{% endhighlight %}

## Testing ABE

Let's fire up QuickCheck and see what we have.  Remember that all `AE` programs famously ran to termination and produced a value.  Will the same thing hold true for `ABE` programs?  I suspect you can easily figure the answer, but let's follow a rigorous process to see why.  The process will be useful to us when our languages become more complex.

### Generator

We need to extend the generator from the `Testing` chapter to generate `ABE` elements.  Like our parser we simply need to define generators for the new `AST` elements and add them to the original generator.

Generating `Boolean` values is identical to generating `Num` values except we use `choose` to select among `True` and `False`:

{% highlight haskell %}
genBool =
  do t <- choose (True,False)
     return (Boolean t)
{% endhighlight %}

Generators for the remaining operators are identical to what we did for `Plus` and `Minus`.  Specifically, generate the arguments and put them together:

{% highlight haskell %}
genAnd n =
  do s <- genABE n
     t <- genABE n
     return (And s t)

genLeq n =
  do s <- genABE n
     t <- genABE n
     return (Leq s t)

genIsZero n =
  do s <- genABE n
     return (IsZero s)

genIf n =
  do s <- genABE n
     t <- genABE n
     u <- genABE n
     return (If s t u)
{% endhighlight %}

Remember that the argument `n` is our size counter that will force the generator to terminate when the new `ABE` structure reaches a specified size.

Now `genABE` for generating complete terms:

{% highlight haskell %}
genABE :: Int -> Gen ABE
genABE 0 = 
  do term <- oneof [genNum,genBool]
     return term
genABE n =
  do term <- oneof [genNum,(genPlus (n-1))
                   ,(genMinus (n-1))
                   ,(genAnd (n-1))
                   ,(genLeq (n-1))
                   ,(genIsZero (n-1))
                   ,(genIf (n-1))]
     return term
{% endhighlight %}

Note what's going on here.  For the base case, we use `oneof` to generate either a number or a boolean.  For the recursive case, we simply add generators for new terms to the argment to `oneof` adding them to the `ABE` generator.  That's it.  We're done.

### QuickCheck

Our testing functions are identical to those for `AE` with `AE` changed to `ABE`.  Literally, that's all there is to it:

{% highlight haskell %}
testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseABE (pprint t) == t)

testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval t))
{% endhighlight %}

Now we're ready to go.

### Initial Tests

Using the `testParser` function we'll first test 1000 random `ABE` terms:

{% highlight haskell %}
testParser 1000
+++ OK, passed 1000 tests.
{% endhighlight %}

So far, so good. The `ABE` parser passes 1000 tests of arbitrary structures.

Using the `testEval` function we'll now test 1000 random `ABE` terms:

{% highlight haskell %}
 testEval 1000
*** Failed! (after 2 tests): 
Exception:
  /Users/alex/Documents/Research/books/plih/sources/haskell/abe.hs:123:23-40: Irrefutable pattern failed for pattern (Main.Num v)
IsZero (Boolean False)
{% endhighlight %}

This result tells us that the `eval` function fails after 2 tests.  Specifically, the term `IsZero (Boolean False)` cases `eval` to fail without generating a value. It should be clear why. `IsZero` expects a number yet it is called on a Boolean value.  `IsZero (Boolean False)` is a *counterexample* for the test and gives us a direction to follow.[^1]

## Discussion

`ABE` is only moderately less silly than `AE`.  We simply extended `AE` to include Boolean values and several new operations that both consume and produce Boolean values.  By doing this, we now have an interpreter that crashes for some inputs.

Working through `ABE` does have value.  We extended `AE` to include Boolean values and terms, then extended the `AE` parser, pretty printer, evaluator, and generator.  In subsequent chapters we'll see that the techniques we used generalize to other languages.  For now, we must do something about failure before moving on.

[^1]: If you execute `testEval` in the same way, you will almost certainly generate a different counterexample due to the arbitrary nature of the generator.