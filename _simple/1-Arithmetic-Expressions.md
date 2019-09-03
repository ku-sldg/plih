---
layout: frontpage
title: Arithmetic Expressions
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
$$

# Simple Expression Interpreters

Let's start our investigation with an interpreter for a very simple language of calculations involving only `+` and `-` over numbers.  We'll use this first discussion to define an interpreter that calculates a value from such expressions.  The language is quite basic, but we can start discussing important concepts of abstract and concrete syntax, abstract syntax trees, parsing, interpretation and testing.  Pay less attention to the language than to the underlying methodology and concepts used to define an interpreter.

## Concrete Syntax

_Concrete Syntax_ is the textual language written by programmers as input to compilers and interpreters.  When you think of a programming language, you think first of its concrete syntax.  The block structure of C, s-expressions defining Lisp, and the functional syntax of Haskell are examples of concrete syntax that immediately associates with a particular language.

Concrete syntax is always described by a grammar consisting of an alphabet and a collection of grammar rules.  As discussed in the introduction, the alphabet defines basic symbols or tokens in the concrete syntax while grammar rules define how those tokens are sequenced defining elements of the language. A _term_ is any text string defined by the grammar.  Similarly, a _language_ is the smallest set of terms that satisfy a set of grammar rules.  It is quite useful to think of a language as a set and a grammar as a specifier for that set.  Using traditional set comprehension we can define a language, $L$, as:

$$L = \{s:string\; \mid\; G(s)\}$$

where $G$ is a predicate that is true when $s$ satisfies $L$'s grammar rules.

Let's define the concrete syntax for our first language that we will call `AE` for _Arithmetic Expressions_.  Terms in `AE` are defined by the following grammar:

$$\begin{align*}
t ::= & \NUM \mid t + t \mid t - t \\
\end{align*}$$

This is hopefully familiar.  Terms, $t$, in `AE` are either numbers or sums and differences nested arbitrarily.

The language `AE` is the set of all strings that can be created with the grammar.  The strings:

```text
3
1+5
3+5-1
```
are all terms in `AE`.  While the strings:

```text
A
1*5
A+B-C
```

are not terms in `AE`.

Let's also define _values_ in `AE` as the numbers:

$$\begin{align*}
v ::= & \NUM \\
\end{align*}$$

The set of values is a subset of the set of terms.  It represents terms that result from interpretation.  The set of values define what we should get when we run an interpreter.  We will define this more formally later, but for now simply remember that $v$ is an interpretation result.

If we define the predicate $ae(t)$ to be true whenever $t$ satisfies `AE`'s grammar rules, then the language can be defined as a set:

$$AE = \{t:string\; \mid\; ae(t)\}$$

In `AE` we can express precisely three things: (i) a number; (ii) adding two terms; and (iii) subtracting two terms.  Not the most useful of languages, but we have to start somewhere.

Hopefully this discussion is review of your formal language theory or theory of computing studies.  If it isn't, I strongly suggest stopping now and studying some formal language theory.  In addition to being foundational, it is beautiful and useful.

## Inference Rules and Axioms

Knowing what `AE` looks like, let's now define how terms in `AE` are interpreted.  Before writing a Haskell interpreter, we should define formally the meaning of terms.  We will use inference rules for this purpose.

The first tells us how to interpret numbers:

$$\frac{}{\underline{v} \Downarrow v}\; [NumE]$$

$\eval$ is the name of the interpretation function and this rule says calling $\eval$ on a value results in the value.  Remember that $v$ is a number and as such cannot be evaluated further.  What we're saying is that interpreting a constant number value gives back the constant number value.

Addition and subtraction are more interesting and hint at how all our interpreters will be structured.  The rule, $PlusE$ defines the interpretation of terms of the form $t_1+t_2$:

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 + t_2 = v_1+v_2}\; [PlusE]$$

$PlusE$'s antecedents and consequent work together to define a recursive interpreter.  The first antecedent states that $\eval t_1 = v_1$ must be true before the consequent can be true.  But $v_1$ is a variable whose value is the result of calling $\eval$ on $t_1$.  In effect, this antecedent says that $v_1$ must be the result of $\eval t_1$.  The second antecedent behaves similarly for $t_2$ and $v_2$.  Both antecedents name the results of interpreting $t_1$ and $t_2$ as $v_1$ and $v_2$ respectively.

Now that we know the results of evaluating $t_1$ and $t_2$, defining their sum is simple.  Values in `AE` are numbers, so we simply use Haskell's notion of addition to define the sum.  Thus the consequent is $\eval t_1 + t_2 = v_1 + v_2$.

We define subtraction similarly in the $MinusE$ rule:

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 - t_2 = v_1-v_2}\; [MinusE]$$

Understanding the structure of these rules before moving forward is vital.  They both define antecedents that effectively name the results of other calculations.  More specifically, other _recursive_ calculations.  When writing and defining interpreters, recursion is your best friend.  We needn't think now about calculating the values of $t_1$ and $t_2$, only that their values are calculated the same way all other values are calculated.

## Abstract Syntax

Now we have both a concrete syntax and an evaluation semantics for  `AE` defined using mathematical techniques.  We need to transform both definitions into Haskell structures to build our first interpreter for `AE`.  To do this, we will first define an _abstract syntax_ for `AE`.

Concrete syntax is nice on the user side, but painful to work with directly when writing interpreters and compilers.  If you want to try an experiment, write a Haskell program that will interpret the `AE` language directly.  It can be done, but making changes or extending the language is quite laborious and potentially painful.

_Abstract Syntax_ is a data structure representing parsed terms.  This data structure is far more useful than concrete syntax when writing interpreters and compilers. Programs are data and abstract syntax is the data structure that represents them.  An abstract syntax for `AE` written using a Haskell data type is:

```haskell
data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  deriving (Show,Eq)
```

where `Num`, `Plus` and `Minus` are the _constructors_ of the data type `AE` that correspond with numbers, sums and differences in the `AE` concrete syntax.  We use the term constructor because all values of type `AE` are constructed with one of these operations.  By definition, the `AE` type contains all constructions using `Plus`, `Minus` and `Num`, and no more.

All terms in AE have associated data structures in the abstract
syntax.  For example `(Num 1)` is the abstract syntax for 1. `(Plus (Num 1) (Num 3))` is the abstract syntax for `1+3`. `(Minus (Plus (Num 3 (Num 5)) (Num 1))` is the abstract syntax for `3+5-1`.  For the abstract syntax to be effective, every term in the concrete syntax must have an associated term in the abstract syntax.  Remember the properties of relations you learned in your discrete math class?  They come in handy right now.  The relationship between concrete syntax and associated abstract syntax should be a total function. Specifically, concrete syntax terms should have exactly one abstract syntax value and all concrete syntax terms should be associated with some abstract syntax value. Remember that errors are outputs.

From this point forward I will use TLA[^1] _AST_ when referring to abstract syntax data structures.  AST literally means _abstract syntax tree_.  It turns out that Haskell data types naturally form trees and trees are perfect representations for abstract syntax.  I'll come back to this later, but for now remember that AST, abstract syntax, and abstract syntax tree refer to the same Haskell data type.

## Parsers and Pretty Printers

A _parser_ is a program that translates concrete syntax into an AST.  It checks the syntax of its input, generates error messages if the syntax is bad, and generates an AST if the syntax is good. The signature of a parser for the _AE_ language is:

```haskell
parseAE :: String -> AE
```

Give `parseAE` a string and it will return an `AE` if it does not crash.  More generally, the parser's signature for any language will be a string to the datatype for that language's abstract syntax.

A _pretty printer_ is the inverse of a parser.  It translates an `AE` data structure into a string representing the concrete syntax.  The signature of a pretty printer for the _AE_ language is:

```haskell
pprintAE :: AE -> String
```

Give `pprintAE` an `AE` data structure and it will return a string.  More generally, the pretty printer's signature for any language will be the datatype for that langauge's abstract syntax to a string.

If `parseAE` and `pprintAE` are correct, then the following equivalence should hold for any string representing legal concrete syntax:

```haskell
pprintAE (parseAE s) == s
```

If we parse a string and pretty print the result, we back the string.  A useful relationship for testing these tools.

This course is not about building parsers and pretty printers.  This task is largely automated and Haskell provides several tools for building parsers.  I will provide examples using [Parsec](https://wiki.haskell.org/Parsec), a monadic parser combinator tool for writing and composing parsers.  After a bit of setup, Parsec is not difficult to extend to the languages we would like to discuss.  For more details there are a number of online tutorials as well as chapters in major Haskell texts.  For now, let's assume we have the `parseAE` and `pprintAE` functions available.

## Evaluator

An _evaluator_ converts an abstract syntax term into a value.  As noted earlier, values represent valid interpretation results.  If an evaluator produces anything besides a value, something went wrong.  Either the input is invalid, the evaluator is written wrong, or the language definition is problematic.  We'll talk about these issues later.  For now, let's look at an evaluator where everything works as it should.

How should the evaluator be constructed?  The data type defined for the abstract syntax gives us a big clue.  If the constructors from the data type define every possible AST element, then defining an evaluator for each element of the data type should do the trick.  We need to define how the interpreter behaves on each constructor from the `AE` datatype.

First, lets get the `eval` signature down.  Our evaluator will take an element of `AE` and produce a value, where a value is defined as a number.  In the AST, numbers are of the form `(Num n)` where `n` is a Haskell `Int`.  Unfortunately, we can't capture value-ness in our signaturep, so we'll just say that `eval` returns  `Maybe AE`.  We'll use `Maybe` to allow `eval` to return a value or an error.  The signature for `eval` is:

```haskell
eval :: AE -> Maybe AE
```

We now define `eval`'s cases, one for each `AE` constructor starting with the `Num` constructor.  `(Num 3)`, for example, represents a constant `3` in the `AE` abstract syntax.  Without much thought it should be clear that as a constant, `(Num 3)` evaluates to `(Num 3)`.  Numbers are values in `AE`, thus they should not be evaluated further making this consistent with our formal definition.  Thankfully, this is exactly what our inference rule for evaluating numbers says if we remember that $v$ represents $\NUM$:

$$\frac{}{\eval v = v}\; [NumE]$$

Thus, `eval` case for `(Num n)` just returns its argument:

```haskell
(Num n) = return (Num n)
```

Because `return = Just`, `eval (Num 3)` returns `(Just (Num 3))`

We now have an evaluator for literal numbers, but nothing more.

The next constructor, `Plus` represents a more interesting case.  We have a rule named $PlusE$ that defines evaluation of `t1+t2`:

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 + t_2 = v_1+v_2}\; [PlusE]$$

The inference rule is defined in terms of concrete rather than abstract syntax, so we have to do a bit of translation work. $t_1+t_2$ translates quickly into `(Plus t1 t2)`. If `t1` evaluates to `(Num v1)` and `t2` evaluates to `(Num v2)` then `(Plus t1 t2)` evalautes to `(Num v1)+(Num v2)`.  There is no `+` operation in Haskell for `(Num n)` constructions, this we need to write one to define addition in `AE`.  We can define addition in `AE` in terms of addition in Haskell.  Specifically:

```Haskell
(Num n) + (Num m) == (Num n+m)
```

There are many ways to construct this operation in Haskell, but we will use a lifting function that will allow us to define any operation in `AE` using an operation from Haskell:

```haskell
liftNum :: (Int -> Int -> Int) -> AE -> AE -> AE
liftNum f (Num l) (Num r) = (Num (f l r))
```

`liftNum` takes a binary function over integers and two `Num` constructions.  It lifts the binary operation into `Num` by extracting the `Int` values, applying the operation, and putting the result back in `Num`.  While we could not use `+` on `Num` constructions directly, we can use `liftNum` to define addition in `AE`:

```haskell
(Num n) + (Num m) == (liftNum (+) n m
```

Furthermore, we can define other `AE` operations similarly.

Now we have everything needed to define `Plus`.  This inference rule can be almost directly translated into Haskell using `do` to evaluate antecedents, calculate the and return the value defined by the consequent:

```haskell
(Plus t1 t2) = do v1 <- eval t1
                  v2 <- eval t2
                 return (liftNum (+) v1 v2)
```

The translation from inference rule to Haskell follows standard rule of thumb.  The `dp` construct bindings manage the rule antecedents, performing evaluation and binding variables.  The `return` is the consequent and evaluates the term.  While only a rule-of-thumb, it will prove highly useful.

Finally, The `Minus` constructor case is identical to the `Plus` constructor case except values are subtracted rather than added together.  For completeness, here is the subtraction case:

```haskell
(Minus t1 t2) = do v1 <- eval t1
                   v2 <- eval t2
                   return (liftNum (-) v1 v2)
```

Putting the cases together in the following evaluator for `AE` that reduces every abstract syntax term to a value:

```haskell
eval :: AE -> Maybe AE
eval (Num x) = return (Num x)
eval (Plus t1 t2) = do v1 <- (eval t1)
                       v2 <- (eval t2)
                       return (liftNum (+) v1 v2)
eval (Minus t1 t2) = do v1 <- (eval t1)
                        v2 <- (eval t2)
                        return (liftNum (-) v1 v2)
eval (Mult t1 t2) = do v1 <- (eval t1)
                       v2 <- (eval t2)
                       return (liftNum (*) v1 v2)
eval (Div t1 t2) = do v1 <- (eval t1)
                      v2 <- (eval t2)
                      return (liftNum div v1 v2)
```

The evaluator follows a pattern that every evaluator we write will follow.  Each constructor from the AST definition has a case in the `eval` function that evaluates that constructor.  This pattern and the accompanying `data` construct gives us three nice language properties - completeness, determinicity, and normalization

Completeness says that every term constructed in `AE` will be evaluated by `eval`.  Can we prove that? As it turns out, the Haskell `data` construct gives us some exceptionally nice properties for free, without direct proof. By definition, every value in the abstract syntax for `AE` is constructed with `Num`, `Plus`, and `Minus `.  There are no other `AE` values.   This is generally true of any type defined using `data` in Haskell.  All values of the type are constructed with its constructors.

We know `AE` is coplete because there is one inference rule for every syntax construct and one case in `eval` for each inference rule.  Specifically, the `eval` function has one case for each constructor from `AE`.  Because all `AE` values are built with those constructors, there is a case for every `AE` value in the `eval` function.  While not quite a proof, this gives strong evidence that our language definition is complete.

Determinicity says that if we call `eval` on any term, we will get the same result.  This is an exceptionally important property as we do bit want the same call to `eval` resulting in different values.  We know `AE` is deterministic because there is precisely one inference rule for interpreting each element of the concrete syntax.  In turn we know that `eval` is deterministic because there is precisely one case in the definition for each `AE` constructor.  Given `(Num n)` there is one rule and one `eval` case.  Given `Plus` there is one rule and one `eval` case.

Normalization says that a call to `eval` on any term constructed in `AE` will terminate.  Again, a pretty bold statement.  Can we prove it?  The elements of a Haskell `data` type specifically and algebraic types generally have are `well-ordered`.  In mathematics, well-ordered means that every subset of a set has a least element.  Getting from well-ordered to guaranteed termination takes a bit of work, but the gist is that components of a constructor are smaller than the constructor itself.  Each recursive call on the parts of a term is made on a smaller term.  Consider `eval (Plus t1 t2)` where `t1` and `t2` can be viewed as smaller than `(Plus t1 t2)`.  Because every set of `AE` terms has a least element, pulling a term apart into its pieces will eventually get to that least element.

The least elements of `AE` are those constructed by `(Num n)`.  When the evaluator reaches the least element, it cannot go further and terminates.  Every call to `eval` gets closer to the least element.  Note that those those least elements are what we defined as values.  This is not a coincidence.  Not only does `AE` terminate, it always terminates with a value.  Said in terms we all understand, the `AE` evaluator never crashes for any element of `AE`.

## All Together Now

Before we say more about `AE`, lets put all the pieces together into a single definition.  Definition of `AE` is now complete syntactically, semantically and operationally:

* Syntax is defined by the `AE` grammar
* Semantics is defined by the `AE` inference rules
* Operations are defined by `eval` and `parse`.

First we defined a concrete syntax for terms:

$$\begin{align*}
t ::= & \NUM \mid t + t \mid t - t \\
\end{align*}$$

and syntactic definition of values:

$$\begin{align*}
v ::= & \NUM \\
\end{align*}$$

Then we defined basic inference rules formally defining how `AE` is interpreted:

$$\frac{}{\eval v = v}\; [NumE]$$

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 + t_2 = v_1+v_2}\; [PlusE]$$

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 + t_2 = v_1-v_2}\; [MinusE]$$

We implemented a parser from the grammar and an evaluator from the inference rules:

```haskell
eval :: AE -> Maybe AE
eval (Num x) = return (Num x)
eval (Plus t1 t2) = do v1 <- (eval t1)
                       v2 <- (eval t2)
                       return (liftNum (+) v1 v2)
eval (Minus t1 t2) = do v1 <- (eval t1)
                        v2 <- (eval t2)
                        return (liftNum (-) v1 v2)
eval (Mult t1 t2) = do v1 <- (eval t1)
                       v2 <- (eval t2)
                       return (liftNum (*) v1 v2)
eval (Div t1 t2) = do v1 <- (eval t1)
                      v2 <- (eval t2)
                      return (liftNum div v1 v2)
```

Given the parser and evaluator for `AE`, we can now define a language interpreter, `interp`, that puts everything together:

```haskell
interp :: String -> Maybe AE
interp = eval . parseAE
```

In words, `interp` is the composition of `parseAE` and `eval`.  This notation uses Haskell's function composition to define that `parse` will be called first and the output passed to `eval`.  If `parseAE` throws an error, `interp` will terminate without passing a value to `eval`.

If you are unfamiliar with the point-free style and the use of function composition, `interp` can be defined in a more traditional manner as:

```haskell
interp x = eval (parseAE x)
```

Use the notation you are most comfortable with.  There is no advantage to either, but function composition more closely resembles the mathematics of interpreter construction.

## Discussion

`AE` is a rather silly language that is less powerful than one of those bank calculators you get when you open a checking account.  It adds and subtracts numbers.  However, we need to start somewhere and `AE` is good for that.

### Complete, Determinicity, and Normalizing

We said earlier that `eval` is complete, deterministic and normalizing.  Complete in that any element of `AE` can be interpreted, deterministic in that there is only one way to interpret any element of `AE`, and normalizing in that every interpretation of and `AE` element terminates.

Unfortunately, these nice properties result from `AE` being such a trivial language.  Completeness and deterministic are properties we will seek to preserve as we add features.  However, normalizing will prove problematic when we add recursion and looping.

### Induction and Extensionality

There is one property of `AE` structures that underlies most of our discussion.  The `AE` abstract syntax specifically and algebraic types generally have both inductive and extensionality principles.  The inductive principle allows us to prove properties over `AE` and the extensionality principle allows us to determine if two `AE` structures are equivalent.  We won't use either principle yet, so let's define them informally for now.

Induction over `AE` says that some property, $p$, is true for all elements of `AE` if we can:

1. Prove $p$ directly for base cases
2. Prove $p$ for inductive cases by assuming $p$ for the case's pieces

What are the base and inductive cases?  For `AE`, `Num` is the base case while `Plus` and `Minus` are inductive cases.  `Num` is a base case because it is not constructed from other elements of `AE`.  We can see this in the `data` definition. It is also reflected in the definition of `eval` where there is no recursive call for `Num`.  `Plus` and `Minus` both depend on other `AE` constructions in their definition and `eval` cases.  Stated in terms of the `AE` concrete syntax, the induction principle states that $p$ is true for any `AE` term $t$ when:

$$
\begin{align*}
\forall & n \cdot p(n) \\
\forall & t_1 t_2 \cdot p(t_1) \rightarrow p(t_2) \rightarrow p(t_1 + t_2) \\
\forall & t_1 t_2 \cdot p(t_1) \rightarrow p(t_2) \rightarrow p(t_1 - t_2) \\
\end{align*}
$$

Extensionality is simpler to state.  Two terms in `AE` are equivalent if they use the same constructor and their parts are equivalent.  For any $t$ and $t'$, $t=t'$ if one of the following cases applies:

$$
\begin{align*}
t_1 + t_2 = t_1' + t_2' \leftrightarrow t_1=t_1' \wedge t_2=t_2' \\
t_1 - t_2 = t_1' - t_2' \leftrightarrow t_1=t_1' \wedge t_2=t_2' \\
\end{align*}
$$

Thus if no case applies, then the terms are not equal.  So, $t_1+t_2$
will never be equal to $t_1'-t_2'$.

## Definitions

Lots of definitions to get us started:

* Concrete Syntax - The textual form of a language used by programmers to write code
* Abstract Syntax - The data structure representation of a language used by tools
* Parser - Translates from concrete syntax to abstract syntax
* Evaluation - Converts abstract syntax into a value.
* Value - A good result from evaluation
* Interpreter - Any program that translates syntax from one form into another
* Algebraic Type - Uses sums and products to define types.  Haskell's `data` system defines algebraic types
* Termination - Halting
* Deterministic - Every invocation with the same inputs generates the same outputs
* Normalization - Every invocation halts
- Induction - Proof principle commonly used for countably infinite structures
- Extensionality - Proof principle used to show two structures are equal.

## Exercises

1. Add multiplication and division to `AE`.  Do we lose any of our nice properties by doing this?
2. Rewrite the `eval` function to return a Haskell `Int` rather than an `AE` value.  This is an alternative to returning the Haskell datatype.
2. Rewrite `AE` replacing the `Plus` and `Minus` constructors in the AST with a single constructor `Binop op t1 t2` where `op` is the represented binary operation.  You will need to change the parser to generate `Binop` rather than `Plus` and `Minus`.  Think carefully about what `op` should be.  If you do it right, you should be able to add any operator to `AE` by simply changing the parser.

## Source

* Download [source]({{site.baseurl}}/haskell/ae.hs) for all interpreter code from this chapter.
* Download [source]({{site.baseurl}}/haskell/parserUtils.hs) for the parser utilities used by the interpreters.

## Notes

Documentation for the [Parsec Expression Parser](https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Expr.html)

[^1]: Three Letter Acronym
