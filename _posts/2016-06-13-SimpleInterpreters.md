---
layout: frontpage
title: Simple Expression Interpreters
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
$$

# Simple Expression Interpreters

Description goes here...

## Concrete Syntax

*Concrete Syntax* is the textual language written by programmers as input to compilers and interpreters.  When you think of a programming language, you think first of its concrete syntax.  The bock structure of C, s-expressions defining Lisp, and the functional syntax of Haskell are examples of concrete syntax that immediately associates with a particular language. 

Concrete syntax is  always described by a *grammar* consisting of an *alphabet* and a collection of *grammar rules*.  The alphabet defines basic symbols and tokens in the concrete syntax while grammar rules define how those tokens are sequenced defining elements of the language.

The alphabet is specified as collection of atomic symbols.  The English alphabet is a good example.  Alphabets can be much more sophisticated and include other characters and words.  For our purposes, the alphabet will be atomic constructs - things that cannot be further broken down.  Numbers, identifiers, keywords, and operators will all be considered elements of our language alphabet.

Grammar rules describe how symbols are arranged to form *terms* in the concrete syntax.  A *term* is any text that satisfies the concrete syntax specified by grammar rules.  Similarly, a *language* is the smallest set of terms that satisfy a set of grammar rules.  It is quite useful to think of a language as a set and a grammar as a specifier for that set.  Using traditional set comprehension I can define a language, $L$, as:

$$L = \{s:string\; \mid\; G(s)\}$$

where $G$ is a predicate that is true when $s$ satisfies $L$'s grammar rules.

Let's define the concrete syntax for our first language that we will call `AE` for *Arithmetic Expressions*.  Terms in `AE` are defined by the following grammar:

$$\begin{align*}
t ::= & \NUM \mid t + t \mid t - t \\
\end{align*}$$

This is hopefully familiary.  Terms, $t$, in `AE` are either numbers or sums and products nested aribtrarily.

The language `AE` is the set of all strings that can be created with the grammar.  The strings:

{% highlight text %}
3  
1+5  
3+5-1  
{% endhighlight %}

are all terms in `AE`.  While the strings:

{% highlight text %}
A  
1*5  
A+B-C  
{% endhighlight %}

are not terms in `AE`.

Let's also defined *values* in `AE` as the numbers:

$$\begin{align*}
v ::= & \NUM \\
\end{align*}$$

The set of values is a subset of the set of terms.  It represents terms that result from interpretation.  The set of values define what we should get when we run an interpreter.  We will define this more formally later, but for now simply remember that $v$ is an interpretation result.

If we define the predicate $AE(t)$ to be true whenever $t$ satisfies _AE_'s grammar rules, then the language can be defined as a set:

$$AE = \{t:string\; \mid\; AE(s)\}$$

In `AE` we can express precisely three things: (i) a number; (ii) adding two expressions; and (iii) subtracting two expressions.  Not the most useful of languages, but we have to start somewhere.

Hopefully this discussion is review of your formal language theory or theory of computing studies.  If it isn't, I strongly suggest stopping now and studying some formal language theory.  In addition to being foundational, it is beautiful and useful.

## Inference Rules and Axioms

Knowing what `AE` looks like, let's now define how terms in `AE` are interpreted.  Before writing a Haskell interpreter, we should define formally the meaning of terms.  We will use inference rules for this purpose.

The first tells us how to interpret numbers:

$$\frac{}{\eval v = v}\; [NumE]$$

$\eval$ is the name of the interpretation function and this rule says calling $\eval$ on a value results in the value.  Remember that $v$ is a number and as such cannot be evaluated further.  What we're saying is that interpreting a constant number value gives back the constant number value.

Addition and subtraction are more interesting and hint at how all our interpreters will be structured.  The rule, $PlusE$ defines the interpretation of terms of the form $t_1+t_2$:

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 + t_2 = v_1+v_2}\; [PlusE]$$

$PlusE$'s antecedents and consequent work together to define a rescursive interpreter.  The first antecedent states that $\eval t_1 = v_1$ must be true before the consqeuent can be true.  But $v_1$ is a variable whose value is the result of calling $\eval$ on $t_1$.  In effect, this antecedent says that $v_1$ must be the result of $\eval t_1$.  The second antecendent behaves similarly for $t_2$ and $v_2$.  Both antecendents name the results of interpreting $t_1$ and $t_2$ $v_1$ and $v_2$ respectively.

Now that we know the results of evaluating $t_1$ and $t_2$, defining their sum is simple.  Values in `AE` are numbers, so we simply use Haskell's notion of addition to define the sum.  Thus the consequent is $\eval t_1 + t_2 = v_1 + v_2$.

We define subtraction similarly in the $MinusE$ rule:

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 - t_2 = v_1-v_2}\; [MinusE]$$

Understanding the structure of these rules before moving forward is vital.  They both define antecedents that effectively name the results of other calculations.  More specifically, *recursive* calculations.  When writing and defining interpreters, recursion is your best friend.  We needn't think now about calculating the values of $t_1$ and $t_2$, only that their values are calculated the same way all other values are calculated.

## Abstract Syntax

Now we have both a concrete syntax and an evaluation semantics for  `AE` defined using mathematical techniques.  We need to transform both definitions into Haskell structures to build our first interpreter for `AE`.  To do this, we will first define an *abstract syntax* for `AE`.

Concrete syntax is nice on the user side, but painful to work with directly when writing interpreters and compilers.  If you want to try an experiment, write a Haskell program that will interpret the `AE` language directly.  It can be done, but making changes or extending the language is quite laborious and potentially painful.

*Abstract Syntax* is a data structure representing parsed terms.  This data structure is far more useful than concrete syntax when writing interpreters and compilers. Programs are data and abstract syntax is the data structure that represents them.  An abstract syntax for `AE` written using a Haskell data type is:

{% highlight haskell %}
data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  deriving (Show,Eq)
{% endhighlight %}

where `Num`, `Plus` and `Minus` are the *constructors* of the data type `AE` that correspond with numbers, sums and differences in the `AE` concrete syntax.  We use the term constructor because all values of type `AE` are constructed with one of these operations.  By definition, the `AE` type contains all constructions using `Plus`, `Minus` and `Num`, and no more.

For example `(Num 1)` is the abstract syntax for 1. `(Plus (Num 1) (Num 3))` is the abstract syntax for `1+3`.  `(Minus (Plus (Num 3 (Num 5)) (Num 1))` is the abstract syntax for `3+5-1`.  For the abstract syntax to be effective, every term in the concrete syntax must have an associated term in the abstract syntax.  Remember the properties of relations you learned in your discrete math class?  They come in handy right now.  The relationship between concrete syntax and associated abstract syntax should be a total function. Specifically, concrete syntax terms should have exactly one abstract syntax value and all concrete syntax terms should be associated with some abstract syntax value.  Remember that errors are outputs.

From this point forward I will use TLA[^1] *AST* when referring to abstract syntax data structures.  AST literally means abstract syntax *tree*.  It turns out that Haskell data types naturally form trees and trees are perfect representations for abstract syntax.  I'll come back to this later, but for now remember that AST, abstract syntax, and abstract syntax tree refer to the same Haskell data type.

## Parsers

A *parser* is a program that translates concrete syntax into an AST.  It checks the syntax of its input, generates error messages if the syntax is bad, and generates an AST if the syntax is good. The signature of a parser for the _AE_ language is:

{% highlight haskell %}
parseAE :: String -> AE
{% endhighlight %}

Give `parse` a string and it will return an `AE` if it does not crash.  More generally, the parser for any language will be from a string to the datatype for that language's abstract syntax.

This course is not about building parsers.  Thankfully, Haskell provides a collection of tools for building parsers automatically.  I will provide examples using [Parsec](https://wiki.haskell.org/Parsec), a monadic parser combinator tool for writing and composing parsers.  After a bit of setup, Parsec is not difficult to extend to the languages we would like to discuss.  For more details there are a number of online tutorials as well as chapters in major Haskell texts.  For now, let's dive in a write a simple expression parser for the _AE_ language that we'll extend throughout the course.

First, the boilerplate that includes standard Haskell libraries for the kinds of parsers we want to write.  Include this at the top of every Haskell implementation where you will use a parser:

{% highlight haskell %}
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils
{% endhighlight %}

The `ParserUtils` has a few definitions that make defining parsers a bit simpler specific to this text.  You need to have `ParserUtils` in the same directory as your Haskell source or installed in a place where GHCI can find it.

The biggest thing `ParserUtils` provides is a standard lexer that we can use for all our projects.  A lexer is simply a parser that converts a stream of characters into a stream of tokens that are language parser can understand.  What we get from `ParserUtils` is collection of token parsers for reserved operations, parenthesis, numbers and white space.

The parser we will create, called `expr` is of type `Parser AE` - a parser that generates `AE` structures.  Our parser for `AE` will be constructed using `buildExpressionParser` on operators defined by `operators` and terms defined by `terms` that we're about to create:

{% highlight haskell %}
expr :: Parser AE
expr = buildExpressionParser operators term
{% endhighlight %}

First let's define the operators table that will be used to generate expressions.  The operators table is a list of lists that define individual operations.  Looking at the first operator definition tells us quite a bit about the operation:

{% highlight haskell %}
inFix "+" Plus AssocLeft
{% endhighlight %}

The `inFix` function is defined in `ParserUtils` to simplify defining inFix operations.  Command tells us that `t1 + t2` is an infix operation that is translated into `Plus (parseAE t1) (ParseAE t2)` and is left associative.  For now, we'll leave out details of what Parsec is doing.  It is sufficient to know that the `inFix` function creates a parser for `t1+t2` that returns an application of `Plus` to parsing `t1` and `t2`.  `AssocLeft` indicates the operator is left associative in the absence of parenthesis.  In other words:

{% highlight text %}
x + 3 - y + 7 == ((x+3) - y) + 7
{% endhighlight %}

There are two operations in _AE_, so there are two `inFix` applications in a list.  The list itself tells the parser generator that `+` and `-` have the same precedent.  We'll see examples with different precendent as our languages grow more complex.  `opTable` is a list of lists where each internal list contains operators of the same precedence.  `+` and `-` are in a list together indicating they are at the same precedence level.

[Expression Parser Documentation](https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Expr.html)  

{% highlight haskell %}
opTable = [ inFix "+" Plus AssocLeft,
            inFix "-" Minus AssocLeft ]
            ]
{% endhighlight %}

Now we have operators, but we don't have base terms for them to operate over.  In other words, we know how to parse `+` in `1+(3-2)`, but not the numbers or parens.  First, let's define a simple parser for numbers: 

{% highlight haskell %}
numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))
{% endhighlight %}

This Haskell expression uses the monadic `do` syntax to sequence operations.  In this case, the `integer` parser is called and its value stored in `i`.  Then an `Int` is extracted from `i` and returned in a `Num` constructor.  As the name `numExpr` implies, this is a parser for numbers.

A `term` in our language is either a parenthesized expression or a number.  This is exactly what the definition for `term` says.  The `<|>` operation is an or operation for parsers.  A `term` is either a parenthesized expression or an integer.  This capability for building parsers from smaller parsers is my reason for selecting Parsec.  Now that we have a basic parser in place, you should find it relatively easy to extend it in this manner to include new terms.

{% highlight haskell %}
term = parens lexer expr <|> numExpr
{% endhighlight %}

Looking back at the definition of `expr` puts the entire thing together.  `exper` is a parser that returns `AE` and is built from `opTable` defining operations over `terms`.

{% highlight haskell %}
expr :: Parser AE
expr = buildExpressionParser operators term
{% endhighlight %}

Following are several utility functions for calling your parser on strings and files.  `parseAE` takes a single argument, parses it, and either returns the resulting AST or displays the resulting error message.  `parseAEFile` does the same, except its input is taken from a file.

{% highlight haskell %}
parseAE = parseString expr

parseAEFile = parseFile expr
{% endhighlight %}

You can stop there if you want, but it is interesting to see what `parseString` and `parseFile` do.  Look at the `case` expression and its argument:

{% highlight haskell %}
parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

parseFile p file =
  do program <- readFile file
     case parse p "" program of
       Left e -> print e >> fail "parse error"
       Right r -> return r
{% endhighlight %}

The built in function `parse` is called with an argument representing the parser.  In our case, this is `expr`.  So `parse expr` is parsing using the definition we created earlier.  This is classic monadic programming.  We'll come back to this later, but keep in mind that parsers are structures that are run by the `parse` function to perform their work.

If the construction of the `AE` parser bothers you, it is save to simply use it for the time being.  We'll learn more about extending and writing parsers as we move along.  Don't get caught up in it now.

## Interpreters

An *interpreter* converts an abstract syntax term into a value.  Ass noted earlier, values represent valid interpretation results.  If an interpreter produces something besides a value, something went wrong.  Either the input is invalid, the interpreter is written wrong, or the language definition is problematic.  We'll talk about these issues later.  For now, let's look at a fun interpretr where everything works as it should.

How should the interpreter be constructed?  The data type defined for the abstract syntax gives is a big clue.  If the constructors from the data type define every possible AST element, then defining an interpreter for each element of the data type should do the trick.

First, lets get the signature down.  Our interpreter will take an element of `AE` and produce an element of `AE` that is a value.  Unfortunately, we can't capture value-ness in our signature, so we'll just say that the interpreter returns an `AE`:

{% highlight haskell %}
eval :: AE -> AE
{% endhighlight %}

Next, lets look at the `Num` constructor.  `(Num 3)` represents a constant `3` in our `AE` abstract syntax.  Without much thought it should be clear that `(Num 3)` evaluates to `(Num 3)`.  Numbers are values in `AE`, thus they should not be evaluated further.  Thankfully, this is exactly what our inference rule for evaluating numbers says if we remember that $v$ represents $\NUM$:

$$\frac{}{\eval v = v}\; [NumE]$$

Thus, `eval` case for `(Num n)` just returns its argument:

{% highlight haskell %}
eval :: AE -> AE
(Num n) = (Num n)
{% endhighlight %}

We now have an interpreter for literal numbers, but nothing more.

The `Plus` constructor represents a more interesting case.  We have a rule named $PlusE$ that defines interpretation of `t1+t2`:

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 + t_2 = v_1+v_2}\; [PlusE]$$

The inference rule is defined in terms of concrete rather than abstract syntax, so we have to do a bit of translation work.  $t_1+t_2$ translates quickly into `(Plus t1 t2)` and the rest of the computation follows from there. Given `(Plus t1 t2)` if `t1` evaluates to `(Num v1)` and `t2` evaluates to `(Num v2)` then `(Plus t1 t2)` evalautes to `(Num (v_1+v_2))`.  This is easily represented in Haskell using `let` to evaluate antecedents, pattern matching to bind `v1` and `v2`, add them in its body:

{% highlight haskell %}
(Plus t1 t2) = let (Num v1) = eval t1
                   (Num v2) = eval t2
               in (Num (v1 + v2))
{% endhighlight %}

Note the Haskell trick in the `let` clause.  If `eval t1` behaves as it should, then its result will be a number.  Using pattern matching the value of that number is bound to a variable and used directly in the sum.  This simplifies the evaluation just a bit.

The translation from inference rule to Haskell follows standard rule of thumb.  The `let` construct bindings manage the rule antecedents, performing evaluation and binding variables.  The let body is the consequent and evaluates the term.  This will not always be true, but will be in most circumstances.

Finally, The `Minus` constructor case is identical to the `Plus` constructor case except values are subtracted rather than added together.  For completeness, here is the subtraction case:

{% highlight haskell %}
(Minus t1 t2) = let (Num v1) = eval t1
                    (Num v2) = eval t2
                in (Num (v1 - v2))
{% endhighlight %}

Putting the cases together the following is an interpreter for `AE` that reduces every abstract syntax term to an abstract number:

{% highlight haskell %}
eval :: AE -> AE
eval (Num n) = (Num n)
eval (Plus t1 t2) = let (Num v1) = eval t1
                        (Num v2) = eval t2
                     in (Num (v1 + v2))
eval (Minus t1 t2) = let (Num v1) = eval t1
		                 (Num v2) = eval t2
                     in (Num (v1 - v2))
{% endhighlight %}

The interpreter follows a pattern that every interpreter we write will follow.  Each constructor from the AST definition has a case in the `eval` function that interprets that constructor.  This pattern and the accompanying `data` construct gives us three nice properties - completeness, determinsitic, and normalizing - that are quite useful.

Completeness says that every term constructed in `AE` will be interpreted by `eval`.  Can we prove that? As it turns out, the Haskell `data` construct gives us some exceptionally nice properties that give us nice properties for free, without direct proof. Bt definition, every value in the abstract syntax for `AE` is constructed with `Num`, `Plus`, and `Minus`.  There are no other constructions of type `AE`.   This is true of any type defined using `data` in Haskell.  All values of the defined type are constructed with its constructors.  A function that has a case for every constructor, is necessarily complete.

The Haskell `data` type definition mechanism is an example of *algebraic types* or *constructed types* that are a staple in fununctional languages and becoming increasinly common in traditional programming languages.  We call them *algebraic* because the individual constructors are *products* of values and the type itself is the *sum* of those products.  Don't worry too much about this now, we'll revisit it later when we define our own types.

Deterministic says that if we call `eval` on any term, we will get the same result.  This is an exceptionally important property as we don't want the same call to `eval` resulting in different values.  We know `AE` is deterministic because there is precisely one inference rule for interpreting each element of the concrete syntax.  In turn we know that `eval` is deterministic because there is precisely one case in the definition for each `AE` constructor.  Given `(Num n)` there is one rule and one `eval` case.  Given `Plus` there is one rule and one `eval` case.

Normalization says that a call to `eval` on any term constructed in `AE` will terminate.  Again, a pretty bold statement.  Can we prove it?  The elements of a Haskell `data` type specifically and algebraic types generally have are `well-ordered`.  In mathematics, well-ordered means that every subset of a set has a least element.  Getting from well-ordered to guaranteed termination takes a bit of work, but the gist is that components of a constructor are smaller than the constructor itself.  Each recursive call on the parts of a term is made on a smaller term.  Consider `eval (Plus t1 t2)` where `t1` and `t2` can be viewed as smaller than `(Plus t1 t2)`.  Because every set of `AE` terms has a least element, pulling a term apart into its peices will eventually get to that least element.

The least elements of `AE` are those constructed by `(Num n)`.  When the interpreter reaches the least element, it cannot go further and terminates.  Every call to `eval` gets closer to the least element.  Note that those those least elements are what we defined as values.  This is not a conicidence.  Not only does `AE` terminate, it always terminates with a value.  Said it terms we all understand, the `AE` interpreter never crashes for any element of `AE1`.

## All Together Now

Before we say more about `AE`, lets put all the pieces together into a single definition.  Definition of `AE` is now complete sytactically, semantically and operationally:

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

We implemented a parser from the grammar and an interpreter from the inference rules:

{% highlight haskell %}
eval :: AE -> AE
eval (Num n) = (Num n)
eval (Plus t1 t2) = let (Num v1) = eval t1
                        (Num v2) = eval t2
                     in (Num (v1 + v2))
eval (Minus t1 t2) = let (Num v1) = eval t1
		                 (Num v2) = eval t2
                     in (Num (v1 - v2))
{% endhighlight %}

Given the parser and interpreter for `AE`, we can now define a language interpreter, `interp`, that puts everything together:

{% highlight haskell %}
interp :: String -> AE
interp = eval . parseAE
{% endhighlight %}

In words, `interp` is the composition of `parseAE` and `eval`.  This notation says that `parse` will be called first and the output passed to `eval`.  If `parseAE` throws an error, `interp` will terminate without passing a value to `eval`.

## Testing

Haskell provides [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck.html) a domain-specific language widely used for testing and adapted to many other languages.  Like Parsec, QuickCheck takes a bit of time to learn, but is a worthwhile tool to have available.

What QuickCheck does is rather ingenious.  Let's assume that we've written a function and would like to test that a property holds for a set of arbitrary inputs to that function.  As a concrete example, let's define a function `f x = x * x` and a property `p v = v >= 0`.  `f` is the *function under test* and `p` is the *property being tested*.  Note the signatures of `f` and `p`:

{% highlight haskell %}
f::Int -> Int
f x = (x * x)

p::Int -> Bool
p v = v >= 0
{% endhighlight %}

If we want to say that `p` holds for all `f`, we would prove the following property:

$$\forall x:Int \cdot (p (f x))$$

This tells us that for every integer $x$, $g$ holds for $f x$.  (This is actually an easy proof, but for discussion let's assume we  have only testing available to us.)

To test `p` we can easily write a function:

{% highlight haskell %}
\x -> (p (f x))
{% endhighlight %}

and call that function with numerous random values of `x`.  The more  tests run, the more confident we are in the result.

This is precisely what QuickCheck does.  Specifically:

{% highlight haskell %}
quickCheck (\x -> (p (f x)))
{% endhighlight %}

will generate 100 arbitrary `Int` values and call `(\x -> (p (f x)))` on each.  In this case, the square of every integer is greater than or equal to zero, and we get the result:

{% highlight haskell %}
+++ OK, passed 100 tests.
{% endhighlight %}

Choosing a test that is false for some integers:

{% highlight haskell %}
p' v = (v /= 100)
{% endhighlight %}

causes QuickCheck to display a counterexample:

{% highlight haskell %}
quickCheck (\x-> (p' (f x)))
*** Failed! Falsifiable (after 18 tests and 1 shrink): 
10
{% endhighlight %}

In this case, 100 random tests were enough to discover that 100 is the square of 10 and the test condition is thus false.

Part of the magic of QuickCheck is the ability to generate random test cases.  In this example, QuickCheck must generate random `Int` values.  Because `Int` is a commonly used type, the ability to generate random `Int` values is built-in to QuickCheck.  In contrast `AE` is a custom, user-defined type that is not built-in.

QuickCheck requires that the domain of any function it is called on be an instance of class `Arbitrary` providing a function `arbitrary` that generates an arbitrary value of that type.  We must make  `AE` and instance of `Arbitrary`:

{% highlight haskell %}
instance Arbitrary AE where
  arbitrary = ...
{% endhighlight %}

The type signature of `arbitrary` tells is what direction we need to follow:

{% highlight haskell %}
arbitrary :: Arbitrary a => Gen a
{% endhighlight %}

The `arbitrary` operator is of type `Gen a` where `a` is also an instance of `Arbitrary`.  `Gen` is a *generator*.  

Let's start by generating an arbitrary `Num`.  The `genNum` function does this by returning an arbitrary number:

{% highlight haskell %}
genNumFirst =
  do t <- arbitrary
     return (Num t)
{% endhighlight %}

`genNumFirst` calls `arbitrary`, stores the result in `t`, and returns `(Num t)`.  Now we see `arbitrary` at work.  The `return` statement constrains `t` to be of type `Int`.  As noted, `Int` is an instance of `Arbitrary` and has an associated `arbitrary` function.  `genNumFirst` thus generates arbitrary values of `Num` by lifting arbitrary `Int` values.

Suppose we don't want to look at all `Int` values, but only a range.  The built in `choose` operation does exactly this by restricting the range of `arbitrary`.  `genNum` uses `choose` to generate arbitrary `Num` values over a subrange of `Int`:

{% highlight haskell %}
genNum =
  do t <- choose (0,100)
     return (Num t)
{% endhighlight %}

We could stop now and define `AE` to be an instance of `Arbitrary` as follows:

{% highlight haskell %}
instance Arbitrary AE where
  arbitrary = genNum
{% endhighlight %}

Of course our arbitrary values of `AE` are not so arbitrary as they will only include number values ranging from 0 to 100.  We need some terms that involve `Plus` and `Minus`.

Let's start with `Plus` and think about how we might generate arbitrary terms.  The simplest thing would be to generate `Plus` terms over arbitrary numbers:

{% highlight haskell %}
genPlusFirst =
  do t1 <- genNum
     t2 <- genNum
     return (Plus t1 t2)
{% endhighlight %}

This generator gets us part way there, but only generates terms of the form `(Plus v1 v2)` where `v1` and `v2` are arbitrary `Num` values.  A term of the form `(Plus (Plus (Num 1) (Num 2)) (Num 3))` would never be generated.  What we need is to generate terms recursively in the `Plus` generator.  Unfortunately we don't have a generator for terms yet, so we'll have to set this aside and just use `genPlusFirst`.

`Minus` is identical to `Plus` modulo the constructor name, so we'll defer that generator as well assuming that we have `genMinusFirst` available.

Now let's put our generators together to generate a more complete set of terms using `genABE`.  The simplest way to compose generators for our individual terms is to use the `oneof` operator that chooses one generator arbitrarily from a list of generators:

{% highlight haskell %}
genABEFirst = oneof [genNum,genPlus,genMinus]
{% endhighlight %}

If we use `genABEFirst` we arbitrarily generate a `Num`, `Plus` or `Minus`.  Getting closer, but not quite there.  Remember that `genPlusFirst` and `genMinusFirst` only build terms from `Num`.  With a generator for `ABE` we can fix that now by using `genABEFirst` where we used `genNum`:

{% highlight haskell %}
genPlusSecond =
  do s <- genAEFirst
     t <- genAEFirst
     return (Plus s t)
{% endhighlight %}

If we do the same thing with `genMinusSecond` we will now generate arbitrary `ABE` terms.  Unfortunately, those arbitrary `ABE` terms are  of *arbitrary size*.  Mathematically this is perfectly fine as really big things don't bother us.  Pragmatically this is not perfectly fine as `genABEFirst` can now generate arbitrarily huge test cases.  If you want to see what this does, use `sample` to generate a few test cases and be prepared to terminate your Haskell process!

How do we make our generator for `AE` not go into the weeds generating huge test cases?  The easiest way is to add a size limit to the `AE` generator function by adding a size parameter to `genAE` that is decremented on each call to `genAE`.

Here's all the code for our generators and making `AE` and instance of `Abitrary` in one place:

{% highlight haskell %}
instance Arbitrary AE where
  arbitrary =
    sized $ \n -> genAE (rem n 10)

genNum =
  do t <- choose (0,100)
     return (Num t)

genPlus n =
  do s <- genAE n
     t <- genAE n
     return (Plus s t)

genMinus n =
  do s <- genAE n
     t <- genAE n
     return (Minus s t)

genAE :: Int -> Gen AE
genAE 0 = 
  do term <- genNum
     return term
genAE n =
  do term <- oneof [genNum,(genPlus (n-1)),(genMinus (n-1))]
     return term
{% endhighlight %}

## Discussion

`AE` is a rather silly language that is less powerful than one of those bank calculators you get when you open a checking account.  It adds and subtracts numbers.  However, we need to start somewhere and `AE` is good for that.

### Complete, Deerministic, and Normalizing

We said earlier that `eval` is complete, deterministic and normalizing.  Complete in that any element of `AE` can be interpretted, deterministic in that there is only one way to interpret any element of `AE`, and normalizing in that every interpretation of and `AE` element terminates.

Unfortunately, these nice properties result from `AE` being such a trivial language.  Completeness and deterministic are properties something we will seek to preserve as we add features.  However,  normalizing will prove problematic when we add recursion and looping.  Some programs such as operating systems shouldn't terminate anyway.  Certainly we would like to ensure that no programs written in our languages crash, but that one will unfortunately go away in the next chapter.

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

This if no case applies, then the terms are not equal.  So, $t_1+t_2$ will never be equal to $t_1'-t_2'$.

## Exercises

1. Add multiplication and division to `AE`.  Recall that `div` is the Haskell function for integer division.  Do we lose any of our nice properties by doing this?
2. Rewrite `AE` replacing the `Plus` and `Minus` constructors in the AST with a single constructor `Binop op t1 t2` where `op` is the represented binary operation.  You will need to change the parser to generate `Binop` rather than `Plus` and `Minus`.  Think carefully about what `op` should be.  If you do it right, you should be able to add any operator to `AE` by simply changing the parser.

[^1]: Three Letter Acronym
