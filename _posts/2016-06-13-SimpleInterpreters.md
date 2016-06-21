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
\newcommand\eval{\mathsf{calc}\;}
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

$$\frac{}{\calc v = v}\; [NumE]$$

$\calc$ is the name of the interpretation function and this rule says calling $\calc$ on a value results in the value.  Remember that $v$ is a number and as such cannot be evaluated further.  What we're saying is that interpreting a constant number value gives back the constant number value.

Addition and subtraction are more interesting and hint at how all our interpreters will be structured.  The rule, $PlusE$ defines the interpretation of terms of the form $t_1+t_2$:

$$\frac{\calc t_1 = v_1,\; \calc t_2 = v_2}{\calc t_1 + t_2 = v_1+v_2}\; [PlusE]$$

$PlusE$'s antecedents and consequent work together to define a rescursive interpreter.  The first antecedent states that $\calc t_1 = v_1$ must be true before the consqeuent can be true.  But $v_1$ is a variable whose value is the result of calling $\calc$ on $t_1$.  In effect, this antecedent says that $v_1$ must be the result of $\calc t_1$.  The second antecendent behaves similarly for $t_2$ and $v_2$.  Both antecendents name the results of interpreting $t_1$ and $t_2$ $v_1$ and $v_2$ respectively.

Now that we know the results of evaluating $t_1$ and $t_2$, defining their sum is simple.  Values in `AE` are numbers, so we simply use Haskell's notion of addition to define the sum.  Thus the consequent is $\calc t_1 + t_2 = v_1 + v_2$.

We define subtraction similarly in the $MinusE$ rule:

$$\frac{\calc t_1 = v_1,\; \calc t_2 = v_2}{\calc t_1 - t_2 = v_1-v_2}\; [MinusE]$$

Understanding the structure of these rules before moving forward is vital.  They both define antecedents that effectively name the results of other calculations.  More specifically, *recursive* calculations.  When writing and defining interpreters, recursion is your best friend.  We needn't think now about calculating the values of $t_1$ and $t_2$, only that their values are calculated the same way all other values are calculuated.

## Abstract Syntax

Now we have both a concrete syntax and a calculation semantics for  `AE` defined using mathematical techniques.  We need to transform both definitions into Haskell structures to build our first interpreter for `AE`.  To do this, we will first define an *abstract syntax* for `AE`.

Concrete syntax is nice on the user side, but painful to work with directly when writing interpreters and compilers.  If you want to try an experiment, write a Haskell program that will interpret the `AE` language directly.  It can be done, but making changes or extending the language is quite laborious and potentially painful.

*Abstract Syntax* is a data structure representing parsed terms.  This data structure is far more useful than concrete syntax when writing interpreters and compilers. Programs are data and abstract syntax is the data structure that represents them.  An abstract syntax for `AE` written using a Haskell data type is:

{% highlight haskell %}
data AE = Num Int
        | Plus AE AE
        | Minus AE AE
          deriving (Show,Eq)
{% endhighlight %}

where `Num`, `Plus` and `Minus` are the *constructors* of the data type `AE` that correspond with numbers, sums and differences in the `AE` concrete syntax.  We use the term constructor because all values of type `AE` are constructed with one of these operations.  By definition, the `AE` type contains all constructions using `Plus`, `Minus` and `Num`, and no more.

For example `(Num 1)` is the abstract syntax for 1. `(Plus (Num 1) (Num 3))` is the abstract syntax for `1+3`.  `(Minus (Plus (Num 3 (Num 5)) (Num 1))` is the abstract syntax for `3+5-1`.  For the abstract syntax to be effective, every term in the concrete syntax must have an associated term in the abstract syntax.  Remember the properties of relations you learned in your discrete math class?  They come in handy right now.  The relationship between concrete syntax and associated abstract syntax should be a total function. Specifically, concrete syntax terms should have exactly one abstract syntax value and all concrete syntax terms should be associated with some abstract syntax value.  Remember that errors are outputs.

From this point forward I will use TLA[^1] *AST* when referring to abstract syntax data structures.  AST literally means abstract syntax *tree*.  It turns out that Haskell data types naturally form trees and trees are perfect representations for abstract syntax.  I'll come back to this later, but for now remember that AST, abstract syntax, and abstract syntax tree refer to the same Haskell data type.

## Parsers

A *parser* is a program that translates concrete syntax into an AST.  It checks the syntax of its input, generates error messages if the syntax is bad, and generates an AST if the syntax is good. The signature of a parser for the _AE_ language is:

{% highlight haskell %}
parse :: String -> AE
{% endhighlight %}

Give `parse` a string and it will return an `AE` if it does not crash.  More generally, the parser for any language will be from a string to the datatype for that language's abstract syntax.

This course is not about building parsers.  Thankfully, Haskell provides a collection of tools for building parsers automatically.  I will provide examples using [Parsec](https://wiki.haskell.org/Parsec), a monadic parser combinator tool for writing and composing parsers.  After a bit of setup, Parsec is not difficult to extend to the languages we would like to discuss.  For more details there are a number of online tutorials as well as chapters in major Haskell texts.  For now, let's dive in a write a simple expression parser for the _AE_ language that we'll extend throughout the course.

First, the boilerplate that includes standard Haskell libraries for the kinds of parsers we want to write.  Include this at the top of every Haskell implementation where you will use a parser:

{% highlight haskell %}
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
{% endhighlight %}

The definition `tokens` constructs a data structure representing a _lexer_ that converts a string into a sequence of tokens for parsing.  Here we tell Parsec that we will create a language using `javaStyle` as a basis adding `+` and `-` as reserved operations:

{% highlight haskell %}
tokenDef =
  javaStyle { Token.reservedOpNames = [ "+","-"] }
{% endhighlight %}

The lexer itself is created using the `makeTokenParser` called on the `tokens` structure we just created:

{% highlight haskell %}
lexer = Token.makeTokenParser tokenDef
{% endhighlight %}

What we get is collection of token parsers for reserved operations, parenthesis, numbers and white space:

{% highlight haskell %}
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
{% endhighlight %}

Now we can create the parser itself. The parser, called `expr` is of  type `Parser AE`.  Note that this is a data structure, not a function.  As is typical for monadic things it must be run later.  More on that as we get deeper into interpreter.  For right now, our parser for _AE_ will be constructed using `buildExpressionParser` on operators `operatiors` and `terms` there we're about to define.

{% highlight haskell %}
expr :: Parser AE
expr = buildExpressionParser operators term
{% endhighlight %}

First let's define the operators table that will be used to generate expressions.  The operators table is a list of lists that define individual operations.  Looking at the first operator definition tells us quite a bit about the operation:

{% highlight haskell %}
Infix (reservedOp "+" >> return (Plus )) AssocLeft
{% endhighlight %}

`x + y` is an infix operation that is translated into `Plus x y` and is left associative.  For now, we'll leave out details.  It is sufficient to know that the `Infix` constructor creates the operation from a parser for `+` that returns an application of `Plus`.  `AssocLeft indicates the operator is left associative in the absence of parenthesis.  In other words:

{% highlight text %}
x + 3 - y + 7 == ((x+3) - y) + 7
{% endhighlight %}

There are two operations in _AE_, so there are two `Infix` applications in a list.  The list itself tells the parser generator that `+` and `-` have the same precedent.  We'll see examples with different precendent as our languages grow more complex.

[Expression Parser Documentation](https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Expr.html)  

{% highlight haskell %}
opTable = [ [Infix (reservedOp "+" >> return Plus) AssocLeft,
             Infix (reservedOp "-" >> return Minus) AssocLeft ]
            ]
{% endhighlight %}

Now we have operators, but we don't have base terms for them to operate over.  In other words, we know how to parse `+` in `1+(3-2)`, but not the numbers or parens.  First, let's define a simple parser for numbers: 

{% highlight haskell %}
numExpr :: Parser AE
numExpr = do i <- integer
             return (Num (fromInteger i))
{% endhighlight %}

This Haskell expression uses the monadic `do` syntax to sequence operations.  In this case, the `integer` parser is called and its value stored in `i`.  Then an `Int` is extracted from `i` and returned in a `Num` constructor.  As the name `numExpr` implies, this is a parser for numbers.

A `term` in our language is either a parenthesized expression or a number.  This is exactly what the definition for `term` says.  The `<|>` operation is an or operation for parsers.  A `term` is either a parenthesized expression or an integer.  This capability for building parsers from smaller parsers is my reason for selecting Parsec.  Now that we have a basic parser in place, you should find it relatively easy to extend it in this manner to include new terms.

{% highlight haskell %}
term = parens expr <|> numExpr
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

## Interpreters

An *interpreter* converts an abstract syntax term into a value.  Ass noted earlier, values represent valid interpretation results.  If an interpreter produces something besides a value, something went wrong.  Either the input is invalid, the interpreter is written wrong, or the language definition is problematic.  We'll talk about these issues later.  For now, let's look at a fun interpretr where everything works as it should.

How should the interpreter be constructed?  The data type defined for the abstract syntax gives is a big clue.  If the constructors from the data type define every possible AST element, then defining an interpreter for each element of the data type should do the trick.

First, lets get the signature down.  Our interpreter will take an element of `AE` and produce an element of `AE` that is a value.  Unfortunately, we can't capture value-ness in our signature, so we'll just say that the interpreter returns an `AE`:

{% highlight haskell %}
calc :: AE -> AE
{% endhighlight %}

Next, lets look at the `Num` constructor.  `(Num 3)` represents a constant `3` in our `AE` abstract syntax.  Without much thought it should be clear that `(Num 3)` evaluates to `(Num 3)`.  Numbers are values in `AE`, thus they should not be evaluated further.  Thankfully, this is exactly what our inference rule for calculating numbers says if we remember that $v$ represents $\NUM$:

$$\frac{}{\calc v = v}\; [NumE]$$

Thus, `calc` case for `(Num n)` just returns its argument:

{% highlight haskell %}
calc :: AE -> AE
(Num n) = (Num n)
{% endhighlight %}

We now have an interpreter for numbers, but nothing more.

The `Plus` constructor represents a more interesting case.  We have a rule named $PlusE$ that defines interpretation of `t1+t2`:

$$\frac{\calc t_1 = v_1,\; \calc t_2 = v_2}{\calc t_1 + t_2 = v_1+v_2}\; [PlusE]$$

The inference rule is defined in terms of concrete rather than abstract syntax, so we have to do a bit of translation work.  $t_1+t_2$ translates quickly into `(Plus t1 t2)` and the rest of the computation follows from there. Given `(Plus t1 t2)` if `t1` evaluates to `(Num v1)` and `t2` evaluates to `(Num v2)` then `(Plus t1 t2)` evalautes to `(Num (v_1+v_2))`.  This is easily represented in Haskell using `let` to calculate antecedent values, pattern matching to bind `v1` and `v2`, add them in its body:

{% highlight haskell %}
(Plus t1 t2) = let (Num v1) = calc t1
                   (Num v2) = calc t2
               in (Num (v1 + v2))
{% endhighlight %}

Note the Haskell trick in the `let` clause.  If `calc t1` behaves as it should, then its result will be a number.  Using pattern matching the value of that number is bound to a variable and used directly in the sum.  This simplifies the calculation just a bit.

The translation from inference rule to Haskell follows standard rule of thumb.  The `let` construct bindings manage the rule antecedents, performing calculations and binding variables.  The let body is the consqeuent and calculates the result of interpretation.  This will not always be true, but will be in most circumstances.

Finally, The `Minus` constructor case is identical to the `Plus` constructor case except values are subtracted rather than added together.  For completeness, here is the subtraction case:

{% highlight haskell %}
(Minus t1 t2) = let (Num v1) = calc t1
                    (Num v2) = calc t2
                in (Num (v1 - v2))
{% endhighlight %}

Putting the cases together the following is an interpreter for `AE` that reduces every abstract syntax term to an abstract number:

{% highlight haskell %}
calc :: AE -> AE
calc (Num n) = (Num n)
calc (Plus t1 t2) = let (Num v1) = calc t1
                        (Num v2) = calc t2
                     in (Num (v1 + v2))
calc (Minus t1 t2) = let (Num v1) = calc t1
		                 (Num v2) = calc t2
                     in (Num (v1 - v2))
{% endhighlight %}

The interpreter follows a structure that every interpreter we write and virtually every interpreter written in a functional language will follow.

---

I made a pretty bold statement when I said the interpreter reduces *every* abstract syntax term to a value.  No proofs here so how can that be the case?  As it turns out, the Haskell `data` construct and algebraic types in general give us some exceptionally nice properties that help with these things.

Every value in the abstract syntax is constructed with `Num`, `Plus`, and `Minus`.  For any algebraic type regardless of language all values of that type are constructed with its defined constructors.  Look carefully at the `calc` function.  There is one case for every constructor.  If all values are build using constructors and every constructor has an interpretation, then our interpreter does in fact cover all syntactic elements of `AE`.

Knowing that we have an interpretation for every AST element, we need to know that each case terminates.  Another thing we get from a Haskell `data` construct and algebraic types in general is that the components of a constructor are smaller than the constructor.  This means that each recursive call on the parts of an expression is made on a smaller expression.  One cannot get smaller forever, thus eventually `calc` is called on `Num` causing the interpreter to terminate.

## Inference Rules and Axioms

$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeof}\;}
\newcommand\interp{\mathsf{interp}\;}
\newcommand\eval{\mathsf{calc}\;}
$$

$$\frac{}{\calc v = v}\; [NumE]$$

$$\frac{\calc t_1 = v_1,\; \calc t_2 = v_2}{\calc t_1 + t_2 = v_1+v_2}\; [PlusE]$$

$$\frac{\calc t_1 = v_1,\; \calc t_2 = v_2}{\calc t_1 - t_2 = v_1-v_2}\; [MinusE]$$

## All Together Now

Definition of `AE` is complete both semantically and operationally.  The language definition.  First we define a concrete syntax for terms:

$$\begin{align*}
t ::= & \mathsf{NUM} \mid t + t \mid t - t \\
\end{align*}$$

and syntactic definition of values:

$$\begin{align*}
v ::= & \mathsf{NUM} \\
\end{align*}$$

Then we define basic inference rules formally defining how `AE` is interpreted:

$$\frac{t_1 \Downarrow v_1\;\; t_2 \Downarrow v_2}{(t_1 + t_2)\Downarrow v_1+v_2}\; [PlusE]$$

$$\frac{t_1 \Downarrow v_1\;\; t_2 \Downarrow v_2}{(t_1 - t_2\Downarrow v_1-v_2}\; [MinusE]$$

We implement a parser from the grammar and an interpreter from the inference rules:

{% highlight haskell %}
calc :: AE -> Int
calc (Num x) = x
calc (Plus l r) = (interp l) + (interp r)
calc (Minus l r) = (interp l) - (interp r)
{% endhighlight %}

Given the parser and interpreter for `AE`, we can now define a language evaluator that puts everything together:

{% highlight haskell %}
eval = calc . parse
{% endhighlight %}

In words, `eval` is the composition of `parse` and `calc`.  This notation says that `parse` will be called first and the output passed to `calc`.  If `parse` throws an error, `eval` will terminate without passing a value to `calc`.

## Attic

{% highlight haskell %}
data (Show a,Eq a) => Expr a
      = Val a
      | Add (Expr a) (Expr a)
      | Sub (Expr a) (Expr a)
        deriving (Eq,Show)
{% endhighlight %}

A language representing the states of a stop light, the *Stop Light Language* (`SLL`), is finite:

{% highlight text %}
<SLL> ::= <C> + <C>
<C> ::= red | green | yellow
{% endhighlight %}

where _<C> + <C>_ is a pair of stoplight colors representing both sides of the light.  _SLL_ does not refer to itself in its own definition and is finite.

The strings:

{% highlight text %}
red+red  
green+yellow  
yellow+yellow  
{% endhighlight %}

are all terms in _SLL_.  In contrast:

{% highlight text %}
red+red+red  
green  
yellow+blue  
{% endhighlight %}

are not terms in _SSL_.

Similarly, an abstract syntax for `SLL` is:

{% highlight haskell %}
data C = red | green | yellow deriving (Show,Eq)
data SSL = Sum C C deriving (Show,Eq)
{% endhighlight %}

The `SSL` abstract syntax is interesting in that it defines `C` then builds `SSL` from it.  Will this data type work just as well?

{% highlight haskell %}
data SSL = red | green | yellow | Sum SSL SSL deriving (Show,Eq)
{% endhighlight %}

---

$$\frac{}{(\mathsf{Num}\; n \Downarrow n)}\; [NumE]$$

$$\frac{t_1 \Downarrow v_1\;\; t_2 \Downarrow v_2}{(\mathsf{Plus}\; t_1\; t_2)\Downarrow v_1+v_2}\; [PlusE]$$

$$\frac{t_1 \Downarrow v_1\;\; t_2 \Downarrow v_2}{(\mathsf{Minus}\; t_1\; t_2\Downarrow v_1-v_2}\; [MinusE]$$

---

[^1]: Three Letter Acronym
