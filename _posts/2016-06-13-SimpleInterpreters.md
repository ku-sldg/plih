---
layout: frontpage
title: Simple Expression Interpreters
use_math: true
category: chapter
---

# Simple Expression Interpreters

Description goes here...

## Concrete Syntax

*Concrete Syntax* is the textual language written by programmers as input to compilers and interpreters.  When you think if a programming language, you probably think first of its concrete syntax.  Concrete syntax is virtually always described by a *grammar* consisting of an *alphabet* and a collection of *grammar rules*.

The alphabet is a collection of atomic symbols comprising a language.  The English alphabet is a good example.  Alphabets can be much more sophisticated and include other characters and words.  For our purposes, the alphabet will be atomic constructs - things that cannot be further broken down.  Numbers, identifiers, keywords, and operators will all be considered elements of our language alphabet.  As a convention throughout the text, I will use `<num>` to represent numbers and `<id>` to represent identifiers rather than define formal grammars for both.

Grammar rules describe out symbols are arranged to form *terms* in the concrete syntax.  A *term* is any text that satisfies the concrete syntax specified by grammar rules.  Similarly, *language* is the set of terms that satisfy a set of grammar rules.  It is quite useful to think of a language as a set and a grammar as a specifier for that set.  Using traditional set comprehension I can define a language, $L$, as:

$$L = \{s:string\; |\; G(s)\}$$

where $G$ is a predicate that is true when $s$ satisfies $L$'s grammar rules.

Let's define the concrete syntax for our first language that we will call _AE_ for *Arithmetic Expressions*:

{% highlight text %}
<AE> ::= <num>
| <AE> + <AE>
| <AE> - <AE>
{% endhighlight %}

The language _AE_ is the set of all strings that can be created with the grammar.  The strings:

{% highlight text %}
3  
1+5  
3+5-1  
{% endhighlight %}

are all terms in _AE_.  While the strings:

{% highlight text %}
A  
1*5  
A+B-C  
{% endhighlight %}

are not terms in _AE_.

If we define the predicate $AE(s)$ to be true whenever $s$ satisfies _AE_'s grammar rules, then the language can be defined as:

$$AE = \{s:string\; |\; AE(s)\}$$

Because _AE_ appears in it's own definition, the _AE_ grammar is recursive and the _AE_ language is infinite.  As it should be.  I'll come back to this later.

In _AE_ we can express precisely three things: (i) a number; (ii) adding two expressions; and (iii) subtracting two expressions.  Not the most useful of languages, but we have to start somewhere.

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

Hopefully this discussion is review of your formal language theory or theory of computing studies.  If it isn't, I strongly suggest stopping now and studying some formal language theory.  In addition to being foundational, it is beautiful and useful.

## Abstract Syntax

Concrete syntax is nice on the user side, but painful to work with directly when writing interpreters and compilers.  If you want to try an experiment, write a Haskell program that will interpret the _AE_ language directly.  It can be done, but making changes or extending the language is quite laborious and potentially painful.

*Abstract Syntax* is a data structure representing parsed terms.  This data structure is far more useful than concrete syntax when writing interpreters and compilers. Instead of dealing with concrete syntax, a *parser* is typically used to translate concrete syntax into abstract syntax.  An abstract syntax for _AE_ written using a Haskell data type is:

{% highlight haskell %}
data AE = Num Int
        | Plus AE AE
        | Minus AE AE
          deriving (Show,Eq)
{% endhighlight %}

where `Num`, `Plus` and `Minus` are the *constructors* of the data type `AE`. This means that all values of type `AE` are constructed with one of these operations.  For example `(Num 1)` is the value 1 in `AE` while `(Plus (Num 1) (Num 3))` is the abstract syntax for `1+3`.

A term in the abstract syntax is anything we can create using this data type.  Every term in the concrete syntax must have an associated term in the abstract syntax.  Remember the properties of relations you learned in your discrete math class?  They come in handy right now.  Your parser should be a total function. (All concrete syntax terms should parse to only one abstract syntax term and all concrete syntax terms should parse to some output value.)  Remember that errors are outputs.

{% highlight haskell %}
parse 3 = (Num 3)
parse 1+5 = (Plus (Num 1) (Num 5))
parse 3+5-1 = (Minus (Plus (Num 3) (Num 5)) (Num 1))
{% endhighlight %}

are abstract syntax representations of the `AE` terms shown previously.

Similarly, an abstract syntax for `SLL` is:

{% highlight haskell %}
data C = red | green | yellow deriving (Show,Eq)
data SSL = Sum C C deriving (Show,Eq)
{% endhighlight %}

The `SSL` abstract syntax is interesting in that it defines `C` then builds `SSL` from it.  Will this data type work just as well?

{% highlight haskell %}
data SSL = red | green | yellow | Sum SSL SSL deriving (Show,Eq)
{% endhighlight %}

From this point forward I will use TLA[^1] *AST* when referring to abstract syntax data structures.  AST literally means abstract syntax *tree*.  It turns out that Haskell data types naturally form trees and trees are perfect representations for abstract syntax.  I'll come back to this later, but for now remember that AST, abstract syntax, and abstract syntax tree refer to the same thing.

## Parsers

A *parser* translates concrete syntax into an AST.  It checks the syntax of its input, generates error messages if the syntax is bad, and generates an AST if the syntax is good. The signature of a parser for the _AE_ language is `parse :: String -> AE`.  More generally, the parser for any language will be from a string to the datatype for that language's abstract syntax.

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

An *interpreter* converts a concrete syntax term into a *value*
where a *value* is a special term that cannot be interpreted further.  Values represent valid interpretation results.  If an interpreter produces something besides a value, something went wrong.  Either the input is invalid, the interpreter is written wrong, or the language definition is problematic.

Let's look at the fun case first when everything works as it should.  The following is an interpreter for *AE* that reduces every abstract syntax term to an integer value:

{% highlight haskell %}
calc :: AE -> Int
calc (Num x) = x
calc (Plus l r) = (interp l) + (interp r)
calc (Minus l r) = (interp l) - (interp r)
{% endhighlight %}

The interpreter follows a structure that every interpreter we write and virtually every interpreter written in a functional language will follow.

I made a pretty bold statement when I said the interpreter reduces *every* abstract syntax term to a value.  No proofs here so how can that be the case?  As it turns out, the Haskell `data` construct and algebraic types in general give us some exceptionally nice properties that help with these things.

Every value in the abstract syntax is constructed with `Num`, `Plus`, and `Minus`.  For any algebraic type regardless of language all values of that type are constructed with its defined constructors.  Look carefully at the `calc` function.  There is one case for every constructor.  If all values are build using constructors and every constructor has an interpretation, then our interpreter does in fact cover all syntactic elements of `AE`.

Knowing that we have an interpretation for every AST element, we need to know that each case terminates.  Another thing we get from a Haskell `data` construct and algebraic types in general is that the components of a constructor are smaller than the constructor.  This means that each recursive call on the parts of an expression is made on a smaller expression.  One cannot get smaller forever, thus eventually `calc` is called on `Num` causing the interpreter to terminate.

## Inference Rules and Axioms

We can also define an interpreter formally using _inference rules_.  An inference rule is a deceptively simple mathematical construct that is used to define formal systems.  Rules are of the form:

$$\frac{A_0, A_1,\ldots\,A_n}{C}$$

where $A_k$ are _antecedents_ and $C$ is the _consequent_.  The inference rule says simple that when all of the $A_0$-$A_n$ are true, then $C$ follows.  An inference rule with on antecedents is called an _axiom_ because its consequent depends on nothing an may be asserted true immediately.

$$\frac{}{(\mathsf{Num}\; n \rightarrow n)}\; [NumE]$$

$$\frac{t_1 \rightarrow v_1\;\; t_2 \rightarrow v_2}{(\mathsf{Plus}\; t_1\; t_2)\rightarrow v_1+v_2}\; [PlusE]$$

$$\frac{t_1 \rightarrow v_1\;\; t_2 \rightarrow v_2}{(\mathsf{Minus}\; t_1\; t_2\rightarrow v_1-v_2}\; [MinusE]$$

## All Together Now

Given the parser and interpreter for `AE`, we can now define a language evaluator that puts everything together:

{% highlight coq %}
eval = calc . parse
{% endhighlight %}

In words, `eval` is the composition of `parse` and `calc`.  This notation says that `parse` will be called first and the output passed to `calc`.  If `parse` throws an error, `eval` will terminate without passing a value to `calc`.

{% highlight haskell %}
data (Show a,Eq a) => Expr a
      = Val a
      | Add (Expr a) (Expr a)
      | Sub (Expr a) (Expr a)
        deriving (Eq,Show)
{% endhighlight %}

[^1]: Three Letter Acronym
