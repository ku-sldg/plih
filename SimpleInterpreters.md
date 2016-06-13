---
layout: frontpage
title: Simple Expression Interpreters
use_math: true
---

# Simple Expression Interpreters

Description goes here...

## Concrete Syntax

*Concrete Syntax* is the textual language written by programmers as input to compilers and interpreters.  When you think if a programming language, you probably think first of its concrete syntax.  Concrete syntax is virtually always described by a *grammar* consisting of an *alphabet* and a collection of *grammar rules*.

The alphabet is a collection of atomic symbols comprising a language.  The English alphabet is a good example.  Alphabets can be much more sophisticated and include other characters and words.  For our purposes, the alphabet will be atomic constructs - things that cannot be further broken down.  Numbers, identifiers, keywords, and operators will all be considered elements of our language alphabet.  As a convention throughout the text, I will use `<num>` to represent numbers and `<id>` to represent identifiers rather than define formal grammars for both.

Grammar rules describe out symbols are arranged to form *terms* in the concrete syntax.  A *term* is any text that satisfies the concrete syntax specified by grammar rules.  Similarly, *language* is the set of terms that satisfy a set of grammar rules.  It is quite useful to think of a language as a set and a grammar as a specifier for that set.  Using traditional set comprehension I can define a language, $L$, as:

$L = \{s:string | G(s)\}$

where $G$ is a predicate that is true when $s$ satisfies $L$'s grammar rules.

Let's define the concrete syntax for our first language that we will call `AE` for *Arithmetic Expressions*:

{% highlight text %}
AE ::= num
| AE + AE
| AE - AE
{% endhighlight %}

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

If we define the predicate $AE(s)$ to be true whenever $s$ satisfies `AE`'s grammar rules, then the language can be defined as:

$AE = \{s:string | AE(s)\}$

Because `AE` appears in it's own definition, the `AE` grammar is recursive and the `AE` language is infinite.  As it should be.  I'll come back to this later.

In `AE` we can express precisely three things: (i) a number; (ii) adding two expressions; and (iii) subtracting two expressions.  Not the most useful of languages, but we have to start somewhere.

A language representing the states of a stop light, the *Stop Light Language* (`SLL`), is finite:

{% highlight text %}
SLL ::= C + C 
C ::= red | green | yellow
{% endhighlight %}

where `C + C` is a pair of stoplight colors representing both sides of the light.  `SLL` does not refer to itself in its own definition and is finite.

The strings:

{% highlight text %}
red+red
green+yellow
yellow+yellow
{% endhighlight %}

are all terms in `SLL`.  In contrast:

{% highlight text %}
red+red+red
green
yellow+blue
{% endhighlight %}

are not terms in `SSL`.

Hopefully this discussion is review of your formal language theory or theory of computing studies.  If it isn't, I strongly suggest stopping now and studying some formal language theory.  In addition to being foundational, it is beautiful and useful.

## Abstract Syntax

Concrete syntax is nice on the user side, but painful to work with directly when writing interpreters and compilers.  If you want to try an experiment, write a Haskell program that will interpret the `AE` language directly.  It can be done, but making changes or extending the language is quite laborious and potentially painful.

*Abstract Syntax* is a data structure representing parsed terms.  This data structure is far more useful than concrete syntax when writing interpreters and compilers. Instead of dealing with concrete syntax, a *parser* is typically used to translate concrete syntax into abstract syntax.  An abstract syntax for `AE` written using a Haskell data type is:

{% highlight haskell %}
data AE = Num Int
        | Plus AE AE
        | Minus AE AE
          deriving (Show,Eq)
{% endhighlight %}

where `Num`, `Plus` and `Minus` are the *constructors* of the type `AE`.  This means that all values of type `AE` are constructed with one of these operations.  For example `Num 1` is the value 1 in `AE` while `(Plus (Num 1) (Num 3))` is the abstract syntax for `1+3`.

A term in the abstract syntax is anything we can create using this data type.  Every term in the concrete syntax must have an associated term in the abstract syntax.  Remember the properties of relations you learned in your discrete math class?  They come in handy right now.  Your parser should be a *total function* (all concrete syntax terms should parse to only one abstract syntax term and all concrete syntax terms should parse to some output value).  Remember that errors are outputs.

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

From this point forward I will use TLA *AST* when referring to abstract syntax data structures.  AST literally means abstract syntax *tree*.  It turns out that Haskell data types naturally form trees and trees are perfect representations for abstract syntax.  I'll come back to this later, but for now remember that AST, abstract syntax, and abstract syntax tree refer to the same thing.

## Parsers

A *parser* translates concrete syntax into an AST.  It checks the syntax of its input, generates syntax error messages if the syntax is bad, and generates an AST if the syntax is good.  This course is not about building parsers.  Chances are you will spend quite a bit of time on parsing in your compilers course, plus parsing is very much a solved problem.

## Interpreters

An *interpreter* converts a concrete syntax term into a *value*
where a *value* is a special term that cannot be interpreted further.  Values represent valid interpretation results.  If an interpreter produces something besides a value, something went wrong.  Either the input is invalid, the interpreter is written wrong, or the language definition is problematic.

Let's look at the fun case first when everything works as it should.  The following is an interpreter for *AE* that reduces every abstract syntax term to a value:

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

