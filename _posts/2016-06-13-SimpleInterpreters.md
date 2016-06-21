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

We now have an interpreter for literal numbers, but nothing more.

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

The interpreter follows a pattern that every interpreter we write will follow.  Each constructor from the AST definition has a case in the `calc` function that interprets that constructor.  This pattern and the accompanying `data` construct gives us three nice properties - completeness, determinsitic, and normalizing - that are quite useful.

Completeness says that every term constructed in `AE` will be interpreted by `calc`.  Can we prove that? As it turns out, the Haskell `data` construct gives us some exceptionally nice properties that give us nice properties for free, without direct proof. Bt definition, every value in the abstract syntax for `AE` is constructed with `Num`, `Plus`, and `Minus`.  There are no other constructions of type `AE`.   This is true of any type defined using `data` in Haskell.  All values of the defined type are constructed with its constructors.  A function that has a case for every constructor, is necessarily complete.

The Haskell `data` type definition mechanism is an example of *algebraic types* or *constructed types* that are a staple in fununctional languages and becoming increasinly common in traditional programming languages.  We call them *algebraic* because the individual constructors are *products* of values and the type itself is the *sum* of those products.  Don't worry too much about this now, we'll revisit it later when we define our own types.

Deterministic says that if we call `calc` on any term, we will get the same result.  This is an exceptionally important property as we don't want the same call to `calc` resulting in different values.  We know `AE` is deterministic because there is precisely one inference rule for interpreting each element of the concrete syntax.  In turn we know that `calc` is deterministic because there is precisely one case in the definition for each `AE` constructor.  Given `(Num n)` there is one rule and one `calc` case.  Given `Plus` there is one rule and one `calc` case.

Normalization says that a call to `calc` on any term constructed in `AE` will terminate.  Again, a pretty bold statement.  Can we prove it?  The elements of a Haskell `data` type specifically and algebraic types generally have are `well-ordered`.  In mathematics, well-ordered means that every subset of a set has a least element.  Getting from well-ordered to guaranteed termination takes a bit of work, but the gist is that components of a constructor are smaller than the constructor itself.  Each recursive call on the parts of a term is made on a smaller term.  Consider `calc (Plus t1 t2)` where `t1` and `t2` can be viewed as smaller than `(Plus t1 t2)`.  Because every set of `AE` terms has a least element, pulling a term apart into its peices will eventually get to that least element.

The least elements of `AE` are those constructed by `(Num n)`.  When the interpreter reaches the least element, it cannot go further and terminates.  Every call to `calc` gets closer to the least element.  Note that those those least elements are what we defined as values.  This is not a conicidence.  Not only does `AE` terminate, it always terminates with a value.  Said it terms we all understand, the `AE` interpreter never crashes for any element of `AE1`.

## All Together Now

Before we say more about `AE`, lets put all the pieces together into a single definition.  Definition of `AE` is now complete sytactically, semantically and operationally:

* Syntax is defined by the `AE` grammar
* Semantics is defined by the `AE` inference rules
* Operations are defined by `calc` and `parse`.

First we defined a concrete syntax for terms:

$$\begin{align*}
t ::= & \NUM \mid t + t \mid t - t \\
\end{align*}$$

and syntactic definition of values:

$$\begin{align*}
v ::= & \NUM \\
\end{align*}$$

Then we defined basic inference rules formally defining how `AE` is interpreted:

$$\frac{}{\calc v = v}\; [NumE]$$

$$\frac{\calc t_1 = v_1,\; \calc t_2 = v_2}{\calc t_1 + t_2 = v_1+v_2}\; [PlusE]$$

$$\frac{\calc t_1 = v_1,\; \calc t_2 = v_2}{\calc t_1 + t_2 = v_1-v_2}\; [MinusE]$$

We implemented a parser from the grammar and an interpreter from the inference rules:

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

Given the parser and interpreter for `AE`, we can now define a language evaluator, `eval`, that puts everything together:

{% highlight haskell %}
eval :: String -> AE
eval = calc . parseAE
{% endhighlight %}

In words, `eval` is the composition of `parse` and `calc`.  This notation says that `parse` will be called first and the output passed to `calc`.  If `parse` throws an error, `eval` will terminate without passing a value to `calc`.

## Discussion

`AE` is a rather silly language that is less powerful than one of those bank calculators you get when you open a checking account.  It adds and subtracts numbers.  However, we need to start somewhere and `AE` is good for that.

### Complete, Deerministic, and Normalizing

We said earlier that `calc` is complete, deterministic and normalizing.  Complete in that any element of `AE` can be interpretted, deterministic in that there is only one way to interpret any element of `AE`, and normalizing in that every interpretation of and `AE` element terminates.

Unfortunately, these nice properties result from `AE` being such a trivial language.  Completeness and deterministic are properties something we will seek to preserve as we add features.  However,  normalizing will prove problematic when we add recursion and looping.  Some programs such as operating systems shouldn't terminate anyway.  Certainly we would like to ensure that no programs written in our languages crash, but that one will unfortunately go away in the next chapter.

### Induction and Extensionality

There is one property of `AE` structures that underlies most of our discussion.  The `AE` abstract syntax specifically and algebraic types generally have both inductive and extensionality principles.  The inductive principle allows us to prove properties over `AE` and the extensionality principle allows us to determine if two `AE` structures are equivalent.  We won't use either principle yet, so let's define them informally for now.

Induction over `AE` says that some property, $p$, is true for all elements of `AE` if we can:

1. Prove $p$ directly for base cases
2. Prove $p$ for inductive cases by assuming $p$ for the case's pieces

What are the base and inductive cases?  For `AE`, `Num` is the base case while `Plus` and `Minus` are inductive cases.  `Num` is a base case because it is not constructed from other elements of `AE`.  We can see this in the `data` definition. It is also reflected in the definition of `calc` where there is no recursive call for `Num`.  `Plus` and `Minus` both depend on other `AE` constructions in their definition and `calc` cases.  Stated in terms of the `AE` concrete syntax, the induction principle states that $p$ is true for any `AE` term $t$ when:

$$
\begin{align*}
\forall & n . p(n) \\
\forall & t_1 t_2 . p(t_1) -> p(t_2) -> p(t_1 + t_2) \\
\forall & t_1 t_2 . p(t_1) -> p(t_2) -> p(t_1 - t_2) \\
\end{align*}
$$

Extensionality is simpler to state.  Two terms in `AE` are equivalent if they use the same constructor and their parts are equivalent.  For any $t$ and $t'$, $t=t'$ if one of the following cases applies:

$$
\begin{align*}
t_1 + t_2 = t_1' + t_2' <-> t_1=t_1' \wedge t_2=t_2'
t_1 - t_2 = t_1' - t_2' <-> t_1=t_1' \wedge t_2=t_2'
\end{align*}
$$

This if no case applies, then the terms are not equal.  So, $t_1+t_2$ will never be equal to $t_1'-t_2'$.

## Exercises

1. Add multiplication and division to `AE`.  Recall that `div` is the Haskell function for integer division.  Do we lose any of our nice properties by doing this?
2. Rewrite `AE` replacing the `Plus` and `Minus` constructors in the AST with a single constructor `Binop op t1 t2` where `op` is the represented binary operation.  You will need to change the parser to generate `Binop` rather than `Plus` and `Minus`.  Think carefully about what `op` should be.  If you do it right, you should be able to add any operator to `AE` by simply changing the parser.

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
