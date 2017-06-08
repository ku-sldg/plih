## Parsers

A *parser* is a program that translates concrete syntax into an AST.  It checks the syntax of its input, generates error messages if the syntax is bad, and generates an AST if the syntax is good. The signature of a parser for the _AE_ language is:

{% highlight haskell %}
parseAE :: String -> AE
{% endhighlight %}

Give `parseAE` a string and it will return an `AE` if it does not crash.  More generally, the parser for any language will be from a string to the datatype for that language's abstract syntax.

This course is not about building parsers.  Thankfully, Haskell provides a collection of tools for building parsers automatically.  I will provide examples using [Parsec](https://wiki.haskell.org/Parsec), a monadic parser combinator tool for writing and composing parsers.  After a bit of setup, Parsec is not difficult to extend to the languages we would like to discuss.  For more details there are a number of online tutorials as well as chapters in major Haskell texts.  For now, let's dive in and write a simple expression parser for the _AE_ language that we'll extend throughout the course.

First, the boilerplate that includes standard Haskell libraries for the kinds of parsers we want to write.  Include this at the top of every Haskell implementation where you will use a parser:

{% highlight haskell %}
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils
{% endhighlight %}

`ParserUtils` has a few definitions that make defining parsers a bit simpler for this text.  You need to have `ParserUtils` in the same directory as your Haskell source or installed in a place where GHC can find it.

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

The `inFix` function is defined in `ParserUtils` to simplify defining
inFix operations.  This
command tells us that `t1 + t2` is an infix operation that is translated into `Plus (parseAE t1) (parseAE t2)` and is left associative.  For now, we'll leave out details of what Parsec is doing.  It is sufficient to know that the `inFix` function creates a parser for `t1+t2` that returns an application of `Plus` to parsing `t1` and `t2`.  `AssocLeft` indicates the operator is left associative in the absence of parenthesis.  In other words:

{% highlight text %}
x + 3 - y + 7 == ((x+3) - y) + 7
{% endhighlight %}

There are two operations in _AE_, so there are two `inFix` applications in a list.  The list itself tells the parser generator that `+` and `-` have the same precedent.  We'll see examples with different precedent as our languages grow more complex.  `operators` is a list of lists where each internal list contains operators of the same precedence.  `+` and `-` are in a list together indicating they are at the same precedence level.

{% highlight haskell %}
operators = [ [ inFix "+" Plus AssocLeft
                , inFix "-" Minus AssocLeft ]
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

Looking back at the definition of `expr` puts the entire thing together.  `expr` is a parser that returns `AE` and is built from `operators` defining operations over `terms`.

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

If the construction of the `AE` parser bothers you, it is safe to simply use it for the time being.  We'll learn more about extending and writing parsers as we move along.  Don't get caught up in it now.

## Pretty Printers

A _pretty printer_ is the opposite of a parser.  It takes an AST and prints the concrete syntax of the represented program.  Pretty printers are used for many reasons, but our use is motivated by testing.  Specifically, a parser can be tested by parsing, pretty printing and re-parsing a program.  If the original parsed result matches the re-parsing result for all test cases we have strong evidence the parser is correct.  We will also use pretty printers to move from generated AST values to test cases for an interpreter.

The pretty printer for `AE` looks very much like an evaluator.  For every AST constructor we return a string representing that construct:

{% highlight haskell %}
pprint :: AE -> String
pprint (Num n) = show n
pprint (Plus n m) = "(" ++ pprint n ++ "+" ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ "-" ++ pprint m ++ ")"
{% endhighlight %}

Numbers generate strings using `show` because `Int`'s are instances of class `Show`.  Composite terms are printed by pretty printing their parts and putting the results together.  Both `Plus` and `Minus` generate strings from their arguments and assemble the strings into text terms.

One could easily make the data type `AE` an instance of `Show` using the new `pprint` function:

{% highlight haskell %}
instance Show AE where
  show = pprint
{% endhighlight %}

Think carefully before doing this.  If you have defined `show` in this way, anytime an AST is printed you will get the pretty printed version.  Debugging is usually far simpler if the Haskell representation is printed rather than the string representation.

## Parsing ABE

Updating the `AE` parser to operate on `ABE` demonstrates a pattern we will use extensively.  The definition of `expr` does not change - it will still be our parser and is built from opTable and terms.

```haskell
expr :: Parser ABE
expr = buildExpressionParser opTable term
```

The addition of new operators allows us to look at more functionality in the operator specification.  You see first the definitions for plus and minus from `AE` together in a list indicating they have the same precedent.  Then you see definitions for `isZero` and `<=` in a list immediately below.  These operators have precent immediately lower than `+` and `-`.  For example, `isZero x - y` will be parsed as `(isZero (x-y))` because `-` has higher precedence than `isZero`.  Finally, `&&` is in its own list following the numeric operations indicating that it has yet lower precedence.  So, `true && isZero (x-y)` parses as `(true && (isZero (x-y))).

```haskell
opTable = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            , [ inFix "<=" Leq AssocLeft
	          , preFix "isZero" IsZero ]
            , [ inFix "&&" And AssocLeft ]
            ]
```

One final addition is the first appearance of the `preFix` operator function.  As you might guess, this creates a prefix operation in a similar manner as `inFix`.  There is also a `postFix` function should you find a need for it.

If you haven't bought in to the Parsec approach, hopefully the next extension will start to convert you.  Here we define parsers for each individual term that operators can operate over.  In `AE` this was only integers, but in `ABE` we add Boolean values and `if`.  The first three parsers operate over number and boolean constants.  We're using the `integer` parser that is built in Parsec for numbers.  For `true` and `false` we use the `reserved` parser that operates over reserved words that are enumerated in `PaserUtils`.  `trueExpr` parses the reserved word `true` and returns the abstract syntax `(Boolean True)` representing the appropriate constant.  Similarly for `falseExpr`.  Anytime you need to parse a reserved work or constant, these built-in parsers can be modified.  Remember however that `lexer` is defined in `ParserUtils` and must be modified if you want to include keywords that are not already included.

```haskell
numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)
```

The parser for `if` is a hint of Parsec's power.  It uses the Haskell `do` notation to compose a collection of smaller parsers.  Within the `do` notation, expressions are evaluated sequentially and results bound to variables using `<-`.  The `ifExpr` parser executes the following parsers in sequence:

1. Parse an "if" using `reserved`
2. Parse an expression and store the result in `c`
3. Parse a "then" using `reserved`
4. Parse an expression and store the result in `t`
5. Parse an "else" using reserved
6. Parse an expression and store the result in `e`
7. Return the AST result `(if c t e)`

Here's the actual code:

```haskell
ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)
```

Now we put the whole thing together to define `term`.  The `<|>`
notation should be interpreted as or.  The `term` parser looks for an expression in parenthesis, a number, a true or false, or an  if expression.  The `parens` parser is another built-in parser that puts things in parenthesis.  So `parens lexer expr` looks for `(expr)`.  The other parsers are what we built above.

```haskell
term = parens lexer expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
```

Looking back at the definition of `expr` it defines a complete parser using `term` and `opTable`.  Expressions can be used in operations and operations in expressions.  Plus, we've now got a parsing infrastructure that can be easily extended without much discussion moving forward.

We'll invoke our new parser the same as always:

```haskell
parseABE = parseString expr

parseABEFile = parseFile expr
```
