---
layout: frontpage
title: Adding Booleans Redux
use_math: true
categories: chapter ch2
---

$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeof}\;}
\newcommand\interp{\mathsf{interp}\;}
\newcommand\eval{\mathsf{eval}\;}
\newcommand\NUM{\mathsf{NUM}\;}
\newcommand\ID{\mathsf{ID}\;}
\newcommand\iif{\mathsf{if}\;}
\newcommand\tthen{\;\mathsf{then}\;}
\newcommand\eelse{\;\mathsf{else}\;}
\newcommand\iisZero{\mathsf{isZero}\;}
\newcommand\bbind{\mathsf{bind}\;}
\newcommand\iin{\mathsf{in}\;}
\newcommand\aand{\;\mathsf{\&\&}\;}
\newcommand\lleq{\;\mathtt{<=}\;}
\newcommand\ttrue{\;\mathsf{true}}
\newcommand\ffalse{\;\mathsf{false}}
\newcommand\tnum{\;\mathsf{TNum}}
\newcommand\tbool{\;\mathsf{TBool}}
$$

# Adding Booleans Redux

Before moving ahead to functions, let's spend a bit of time looking at type checking when adding Booleans to BAE.  Let's define a new language BAE with Booleans (`BBAE`) that simply adds the same boolean operators added to `AE` to get `ABE`.  Let's quickly walk through our standard methodology for extending a language and add at the same time add type checking.  We'll treat this almost like an exercise with some quick commentary, but not a great deal of detail.

## Concrete Syntax and Values

The first step is extending concrete syntax and adding new values if necessary.  The concrete syntax for `BBAE` is literally the concrete syntax of `ABE` and `BAE` composed:

$$\begin{align*}
t ::= & \NUM \mid t + t \mid t - t \\
      & \mid \bbind \ID=t\; \iin t \\
      & \mid \ttrue \mid \ffalse \mid \iif t \tthen t \eelse t \\
      & \mid t \lleq t \mid t \aand t \mid \iisZero t \\
\end{align*}$$

Similarly, there are no new values that do not appear in `ABE` and `BAE`.  We simply compose the values from those to languages:

$$\begin{align*}
v := \NUM \mid \ttrue \mid \ffalse \\
\end{align*}$$

We now have a syntax for `BBAE` to work from.  On to the parser and pretty printer.

## Abstract Syntax, Parser, Pretty Printer, and Generator Definition

The abstract syntax of `BBAE` follows immediately from its concrete syntax.  One can simply copy-and-paste the elements of `ABE` and `BAE` into a single AST resulting in:

{% highlight haskell %}
data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)
{% endhighlight %}

A quick check (no pun intended) reveals that we have an abstract syntax element for each language element.  One can literally walk through the concrete syntax and map constructs to their constructors.

The parser, pretty printer, and generator for `BBAE` are simple extensions of `ABE` and `BAE`.  For brevity, we'll move on without including them in the discussion.  However, you'll find code for all three in the `BBAE` source files.  Note that the `ABE` generator must be modified to account for keeping track of generated identifiers.  This is a simple addition, but easy to miss.

## Evaluation Rules

Like the syntax definition, the definition for eval composes rules from `ABE` and `BAE`.  Literally nothing changes:

$$\frac{}{\eval v = v}\; [NumE]$$

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 + t_2 = v_1+v_2}\; [PlusE]$$

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 + t_2 = v_1-v_2}\; [MinusE]$$

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 \aand t_2 = v_1 \wedge v_2}\; [AndE]$$

$$\frac{\eval t_1 = v_1,\; \eval t_2 = v_2}{\eval t_1 \lleq t_2 = v_1\leq v_2}\; [LeqE]$$

$$\frac{\eval t = v}{\eval \iisZero t = v==0}\; [isZeroE]$$

$$\frac{\eval t_0 = v_0,\;\eval t_1 = v_1,\;\eval t_2 = v_2}{\eval \iif t_0 \tthen t_1 \eelse t_2 = \iif v_0 \tthen v_1 \eelse v_2}\;[IfE]$$

$$
\frac{\eval a = v}{\eval (\bbind\; i\; = a\;\iin s) = \eval [i\mapsto v]s}\;[BindE]
$$

How does this work?  We've combined two languages with their own constructs by simply combining rules.  How do they know about each other during interpretation?  The trick is in the definition of `t` and its associated abstract syntax.  As long as we have a rule for every abstract syntax element, recursive calls to the interpreter take care of integrating the languages.  Said differently, in virtually every rule we can interpret subterms of terms by simply calling the interpreter *without regard to the specifics of the subterm*.  Recursion is your friend here.

The property that allows us to compose languages in this way is _orthogonality_.  The valuation rules for one expression do not interact with other specific expressions.  For example, while `isZero` uses the value of its argument expression, that expression can be any properly formed language expression.  Orthogonality is an important design principle that dramatically simplifies language design and implementation.

We now have a mathematical definition for the evaluation relation.

## Eval Definition

Now the implementation details for the evaluator.  Here things are not quite as simple as composing previous interpreters.  We'll need to spend some time thinking through how we want our interpreters to behave before diving into the code.

What should the interpreter return?  Previously we've implemented interpreters that return:

1. Haskell values
2. AST values
3. `Either` error messages or AST values

The plan is to eventually write a type checker for this language. AST values are sufficient for this `eval` implementation because we hope to predict success before execution.  Haskell values would be fine, but we want to use our earlier technique for implementing error messages.  The `Either` implementation was specifically for catching errors at runtime and that is not required for a type checking interpreter.

What about the environment?  `Env` remains unchanged from previous implementations because the environment remains a simple list of identifier/value pairs.  Nothing changes other than the type stored in the environment changing to `BBAE`:

{% highlight haskell %}
type Env = [(String,BBAE)]
{% endhighlight %}

This gives us the expected type signature for the `eval` function:

{% highlight haskell %}
eval :: Env -> BBAE -> BBAE
{% endhighlight %}

`eval` will accept an environment and abstract syntax term and produce an abstract syntax term.

Evaluation cases for each AST element are defined by evaluation rules and taken verbatim from the `ABE` and `BAE` interpreters.  Again, orthogonality helps us out.  The resulting function looks like this:

{% highlight haskell %}
eval ::  Env -> BBAE -> BBAE
eval env (Num x) = (Num x)
eval env (Plus l r) = let (Num l') = (eval env l)
                          (Num r') = (eval env r)
                      in (Num (l'+r'))
eval env (Minus l r) = let (Num l') = (eval env l)
                           (Num r') = (eval env r)
                       in (Num (l'-r'))
eval env (Bind i v b) = let v' = eval env v in
                          eval ((i,v'):env) b
eval env (Id id) = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Variable not found"
eval env (Boolean b) = (Boolean b)
eval env (And l r) = let (Boolean l') = (eval env l)
                         (Boolean r') = (eval env r)
                      in (Boolean (l' && r'))
eval env (Leq l r) = let (Num l') = (eval env l)
                         (Num r') = (eval env r)
                      in (Boolean (l' <= r'))
eval env (IsZero v) = let (Num v') = (eval env v)
                      in (Boolean (v' == 0))
eval env (If c t e) = let (Boolean c') = (eval env c)
                      in if c' then (eval env t) else (eval env e)
{% endhighlight %}

Note that `eval` uses the Haskell `error` function rather than returning value.  Given that we call a type checker before evaluating an expression, why do we need to do this?  As it turns out, that `error` instance will never be called.  Type checking assures that all variables are bound and the "Variable not found" case will not occur.  However, Haskell requires case statements to completely cover a type.  The language has no way of determining the `Nothing` case will not be called.  We could have the code literally do anything in the `Nothing` case, but we choose to generate an error knowing that something is horribly wrong if it is actually generated.

The interpretation final function that composes the interpreter and parser is identical to that for `BAE`.  Specifically, call the parser and evaluate the result with an initial, empty environment:

{% highlight haskell %}
interp = (eval []) . parseBBAE
{% endhighlight %}

## Testing Eval

We now have an interpreter for `BBAE` that requires testing.  By defining `interp` in the same manner as `BAE`, the test function for `eval` literally does not change:

{% highlight haskell %}
testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval [] t))
{% endhighlight %}

With everything in place, we evaluate over just a few cases and quickly learn that `eval` without any dynamic error checking or static type checking fails.  Hopefully this is not surprising to you.

The next step is defining and implementing a type checker to statically determine if a `BBAE` expression will execute.

## Type Rules

Type rules for `BAE` and `ABE` expressions remain unchanged in `BBAE`.  Numbers remain of type $\tnum$ and Booleans $\tbool$:

$$\frac{}{\typeof \NUM = \tnum}\; [NumT]$$

$$\frac{}{\typeof \ttrue = \tbool}\; [TrueT]$$

$$\frac{}{\typeof \ffalse = \tbool}\; [FalseT]$$

Unary and binary operations similarly remain the same:

$$\frac{\typeof t_1 = \tnum,\; \eval t_2 = \tnum}{\typeof t_1 + t_2 = \tnum}\; [PlusT]$$

$$\frac{\typeof t_1 = \tnum,\; \eval t_2 = \tnum}{\typeof t_1 + t_2 = \tnum}\; [MinusT]$$

$$\frac{\typeof t_1 = \tbool,\; \typeof t_2 = \tbool}{\typeof t_1 \aand t_2 = \tbool}\; [AndT]$$

$$\frac{\typeof t_1 = \tnum,\; \typeof t_2 = \tnum}{\eval t_1 \lleq t_2 = \tbool}\; [LeqT]$$

$$\frac{\typeof t = \tnum}{\typeof \iisZero t = \tbool}\; [isZeroT]$$

$$\frac{\typeof t_0 = \tbool,\;\typeof t_1 = T,\;\typeof t_2 = T
}{\typeof \iif t_0 \tthen t_1 \eelse t_2 = T}\;[IfT]$$

The new concept in this language is identifiers introduced by `bind` and used anywhere in expressions.  How do we determine the type of identifiers?  To get an idea, recall how `eval` handles `bind` using an environment.  when evaluating a `bind`:

{% highlight text %}
bind x = v in b
{% endhighlight %}

`x` takes the value `v` in `b`.  We use an environment to hold bindings of identifiers to values.  Can we do the same thing with types of identifiers?  Specifically, create a environment-like structure that maintains bindings of identifiers to types and simply look up type in that structure?

The environment-like structure that contains types will be called a _context_ and is frequently represented by $\Gamma$.  Like the environment, it is a list of pairs and behaves in exactly the same fashion.  The difference is each pair consists of an identifier and type rather than identifier and value.

We will use the notation $(x,T)$ to represent the binding of a variable to some type and the Haskell list append $(x,T):\Gamma$ to represent addition of new binding to a context.  Finally, $(x,T)\in\Gamma$ represents finding the first instance of $(x,T)$ in $\Gamma$.

The rule for `bind` adds the newly bound identifier with its type to the context and finds the type of the `bind` body:

$$\frac{\typeof v=T}{\typeof \bbind i=v \iin b = \typeof (i,T):\Gamma b}\;[BindT]$$

The type of the body becomes the type of the `bind`.  This makes sense as the body is what gets evaluated when the `bind` is evaluated.  It's also interesting to think of `bind` as a special expression that simply adds to the environment or context.

One more type rule for identifiers is needed to finish the definition.  It simply looks up a type binding $(v,T)$ in $\Gamma$ and asserts that $v:T$ is true when the type binding is found:

$$\frac{(i,T)\in \Gamma}{\typeof i = T}\;[IdT]$$

This lookup is quite similar to the lookup used when the environment is used.  In fact, functions for manipulating the environment and context are identical.

## Typeof Definition

Now we implement the `typeof` function. First the standard data type for the two types expressions defined for `BBAE`:

{% highlight haskell %}
data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)
{% endhighlight %}

Now we build out the type checker from the type checking rules.  Like the `eval` function earlier, virtually all type checking code comes from the `BAE` and `ABE` type checkers.

`typeof` accepts a context and AST and returns either an error message or a type:

{% highlight haskell %}
typeof :: Cont -> BBAE -> Either String TBBAE
{% endhighlight %}

Note that we're using the `Either` construct to return the type or error.  However, we're not using `Either` as a monad in this type inference function.  The `typeof` signature is eerily similar to the `eval` signature.  We'll come back to that later, but it's worth thinking about why that's the case.

Here's the complete code for the `typeof` function:

{% highlight haskell %}
typeof cont (Num x) = (Right TNum)
typeof cont (Plus l r) = let l' = (typeof cont l)
                             r' = (typeof cont r)
                         in if l'==(Right TNum) && r'==(Right TNum)
                            then (Right TNum)
                            else (Left "Type Mismatch in +")
typeof cont (Minus l r) = let l' = (typeof cont l)
                              r' = (typeof cont r)
                          in if l'==(Right TNum) && r'==(Right TNum)
                             then (Right TNum)
                             else (Left "Type Mismatch in -")
typeof cont (Bind i v b) = let v' = typeof cont v in
                             case v' of
                               (Right v'') -> typeof ((i,v''):cont) b
                               (Left _) -> v'
typeof cont (Id id) = case (lookup id cont) of
                        Just x -> (Right x)
                        Nothing -> (Left "Varible not found")
typeof cont (Boolean b) = (Right TBool)
typeof cont (And l r) = if (typeof cont l) == (Right TBool)
                           && (typeof cont r) == (Right TBool)
                        then (Right TBool)
                        else (Left "Type mismatch in &&")
typeof cont (Leq l r) = if (typeof cont l) == (Right TNum)
                           && (typeof cont r) == (Right TNum)
                        then (Right TBool)
                        else (Left "Type mismatch in <=")
typeof cont (IsZero v) = if (typeof cont v) == (Right TNum)
                         then (Right TBool)
                         else (Left "Type mismatch in IsZero")
typeof cont (If c t e) = if (typeof cont c) == (Right TBool)
                            && (typeof cont t)==(typeof cont e)
                         then (typeof cont e)
                         else (Left "Type mismatch in if")
{% endhighlight %}

There's really nothing to see here.  Our technique for defining languages and specifying their evaluation and type characteristics makes it simple to compose orthogonal language elements.  Things will get harder as we add other constructs, but for now things are pretty easy.

## Testing

Could it be that testing the `BBAE` evaluator is as simple as composing the `ABE` and `BAE` QuickCheck capabilities?  Fortunately, it is.  We simply need to generate arbitrary terms that include terms from both the `ABE` and `BAE` language subsets.

As usual, we first make the `BBAE` data type an instance of `Arbitrary` and define the `arbitrary` function.  The use of `genBBAE` is where the generator for `BBAE` is integrated:

{% highlight haskell %}
instance Arbitrary BBAE where
  arbitrary =
    sized $ \n -> genBBAE (rem n 10) []
{% endhighlight %}

The hard work for the generator itself is already done.  First, we'll define generators for the numeric elements of `BBAE`:

{% highlight haskell %}
genNum =
  do t <- choose (0,100)
     return (Num t)

genPlus n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (Plus s t)

genMinus n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (Minus s t)

genAnd n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (And s t)
{% endhighlight %}

Now the elements that involve both numbers and booleans:

{% highlight haskell %}
genBool =
  do t <- choose (True,False)
     return (Boolean t)

genLeq n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (Leq s t)

genIsZero n e =
  do s <- genBBAE n e
     return (IsZero s)

genIf n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     u <- genBBAE n e
     return (If s t u)
{% endhighlight %}

Finally, the elements that involve identifiers and their values:

{% highlight haskell %}
genBind n e =
  do i <- genName
     v <- genBBAE n e
     b <- genBBAE n (i:e)
     return (Bind i v b)

genName =
  do i <- choose ('v','z')
     return [i]

genId e =
  do n <- elements e
     return (Id n)
{% endhighlight %}

Now we simply put everything together into a single generator:

{% highlight haskell %}
genBBAE :: Int -> [String] -> Gen BBAE
genBBAE 0 e = 
  do term <- oneof (case e of
                      [] -> [genNum,genBool]
                      _ -> [genNum
                           , genBool
                           , (genId e)])
     return term
genBBAE n e =
  do term <- oneof [genNum
                   , (genPlus (n-1) e)
                   , (genMinus (n-1) e)
                   , (genAnd (n-1) e)
                   , (genLeq (n-1) e)
                   , (genIsZero (n-1) e)
                   , (genIf (n-1) e)]
     return term
{% endhighlight %}

What changes here from the previous generators is the list of generators passed to `oneof`.  Instead of choosing only expressions from the numeric expression or the boolean expressions, we now choose from both.  For values, things work similarly choosing vrom both Boolean and numeric values.

## Discussion

Three things worth thinking about are happening in this chapter.  First, we are going from beginning to end defining a new language.  We started with syntax, defined evaluation and implemented an evaluation function, defined a type system and implemented a type checker, and concluded with QuickCheck functions for testing.  This is the methodology we use for defining languages at work.

Second, we see how composition of orthogonal language constructs makes this reasonably simple.  How many times in the chapter did we talk about composing elements of previous interpreters?  Usually that was a simple cut-and-past operation.  The only real exception being the arbitrary test case generator where we had to compose the language subsets.

Finally, we introduced the concept of a context that behaves like an environment for types.  An interesting difference is the environment is a runtime construct while the context is used during static analysis.  The reason is values cannot be known until runtime, but types are known statically.  This is not true for all languages, but is true for our languages.

Remember these things moving forward.  The step-by-step approach for defining a language and the composition of orthogonal language constructs are important design concepts.  Context is something we will continue to use throughout our study.

## Definitions
* Context - list containing bindings of identifiers to types defining identifiers currently in scope.

## Exercises

1. Write the function `evalErr :: Env -> BBAE -> Either String BBAE` for for `BBAE` that returns either a string error message or a value following interpretation.
2. Compare the `evalErr` function with the `evalType` function using the same techniques used for `ABE`
3. Prove that the `error` function will never be called in `eval` given that all expressions given to `eval` successfully type check.
4. Rewrite `typeof` using `Either` as a monad.

## Source

Download [source]({{site.baseurl}}/haskell/bbae.hs) for all interpreter code from this chapter.