---
layout: frontpage
title: Failure
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
\newcommand\iif{\mathsf{if}\;}
\newcommand\tthen{\;\mathsf{then}\;}
\newcommand\eelse{\;\mathsf{else}\;}
\newcommand\iisZero{\mathsf{isZero}\;}
\newcommand\aand{\;\mathsf{\&\&}\;}
\newcommand\lleq{\;\mathtt{<=}\;}
\newcommand\ttrue{\;\mathsf{true}}
\newcommand\ffalse{\;\mathsf{false}}
\newcommand\tnum{\;\mathsf{TNum}}
\newcommand\tbool{\;\mathsf{TBool}}
$$

# Failure Is an Option

We've established that `ABE` fails for test cases that attempt to use Boolean values where numbers should be used or numbers where Boolean values should be used.  Before addressing that specific problem, let's look at how failure should be handled generally.

First a bit of new terminology.  We are defining a new language called `ABE` using Haskell to build parsers and interpreters.  We will refer to `ABE` as the *external language* or *domain specific language*.  We refer to Haskell as the *host language* or *implementation language*.  The distinction is we are building tools for `ABE` and using Haskell to build them.

When the current `ABE` interpreter fails, it fails in the host language.  When we call the Haskell `error` function we jump out of our interpreter and generate a Haskell error message.  This approach works, but has several problems.

First, we have no control over errors.  The happen and our control of execution ends.  Java uses an innovative approach where exceptions are Java data structures that allow us to write Java programs to process them.  This is how systems like Eclipse can allow new tools that generate error messages to simply be plugged into the infrastructure.  Right now, we can't do this.

Second, our only choice is to fail.  What if our interpreters can avoid failures?  What if we can predict failures during or before execution?  This results in systems that are more robust and code that we can better control.

Let's look at two approaches to handling errors.  The first will handle them *dynamically* or at run-time.  Our interpreter will generate error messages as data structures that we can process how we choose.  The second will handle them *statically* by predicting runtime failure before execution.

## Runtime Error Checking

We will update our `ABE` evaluator to catch errors and run time rather than falling into Haskell using the `error` function.  The new evaluator will return either a value or an error message that we can handle however we want.

### Eval

Let's change the type signature of the `ABE` `eval` function just a bit and define a new function called `evalErr` that returns either a n `ABE` term or a string representing an error:

{% highlight haskell %}
evalErr :: ABE -> Either ABE String
{% endhighlight %}

If you're not familiar with the `Either` type constructor, there is ample documentation of its use.  In this example it provides two constructors, `Left` and `Right` that contain an `ABE` value and a `String` respectively.  We'll use the `Either` type to return either an `ABE` value, `v` (`Left v`) or a string error message, `s` (`Right s`).  We can then use a `case` expression to discriminate between values.  Any time we call `evalError` we can do something like this:

{% highlight haskell %}
case (evalErr t) of
  (Left v) -> actions taken for a value v
  (Right m) -> actions taken for an error message m
{% endhighlight %}

We're going to use this pattern extensively in our new definition of `evalErr`.

Start with the two easy cases for number and Boolean constants.

{% highlight haskell %}
evalErr (Num t) = (Left (Num t))
evalErr (Boolean b) = (Left (Boolean b))
{% endhighlight %}

In `eval`, both numbers and Booleans evaluate to themselves and cannot crash.  Thus, in `evalErr` we don't need to deal with errors.  The return value is `(Left (Num t))`, with `Left` indicating a value and `(Num t)` being the value.

Constant cases are not particularly interesting, so let's look at `isZero`.  Unlike the constant cases, `isZero` can fail when evaluating its argument or when its arguments is not a number:

{% highlight haskell %}
evalErr (IsZero t) =
  let r = (evalErr t)
  in case r of
       (Right m) -> r
       (Left (Num v)) -> (Left (Boolean (v == 0)))
       (Left _) -> (Right "Type error in isZero")
{% endhighlight %}

What's happening here?  First the argument to `IsZero` is evaluated and assigned to `r`.  By definition we know that `r` is now either a value or some error message.  We'll use our pattern from above to decide what we're looking at.

The first option is `(Right m)`.  This occurs when evaluating `t` results in an error.  If this is the case, we have no work to do and we simply return the error message for the calling function to deal with.

The second option is `(Left (Num v))`.  If this pattern matches we know two things.  First, that `evalErr t` returned a value and that value is a `Num`.  This is the success case and we return `(Left (Boolean (v==0)))`.  `Left` because we have a value, `Boolean` to construct a Boolean value, and `v==0` to calculate the Boolean value.

The final option is `(Left _)`.  If this pattern matches we know the previous pattern did not.  Thus, we have a value and that value is not a number.  In this case we create an error message and return it as `(Right "Type error in isZero")`.  `Right` because we have an error and `"Type error in isZero"` as the error value.

To summarize, evaluating `IsZero` requires evaluating its argument and taking one of three actions:

1. Return the result if it is an error
2. Return the comparison of the result with 0 if it is a number
3. Retern an error message if it is a value, but not a number

The remaining binary operations are virtually the same except we have two arguments to evaluate and need to nest handling argument results.  You'll see our pattern occurring twice in the code for `Plus`:

{% highlight haskell %}
evalErr (Plus t1 t2) =
  let r1 = (evalErr t1)
      r2 = (evalErr t2)
  in case r1 of
       (Right m) -> r1
       (Left (Num v1)) -> case r2 of
                            (Right m) -> r2
                            (Left (Num v2)) -> (Left (Num (v1+v2)))
                            (Left _) -> (Right "Type Error in +")
       (Left _) -> (Right "Type Error in +")

{% endhighlight %}

There is no magic here!  We calculate the values of both arguments and store the results in `r1` and `r2` respectively.  Then we apply the same pattern as `IsZero` and determine if the first argument is an error, number, or something else.  In the error and something else cases, we do exactly what we did previously.  In the number case, we repeat the same process for `r2` and do the same thing.  In the error and something else cases, we do exactly what we did previously.  IN the number case, we calculate the result of `Plus` and return it as `(Left (Num (v1+v2)))`.  The other binary operations follow similarly.

The remaining operation is `if` that is treated like a one parameter expression.  The condition is evaluated and the outcome handled using the same pattern as other expressions.  If the condition evaluates to a Boolean, them we choose the expression to evaluate based on the Boolean value.  The final code has the following form:

{% highlight haskell %}
evalErr (If t1 t2 t3) =
  let r = (evalErr t1)
  in case r of
       (Right _) -> r
       (Left (Boolean v)) -> if v then (evalErr t2) else (evalErr t3)
       (Left _) -> (Right "Type error in if")
{% endhighlight %}

Once the interpreter is completed, we can define an interpreter function in a manner similar to the original interpreter function for `eval`:

{% highlight haskell %}
interpErr = evalErr . parseABE
{% endhighlight %}

Now we're set to test our new interpreter

### QuickCheck

The same functions used for testing the original `ABE` evaluator are also useful for `evalErr`.  We simply substitute `interpErr` for `interp` in the test functions:

{% highlight haskell %}
testEvalErr :: Int -> IO ()
testEvalErr n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interpErr $ pprint t) == (evalErr t))
{% endhighlight %}

Running `testEvalErr` for 1000 random cases reveals that we accomplished our original goal of having an evaluator for `ABE` that does not crash.

However, we can do more.  Let's compare `eval` and `evalErr` to assess whether our new implementation agrees with our original implementation.  Unfortunately, this is not as simple as creating a function that compares the results of `eval` and `evalErr` like this:

{% highlight haskell %}
\t -> eval t == evalErr t
{% endhighlight %}

because the return type of `eval` is different than the return type of `evalErr` and `eval` still crashes.  What we care about are cases when `eval` should not crash and produce a value.  We can't test for  `eval` not crashing, but we can test for when `evalErr` produces a value rather than an error.  Remember `Left` and `Right`?  When `evalErr` returns a `Left` value we know it produced a value and `eval` should also produce a value.  When `evalErr` returns a `Right` we know it produced an error message and we should not evaluate `eval`.  Here's a function to do just this:

{% highlight haskell %}
  (\t -> (let r = (evalErr t) in
            case r of
              (Left v) -> v == (eval t)
              (Right v) -> True))
{% endhighlight %}

The `case` performs exactly the check we need.  `Left` compares the value generated with the results of `eval` on `t`.  `Right` just returns  `True`.  Why?  QuickCheck checks to see if the conjunction of all tests succeed.  `True` causes QuickCheck to, in essense, ignore the case.  Exactly what we want.  Here's the QuickCheck function:

{% highlight haskell %}
testEvals :: Int -> IO ()
testEvals n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (let r = (evalErr t) in
            case r of
              (Left v) -> v == (eval t)
              (Right v) -> True))
{% endhighlight %}

Running `testEvals` on a thousand test cases should generate no errors.

Where QuickCheck was helpful in earlier tests, it shines here.  We updated `eval` and we want to make sure we did not introduce errors.  This is exactly what QuickCheck's random testing does for us.  Whether developing interpreters or other tools, this technique will prove exceptionally useful.

## Static Error Prediction

If you get anything out of this missive, it should be that programs are simply data structures.  We can write programs that evaluate them, look at them, transform them, and synthesize them.  They are data structures and can be treated like any other data structure.

In that spirit, let's look at our first example of an application that predicts failure before evaluating a program.  The specific failure we will look for is the same failure we caught dynamically in the previous example.  Specifically, when the types of arguments to an operator do not match that operator - a type checker.  This is an example of *static analysis* where we want to say something about a program without actually running it.

### Type Rules

Like $\eval$ earlier, let's define a set of rules for the $\typeof$ function.  First the values:

$$\frac{}{\typeof \NUM = \tnum}\; [NumT]$$

$$\frac{}{\typeof \ttrue = \tbool}\; [TrueT]$$

$$\frac{}{\typeof \ffalse = \tbool}\; [FalseT]$$

Here we simply define axioms that give numbers, $\ttrue$ and $\ffalse$ their appropriate types.  It is important that each `ABE` expression have exactly one type.  Given there is only one rule for each value, this condition is satisfied for values.

Next we'll start with numerical operations from `AE`:

$$\frac{\typeof t_1 = \tnum,\; \eval t_2 = \tnum}{\typeof t_1 + t_2 = TNum}\; [PlusT]$$

$$\frac{\typeof t_1 = \tnum,\; \eval t_2 = \tnum}{\typeof t_1 + t_2 = \tnum}\; [MinusT]$$

In both rules the antecedents place requirements on the types of the operation's arguments.  Addition is a number if its arguments are numbers.  Similarly for subtraction.

Compare these rules with the rules for $\eval$.  Notice anything interesting? Let's keep going with the next three `ABE` operations:

$$\frac{\typeof t_1 = \tbool,\; \typeof t_2 = \tbool}{\typeof t_1 \aand t_2 = \tbool}\; [AndT]$$

$$\frac{\typeof t_1 = \tnum,\; \typeof t_2 = \tnum}{\eval t_1 \lleq t_2 = \tbool}\; [LeqT]$$

$$\frac{\typeof t = \tnum}{\typeof \iisZero t = \tbool}\; [isZeroT]$$

These operations are almost identical to the addition and subtraction operations.  Requirements are placed on the argument types and if those requirements are met, the specified term is given a type.  Once again there is only one rule for each expression assuring that each expression will have only one type.

The $\iif$ expression is a bit more interesting:

$$\frac{\typeof t_0 = \tbool,\;\typeof t_1 = T,\;\typeof t_2 = T
}{\typeof \iif t_0 \tthen t_1 \eelse t_2 = T}\;[IfT]$$

The condition is required to be $\tbool$ as expected, but the true and false cases are required to be of type $T$.  $T$ in this context is a *type variable* that can take any type value.  What the second two antecedents say is that both the true and false cases must have the same type, but that type can be either $\tbool$ or $\tnum$.

The $IfT$ rule ensures that any $\iif$ expression has only one type.  If the two cases were allowed to have different types, then  the $\iif$'s type cannot be predicted without knowing the value of the conditional.  This will only be known *dynamically* and we are trying to predict errors *statically*.  By requiring true and false cases to have the same type, we know that the $\iif$ expression will have that single type.

The $\typeof$ relation implements *type inference* where we calculate a type for an expression.  Haskell uses type inference extensively, but you're likely more familiar with languages that implement *type checking*.  In type checking we don't necessarily calculate a type, but instead annotate expressions with types and check to see if those annotations hold.  A function $\mathsf{typecheck}$ would accept an expression and a type as arguments and return a Boolean value if the expression has that type.  We'll say that an expression, $t$, is *well-typed* if $(\typeof t)$ is defined or $(\mathsf{typecheck} e t)$ is true.  As an exercise you will implement $\mathsf{typecheck}$ with $\typeof$.

Back to comparison with $\eval$ rules.  Do you see the parallel between $\eval$ rules and $\typeof$ rules?  There is a one-to-one correspondence between the rules.  They are structured the same way and as we'll see soon, they will be implemented in roughly the same way.  This is not always true, but the similarity is something we'll revisit in later discussions.

### Typeof

Back to building things.  We'll build `typeof` by defining a function that predicts the type of an expression according to the type rules above.  The `typeof` function will take an `ABE` structure and return either its type or an error message:

{% highlight haskell %}
typeof :: ABE -> Either TABE String
{% endhighlight %}

We're building `typeof` like our new `eval` function so we can catch errors rather than use Haskell for reporting.

`typeof` needs to return a type, yet `ABE` has no terms for types. We will handle this buy simply defining a new type for representing `ABE` types.  `TABE` is a new data type representing the two types of `ABE` expressions:

{% highlight haskell %}
data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)
{% endhighlight %}

$\tnum$ and $\tbool$ from our mathematical definitions correspond with `TNum` and `TBool` in the type definition.

Given an `ABE` expression, `typeof` will return its type if it is well-typed and fail if it is not.  The cases for `Num` and `Boolean` are trivial and simply return their associated types, `TNum` and `TBool` respectively:

{% highlight haskell %}
typeof (Num x) = (Left TNum)
typeof (Boolean b) = (Left TBool)
{% endhighlight %}

They are both identical to their associated type rules from above.

`Plus` and `Minus` are also virtually identical to their associated rules $PlusT$ and $MinusT$.  The types of their arguments are checked to determine if they are type `TNum`.  If so, both are also of type `TNum`:

{% highlight haskell %}
typeof (Plus l r) = let l' = (typeof l)
                        r' = (typeof r)
                     in if l'==(Left TNum) && r'==(Left TNum)
                        then (Left TNum)
                        else Right "Type Mismatch in +"
typeof (Minus l r) = let l' = (typeof l)
                         r' = (typeof r)
                     in if l'==(Left TNum) && r'==(Left TNum)
                        then (Left TNum)
                        else Right "Type Mismatch in -"
{% endhighlight %}

More of the same for `And`, `Leq` and `IsZero`.  Each Haskell case for `typeof` virtually identical to its associated type rule:

{% highlight haskell %}
typeof (And l r) = if (typeof l) == (Left TBool)
                      && (typeof r) == (Left TBool)
                   then (Left TBool)
                   else Right "Type mismatch in &&"
typeof (Leq l r) = if (typeof l) == (Left TNum) && (typeof r) == (Left TNum)
                   then (Left TBool)
                   else Right "Type mismatch in <="
typeof (IsZero v) = if (typeof v) == (Left TNum)
                    then (Left TBool)
                    else Right "Type mismatch in IsZero"
{% endhighlight %}

Finally `if` and we're done.  `if` checks the types of its conditional to determine if it is Boolean and they checks to see if the types of the true and false conditions are the same.  If so, the type is returned:

{% highlight haskell %}
typeof (If c t e) = if (typeof c) == (Left TBool)
                       && (typeof t)==(typeof e)
                     then (typeof t)
                     else Right "Type mismatch in if"
{% endhighlight %}

The result is the following `typeof` definition:

{% highlight haskell %}
typeof :: ABE -> TABE
typeof (Num x) = TNum
typeof (Plus l r) = let l' = (typeof l)
                        r' = (typeof r)
                    in if l'==TNum && r'==TNum
                       then TNum
                       else error "Type Mismatch in +"
typeof (Minus l r) = let l' = (typeof l)
                         r' = (typeof r)
                     in if l'==TNum && r'==TNum
                        then TNum
                        else error "Type Mismatch in -"
typeof (Boolean b) = TBool
typeof (And l r) = if (typeof l) == TBool && (typeof r) == TBool
                   then TBool
                   else error "Type mismatch in &&"
typeof (Leq l r) = if (typeof l) == TNum && (typeof r) == TNum
                   then TBool
                   else error "Type mismatch in <="
typeof (IsZero v) = if (typeof v) == TNum
                    then TBool
                    else error "Type mismatch in IsZero"
typeof (If c t e) = if (typeof c) == TBool
                       && (typeof t)==(typeof e)
                    then (typeof t)
                    else error "Type mismatch in if"
{% endhighlight %}

### Interpreting with typeof

The `typeof` function gives the type of an `ABE` expression if it is well-typed and generates an error message if it is not.  We can now predict type errors before we evaluate an `ABE` expression.  We call `typeof` before `eval` and only call `eval` if `typeof` results in a type.  Here is one way to do that:

{% highlight haskell %}
interpTyped :: String -> Either ABE String
interpTyped e = let p=(parseABE e) in
                  case (typeof p) of
                    (Left _) -> (Left eval p)
                    (Right m) -> (Right m)
{% endhighlight %}

`interpTyped` is does exactly what we need.  It parses and calls `typeof` on its input argument.  The `case` chooses between `Left` that contains a type and `Right` that contains an error message.  `eval` is called on the parsed input if a type is returned while an error message is simply returned in the error case.  Note that we're using `Either` the same way we did with the runtime error interpreter.  This is simply for consistency and will help us when  we start testing.

### QuickCheck

QuickCheck is going to serve us exceptionally well again.  We can test the type checker in the same manner as we have tested interpreters by calling `typeof` on random inputs.  However let's skip that step and move to something more interesting.

The first property we would like to check is whether `typeof` statically predicts runtime errors.  We can do this by generating random inputs, calling `typeof` on each input, and `eval` on the result only when `typeof` does not generate an error message.  If `eval` never crashes, then typeof is catching at least all the errors `eval` crashed on.

{% highlight haskell %}
testTypedEval :: Int -> IO ()
testTypedEval n = quickCheckWith stdArgs {maxSuccess=n}
                  (\t -> case typeof t of
                           (Left _) -> eval (parseABE (pprint t)) == (eval t)
                           (Right _) -> True)
{% endhighlight %}

Note that we're calling `eval` as before by parsing the printed arbitrary term.  This is not entirely necessary, but allows us to do some sanity checking in this set of tests.

Note that `typeof` may still not be correct even though it prevents crashs.  If our `typeof` function were defined as:

{% highlight haskell %}
typeof e = (Right "Ha!")
{% endhighlight %}

it would pass the above test!  Thus, it is not sufficient to run just this test.  Correctness testing is also necessary.

When we wrote `evalErr` we tested it against our original evaluation function.  Let's test our `interpErr` function against `interpTyped` to see if the type checker catches the same errors that are caught at run time.  Let's try the simple solution first and compare the results of the two interpreters on the same input.  Recall that we defined both to return `Either ABE String` so we can simply compare their results directly:

{% highlight haskell %}
(\t -> (interpTyped t) == (interpErr t))
{% endhighlight %}

Even a small number of test cases reveals a problem.  If both interpreters produce a value and not an error message, everything is fine.  If not, the error messages don't match and the equality test fails.  What we want to know is whether both interpreters produce the same result *modulo specific error messages*.[^1]

Instead of using strict equality, we can use a weaker comparison:

{% highlight haskell %}
eqInterp :: Either ABE String -> Either ABE String -> Bool
eqInterp s t =
  case s of
    (Left x) -> case t of
                  (Left y) -> x == y
                  (Right _) -> False
    (Right x) -> case t of
                   (Left y) -> False
                   (Right _) -> True
{% endhighlight %}

In `eqInterp` interpretation results are compared directly for values and specific messages ignored for errors.  We can now use this in a proposition for checking:

This proposition calls `typeof` and `evalErr` on the same arbitrary term and determines if they both generate an error or `eval` and `evalErr` both generate the same value:

{% highlight haskell %}
testTypedErrEval :: Int -> IO ()
testTypedErrEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in (eqInterp (interpTyped t') (interpErr t')))
{% endhighlight %}

Effectively, `testTypedErrEval` determines if the two interpreters are equivalent.  Let's call `testTypedErrEval` on 1000 test cases to make sure.  Here's what I got on my first attempt:

{% highlight haskell %}
Main> testTypedErrEval 1000
*** Failed! Falsifiable (after 92 tests): 
If (Boolean False) (Boolean False) (Num 59)
{% endhighlight %}

An error is not what we expected!  We know already that `eval` and `evalErr` produce the same value in cases where a term does not generate an error.  Why is this different?  Examining the counterexample QuickCheck provides gives us a clue.

The concrete syntax for the counterexample is:

{% highlight text %}
if false then true else 59
{% endhighlight %}

According to the $IfT$ type rule, this term does not type check. The first term is Boolean as required, but the second two terms do not have the same type.  Thus, the antecedents of $IfT$ are violated.

According to the implementation of $interpErr$, this term evaluates to 59.  The condition is `false`, thus the `else` expressin is evaluated.

What gives?  Which is correct?

In a very real sense, both are.  Error checking at runtime as implemented in our `interpErr` is what languages like Scheme do.  It's quite common to have constructs like:

{% highlight racket %}
(if x 3 "oops")
{% endhighlight %}

in Scheme.  The calling code must deal with all possible outcomes of evaluating `if`.  What `interpType` does is what languages like Haskell do.

Both interpreters implement the same language.  What should we check?  Let's break the equality in half.  Specifically: (i) if `interpErr` returns a value see if `interpTyped` returns the same value; and (ii) if `interpTyped` returns a value see if `interpErr` returns the same value.  Here are the QuickCheck properties:

{% highlight haskell %}

testErrThenTyped :: Int -> IO ()
testErrThenTyped n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in
           case (interpErr t') of
             (Left v) -> (Left v) == interpTyped t'
             (Right _) -> True)
               
testTypedThenErr :: Int -> IO ()
testTypedThenErr n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in
           case (interpTyped t') of
             (Left v) -> (Left v) == interpErr t'
             (Right _) -> True)
{% endhighlight %}

Running each on 1000 cases reveals the first property does not hold, while the second does.  Static type checking is *more conservative* that run-type type checking.  An interesting result that we will revisit later.

## Discussion

We end this chapter with two interpreters for `ABE` that are not equivalent.  Our first QuickCheck experiment quickly demonstrated that the `if` expression is handled differently when checked dynamically than when checked statically.  Our last two tests established that checking statically is more conservative than checking dynamically.  Specifically, handling errors at run time allowed more programs to execute than static type checking.

Which interpreter is correct?  As it turns out, both are correct for two reasons.  First, the inference rules for `ABE` do not describe failure.  They only describe successful computations leaving failure completely up to the implementer.  Thus, neither of our `ABE` implementations violate the language definition.

Second, `ABE` and `ABE` with types are two different languages.  `ABE` with types (`ABET`[^2]) uses the same rules for defining evaluation, but implicitly says that only languages satisfying its associated `typeof` function should be interpreted.  The `typeof` definition becomes a part of the `ABET` language.  We'll discuss this further in later chapters.

An interesting question is whether the `ABE` interpreters can be made equivalent.  The issue is in the `if` statement where the original interpreter does not require the true and false cases to have the same type while the typed interpreter does.  The simple answer is no.  Making the `ABE` interpreter with dynamic error handling catch cases where the true and false cases are not the same type requires having the interpreter predict types or execute both arms.  Making the type checker allow cases where the types are different, but the interpreter does not crash requires predicting how the result of the `if` will be used.  In essense, each interpreter would be required to implement the other.

## Definitions

- Host Language - The language used to implement tools for another language.
- Domain Specific Language - The new language for which we're building tools.
- Static - Before execution
- Dynamic - During execution
- Well-typed - An expression is well-typed if its type can be calculated

## Exercises

1. Modify `interpErr` to make error messages values in `ABE` rather than use the `Either` type.  You should update the `ABE` AST to include a new constructor, `Error` that is a constant like `Num` and `Boolean`.
2. Add operations for multiplication and addition to `ABE` and implement them in `interp` and `interpErr`.  Make sure that `interpErr` continues to avoid crashing.  Use QuickCheck to ensure the interpreters agree on their behavior.
3. Using `typeof` implement a function `typecheck` that accepts an expression and a type and returns true if the expression has that type.
4. Add multiplication and division to the `ABE` interpreter with run-time error handling.  Update definitions for concrete syntax, abstract syntax, inference rules for `eval`, and implementations.  If you can implement divide by zero error catching, do so.
5. Add multiplication and division to the `ABE` interpreter with type checking.  Update definitions for concrete syntax, abstract syntax, inference rules for `eval` and `typeof`, and implementations.  If you can implement divide by zero error catching in `typeof`, do so.

## Source

Download [source]({{site.baseurl}}/haskell/abe.hs) for all interpreter code from this chapter.

## Notes

[^1]:Modulo error messages implies the values are the same except for differences in the specific message.  (Right "Error message 1") and (Right "Different error message") are equivalent modulo error message.

[^2]:The `ABET` is a tribute to my friend and colleague Nancy Kinnersley who passed away this past summer.  She was committed to service through the ABET accreditation organization.  Seems only fitting.
