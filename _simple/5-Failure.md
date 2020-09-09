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
\newcommand\bbool{\mathsf{Boolean}\;}
\newcommand\nnum{\mathsf{Num}\;}
$$

# Failure Is an Option

We've established that `ABE` fails for test cases that attempt to use `Boolean` values where numbers should be used or numbers where Boolean values should be used.  Before addressing that specific problem, let's look at how failure should be handled generally.  But first, a bit of new terminology.

We are defining a new language called `ABE` using Haskell to build parsers and interpreters.  We will refer to `ABE` as the *external language* or *domain specific language*.  We refer to Haskell as the *host language* or *implementation language*.  The distinction is we are building tools for `ABE` and using Haskell to build them.

When the current `ABE` interpreter fails, it fails in the host language.  When we call the Haskell `error` function we jump out of our interpreter and generate a Haskell error message.  This approach works, but we cannot control errors and our only choice is hard failure.

We have no control over errors.  They happen and our control of execution ends.  Java uses an innovative approach where exceptions are Java data structures that allow us to write Java programs to process them.  This is how systems like Eclipse can allow new tools that generate error messages to simply be plugged into the infrastructure.  Right now, we can't do this.

Our only choice is to fail completely.  What if our interpreters can avoid failures?  What if we can predict failures during or before execution?  This results in systems that are more robust and code that we can better control.

Let's look at two approaches to handling errors.  The first will handle them *dynamically or at run-time.  Our interpreter will generate error messages as data structures that we can process how we choose.  The second will handle them *statically* by predicting runtime failure before execution.  We will still have run-time errors, but substantially fewer.

## Runtime Error Prediction

We will update our `ABE` evaluator to catch errors and run time rather than falling into Haskell using the `error` function.  The new evaluator will return either a value or an error message that we can handle however we want.

### Eval

Let's change the type signature of the `ABE` `eval` function just a bit and define a new function called `evalErr` that returns either an `ABE` term or a string representing an error:

```haskell
evalErr :: ABE -> Maybe ABE
```

Let's start with the two easy cases for number and Boolean constants.

```haskell
evalErr (Num t) = (return (Num t))
evalErr (Boolean b) = (return (Boolean b))
```

In both cases there is no need to evaluate tne term because `Num` and `Boolean` are in fact values.  The statements `(return (Num t))` and `(return (Boolean t))` do just what we need.  Remember that for the `Maybe` monad `return = Just`, so both statements simply create an instance of `Just` and return it.

Constant cases are not particularly interesting, so let's look at `isZero`.  Unlike the constant cases, `isZero` can fail when evaluating its argument or when the argument is not the right type.:

```haskell
evalErr (IsZero t) =
  do r <- (evalErr t)
     case r of
       (Num v) -> (return (Boolean (v == 0)))
       _ -> Nothing
```

First the argument to `IsZero` is evaluated and bound to `v` using the `do` notation.  If `eval t` returns `Just x` then `r` is bound to `x`.  When building `eval`, we were pretty much done.  Compare the result to zero and return result using `return` and `Right` and crash if somehow the comparison fails.  Here we catch that failure using the `case` operation over `r`.  If `r` is a `Num`, then we return exactly what we returned before.  The construction is a bit different as we use pattern matching to project `v` from `Num v` to perform the comparison with `0`

If `r` is anything but a number, we immediately return `Nothing`.  `Nothing` represents an error in this implementation.  Remember that `return = Just`, so `return Nothin` is not well typed and not what we want.  Simply returning `Nothing` directly indicates an error.  Note that any operation consuming the error result will simply pass it through due to the use of the `Maybe` monad.  So, we don't need to implement all kinds of error checking.

The remaining binary operations are virtually the same except we have two arguments to evaluate and need to nest handling argument results.  Look first at `Plus`:

```haskell
evalErr (Plus t1 t2) =
  do r1 <- (evalErr t1)
     r2 <- (evalErr t2)
     case r1 of
       (Num v1) -> case r2 of
                     (Num v2) -> (return (Num (v1+v2)))
                     _ -> Nothing
       _ -> Nothing
```

There is no magic here!  We calculate the values of both arguments and bind the results to `r1` and `r2` respectively.  Then we apply the same pattern as `IsZero` and determine if the first argument is a number or something else. In the number case, we repeat the same process for `r2` and do the same thing.  In the something else cases, we do exactly what we did previously and return `Nothing`.  In the number case, we calculate the result of `Plus` and return it as `(return (Num (v1+v2)))`.  The other binary operations follow similarly.

The remaining operation is `if`.  The condition is evaluated and the outcome handled using the same pattern as other expressions.  If the condition evaluates to a Boolean, them we choose the expression to evaluate based on the Boolean value.  The final code has the following form:

```haskell
evalErr (If t1 t2 t3) =
           do r <- (evalErr t1)
              case r of
                (Boolean v) -> if v then (evalErr t2) else (evalErr t3)
                _ -> Nothing
```

`If` follows the same pattern as `isZero`.  The condition is evaluated first and bound to `r` if no error is generated.  If `Nothin` results from evaluating the condition, then `Nothing` falls through.  (See the monadic patter at work?)  `r` is then used to determine which arm of the `If` to evaluate and `evalErr` called as appropriate.  Note that we do not use `return` here as the `evalErr` result will be of the right type without lifting with `Just`.

Once the interpreter is completed, we can define an interpreter function in a manner similar to the original interpreter function for `eval`:

```haskell
interpErr = evalErr . parseABE
```

Now we're set to test our new interpreter.

### QuickCheck

The same functions used for testing the original `ABE` evaluator are also useful for `evalErr`.  We simply substitute `interpErr` for `interp` in the test functions:

```haskell
testEvalErr :: Int -> IO ()
testEvalErr n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interpErr $ pprint t) == (evalErr t))
```

Running `testEvalErr` for 1000 random cases reveals that we accomplished our original goal of having an evaluator for `ABE` that does not crash.

However, we can do more.  Let's compare `eval` and `evalErr` to assess whether our new implementation agrees with our original implementation.  Unfortunately, this is not as simple as creating a function that compares the results of `eval` and `evalErr` like this:

```haskell
\t -> eval t == evalErr t
```

because the return type of `eval` is different than the return type of `evalErr` and `eval` still crashes.  What we care about are cases when `eval` should not crash and produce a value.  We can't test for  `eval` not crashing, but we can test for when `evalErr` produces a value rather than an error.  Remember `Right` and `Left`?  When `evalErr` returns a `Right` value we know it produced a value and `eval` should also produce a value.  When `evalErr` returns a `Left` we know it produced an error message and we should not evaluate `eval`.  Here's a function to do just this:

```haskell
(\t -> (let r = (evalErr t) in
          case r of
            (Right v) -> v == (eval t)
            (Left v) -> True))
```

The `case` performs exactly the check we need.  `Right` compares the value generated with the results of `eval` on `t`.  `Left` just returns  `True`.  Why?  QuickCheck checks to see if the conjunction of all tests succeed.  `True` causes QuickCheck to ignore the case.  Exactly what we want.  Here's the QuickCheck function:

```haskell
testEvals :: Int -> IO ()
testEvals n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (let r = (evalErr t) in
            case r of
              (Right v) -> v == (eval t)
              (Left v) -> True))
```

Running `testEvals` on a thousand test cases should generate no errors.

Where QuickCheck was helpful in earlier tests, it shines here.  We updated `eval` and we want to make sure we did not introduce errors.  This is exactly what QuickCheck's random testing does for us.  Whether developing interpreters or other tools, this technique will prove exceptionally useful.

## Static Error Prediction

If you get anything out of this missive, it should be that programs are simply data structures.  We can write programs that evaluate them, look at them, transform them, and synthesize them.  They are data structures and can be treated like any other data structure.

In that spirit, let's look at our first example of such an application that predicts failure before evaluating a program.  The specific failure we will look for is the same failure we caught dynamically in the previous example.  Specifically, we will predict when arguments to an operator do not match what the operator expects before execution.  Instead of running code and watching for errors, we will predict errors before running the code.  This is an example of *static analysis* where we want to say something about a program without actually running it.

To understand where we're headed think about the operations `2+3` and `false+3`.  If we interpret `2+3` we will get a value where interpreting `false+3` throws an error.  The problem is that `+` is not defined for `false`. We saw this earlier when our interpreter crashed and when we caught the error at run-time.

We caught the error by looking at the argument evaluation result's constructors.  Evaluating `2` and `3` before adding gives two results that are instances of `Num`.  The interpreter sees the `Num` constructor and can infer both arguments have evaluated to numbers.  Evaluated `false` does not give a number, but instead gives a Boolean constructed with `Boolean`.

What we've discovered is that `+` will only operate on number values and that number values are always constructed with `Num`.  Similarly, `&&` only operates on Boolean values and all Boolean values are constructed with `Boolean`.  Again similarly, `<=` only operates on number values and number values are always constructed with `Num`.  It should be clear we are looking at sets of values constructed with a specific constructor.

If we want to predict failure, we need to predict constructors.  `t1 + t1` will execute correctly if evaluating `t1` and `t2` results in something constructed with `Num`.  Said mathematically:

$$\eval t_1 \in \{(\nnum x) \mid x\in Int\}$$
$$\eval t_2 \in \{(\nnum x) \mid x\in Int\}$$

where $Int$ is the Haskell `Int` type.  Note that both $t_1$ and $t_2$ must belong to the same set of nuymber values created with $\nnum$.  Let's give this set a name, $\tnum$:

$$\tnum == \{(\nnum x) \mid x\in Int\}$$

Now the question becomes whether we can predict $\tnum$ from $t_1$ and $t_2$ without executing either.  `eval` is defined by including one case for each language constructor in its definition.  Can we do the same here?  Let's try by defining a function we'll never implement called `predict`.  First, let's take care of our values:

```haskell
predict (Num _) = TNum
predict (Boolean _) = TBool
```

Because `TNum` is defined as everything created using the `Num` constructor, the first definition should be obvious.  Certainly anything created with `Num` can be identified as belonging to `TNum`.  The same holds for `Boolean` except the set is defined just like `TNum` as:

$$\tbool == \{(\bbool x) \mid x \in Bool\}$$

Next let's look at one of our binary operations, `+`.  We decided earlier than `+` operates on `TNum` values, so we need to check that its arguments are in `TNum`.  If they are, then `+` produces a number value in `TNum`.  Again writing a definition for `predict` over the abstract syntax for `+` gives:

```haskell
predict Plus t1 t2 = if predict t1==TNum && predict t2==TNum then TNum else error
```

Forget about the error and focus on the `then` clause.  When both `t1` and `t2` are predicted to be numbers, `t1+t2` is predicted to be a number.  How about `<=`:

```haskell
predict Lte t1 t2 = if predict t1==TBool && predict t2==TBool then TNum else error
```

One can imagine doing the same thing for other constructions in `ABE`.  Which constructions can this not be done for?  As it turns out, none.  We can predict the set associated with any expression in `ABE`.

What is going on here is a simple form of *type inference*.  If we treat `TNum` and `TBool` is the names of types, then `predict` is a function that returns the type of an expression that we will call `typeof`.  It predicts what set - or *type* - an expression's associated value is in.

The `typeof` function is simply another interpreter for the `ABE` language.  `typeof` takes an expression in `ABE` and evaluates it just like `eval`, except the values associated with `typeof` are `TNum` and `TBool`.  Programs are just data structures and can be interpreted in many ways.  `typeof` is one such alternate interpretation.

### Type Rules

Like `eval` earlier, let's define a set of rules for our new `typeof` function before implementing it.  These rules will define the relation $t:T$ that is read _t of type T_. The same notation defining antecedents and consequents can be used to define each rule.

First the constant values:

$$\frac{}{\NUM : \tnum}\; [NumT]$$

$$\frac{}{\ttrue : \tbool}\; [TrueT]$$

$$\frac{}{\ffalse : \tbool}\; [FalseT]$$

Here we simply define axioms that give numbers, `true`, and `false` their associated types.  It is important that each `ABE` expression have exactly one type, thus there is precisely one axiom for each value.

Next we'll define types for numerical operations from `AE`:

$$\frac{t_1 : \tnum,\; t_2 : \tnum}{t_1 + t_2 : \tnum}\; [PlusT]$$

$$\frac{t_1 : \tnum,\; t_2 : \tnum}{t_1 + t_2 : \tnum}\; [MinusT]$$

In both rules the antecedents place requirements on the types of the operation's arguments.  Addition is a number if its arguments are numbers.  Similarly for subtraction.  Both addition and subtraction have no defined type if their arguments are not numbers.

Compare these rules with the rules for `eval`.  Notice anything interesting? Let's keep going with the next three `ABE` operations for `&&`, `<=`, and `isZero`:

$$\frac{t_1 : \tbool,\; t_2 : \tbool}{t_1 \aand t_2 : \tbool}\; [AndT]$$

$$\frac{t_1 : \tnum,\; t_2 : \tnum}{t_1 \lleq t_2 : \tbool}\; [LeqT]$$

$$\frac{t : \tnum}{\iisZero t : \tbool}\; [isZeroT]$$

These operations are almost identical to the addition and subtraction operations.  Requirements are placed on the argument types and if those requirements are met, the specified term is given a type.  Once again there is only one rule for each expression assuring that each expression will have only one type.

The `if` expression is a bit more interesting:

$$\frac{t_0 : \tbool,\; t_1 : T,\; t_2 : T
}{\iif t_0 \tthen t_1 \eelse t_2 : T}\;[IfT]$$

The condition is required to be `TBool` as expected.  However, the then and else cases are both required to be of unknown type $T$.  $T$ in this context is a *type variable* that can take any type value.  Thus, the arms of an `if` expression can be either Boolean or numbers as long as they are the same.  Unlike all other terms, all instances of `if` do not have the same type.  Is this a problem?

The $IfT$ rule ensures that any specific `if` expression has only one type.  If the two cases were allowed to have different types, the `if`'s type cannot be predicted without knowing the value of the conditional.  This will only be known *dynamically* and we are trying to predict errors *statically*.  By requiring true and false cases to have the same type, we know that the $\iif$ expression will have that single type.

The $\typeof$ function implements *type inference* where we calculate a type for an expression.  Haskell uses type inference extensively, but you're likely more familiar with languages that implement *type checking*.  In type checking we don't necessarily calculate a type, but instead annotate expressions with types and check to see if those annotations hold.  A function `typecheck` would accept an expression and a type as arguments and return a Boolean value if the expression has that type.  We'll say that an expression, $t$, is *well-typed* if `typeof t` is defined or `typecheck e t` is true for some type `t` and `e`.

Back to comparison with `eval`.  Do you see the parallel between $\eval$ rules and $\typeof$ rules?  There is a one-to-one correspondence between the rules.  They are structured the same way and as we'll see soon, they will be implemented in roughly the same way.  This is not always true, but the similarity is something we'll revisit in later discussions.

### Typeof

We'll build a functioned called `typeof` by defining a function that predicts the type of an expression.  The `typeof` function will take an `ABE` structure and return either a type or an error indicator.  The `Maybe` monad will prove useful as it has before.  The signature of `typeof` is:

```haskell
typeof :: ABE -> Maybe TABE
```

where `TABE` is the type of an `ABE` expression.  We're structuring `typeof` like `eval` so we can catch errors rather than use Haskell for reporting.

We need to define `TABE` to represent all term types in `ABE`.  There are only two, number and Boolean and we've given them names already - `TNum` and `TBool`.  It's a simple matter to define the new type `TABE` for representing `ABE` types:

```haskell
data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)
```

`TNum` is the type of numbers and `TBool` is the type of Booleans.

Given an `ABE` expression, `typeof` will return its type using `Just` if it is well-typed and return `Nothing` if it is not.  Like `eval`, if we define one case for each `ABE` expression, we will completely define `typeof` for ABE.

The cases for the `Num` and `Boolean` constructors are trivial.  All `Num` constructions are of type `TNum` and `Boolean` constructions are of type `TBool`.  The `typeof` cases for `Num` and `Boolean` simply return their associated types, `TNum` and `TBool` respectively:

```haskell
typeof (Num _) = return TNum
typeof (Boolean _) = return TBool
```

Note that each value has precisely one type as specified by the first three type rules.

Next we'll consider the `Plus` and `Minus` operations.  Both require their arguments to be of type `TNum`.  If they are, then the operation is also of type `TNum`.  In the following code snippet, arguments to `Plus` and `Minus` are checked to determine if they are type `TNum`.  If so, `(Just TNum)` is returned to indicate a type was found and it is `TNum`:

```haskell
typeof (Plus l r) = do l' <- (typeof l)
                       r' <- (typeof r)
                       if l'==TNum && r'==TNum
                         then return TNum
                         else Nothing
typeof (Minus l r) = do l' <- (typeof l)
                        r' <- (typeof r)
                        if l'==TNum && r'==TNum
                          then return TNum
                          else Nothing
```

If either argument type is not `TNum`, then  `Nothing` is returned indicating an error.  It still holds that if either `l` or `r` are not well-typed, `Nothing` is returned and falls through the `do` indicating an error.

Before moving on, take note of the similarity between the code for `Plus` and its associated inference rule:

$$\frac{t_1 : \tnum,\; t_2 : \tnum}{\typeof t_1 + t_2 : \tnum}\; [PlusT]$$

The antecedent types are found and compared with `TNum` just as specified in the rule.  One could easily imagine an automatic transformation from rules like this to Haskell code.

It's more of the same for `And`, `Leq` and `IsZero` except they require different argument types and produce different types.  `And` requires Boolean arguments and produces a Boolean type.  `Leq` requires numerical arguments and produces a Boolean type.  Finally, `isZero` requires a numerical argument and produces a Boolean type:

```haskell
typeof (And l r) = do l' <- (typeof l)
                      r' <- (typeof r)
                      if l'==TBool && r'==TBool
                        then (return TBool)
                        else Nothing
typeof (Leq l r) = do l' <- (typeof l)
                      r' <- (typeof r)
                      if l'==TNum && r'==TNum
                        then (return TBool)
                        else Nothing
typeof (IsZero t) = do t' <- (typeof t)
                       if t'==TNum
                         then (return TBool)
                         else Nothing
```

In each case, an error is return if arguments are not of the correct type.  In each case the structure of the Haskell code matches the structure of the inference rule.

`if` is the most interesting of the `ABE` expressions.  Unlike other expressions, different `if` expressions do not always have the same type.  Consider two examples:

```
if x then 5 else 7+1
if x then true else 7<=1
```

where the first expression has type number and the second has type Boolean.  Is this expression okay:

```
if x then 5 else true
```

If `x` is true, then the type of the `if` is clearly number.  But if `x` is false, then the type of the `if` is clearly Boolean.  What gives?  Can the `if` expression have two types?  For an answer, think about the expression:

```
1 + if x then 5 else true
```

The addition operation requires both arguments to be numbers.  In this case, we can't say whether the `if` is or is not a number until we know the value of `x`.  This is key.  It's one thing to predict the type of `x`, but the value requires evaluating `x`.  Running the evaluator during type checking makes the two mutually recursive and is not wise.  At least for the time being.

What we need is for the type to be independent of the Boolean condition.  If both `if` outcomes have the same type, then no matter then value of the conditional the expression has the same type.  Thus, `typeof` checks the type of its conditional to determine if it is Boolean and then checks to determine if the types of the true and false conditions are the same.  If so, the type is returned.  If not, an error is returned.  Putting all this together, the `If` expression's type is checked as follows:

```haskell
typeof (If c t e) = do c' <- (typeof c)
                       t' <- (typeof t)
                       e' <- (typeof e)
                       if c' == TBool && t'==e'
                         then (return t')
                         else Nothing
```

If the condition is not Boolean or the result types are not the same, then `typeof` returns an error message using `Nothing`.

Putting everything together for all expressions, the result is the following `typeof` definition:

```haskell
typeof :: ABE -> Maybe TABE
typeof (Num x) = (return TNum)
typeof (Plus l r) = do l' <- (typeof l)
                       r' <- (typeof r)
                       if l'==TNum && r'==TNum
                         then return TNum
                         else Nothing
typeof (Minus l r) = do l' <- (typeof l)
                        r' <- (typeof r)
                        if l'==TNum && r'==TNum
                          then return TNum
                          else Nothing
typeof (Boolean b) = (return TBool)
typeof (And l r) = do l' <- (typeof l)
                      r' <- (typeof r)
                      if l'==TBool && r'==TBool
                        then (return TBool)
                        else Nothing
typeof (Leq l r) = do l' <- (typeof l)
                      r' <- (typeof r)
                      if l'==TNum && r'==TNum
                        then (return TBool)
                        else Nothing
typeof (IsZero t) = do t' <- (typeof t)
                       if t'==TNum
                         then (return TBool)
                         else Nothing
typeof (If c t e) = do c' <- (typeof c)
                       t' <- (typeof t)
                       e' <- (typeof e)
                       if c' == TBool && t'==e'
                         then (return t')
                         else Nothing
```

### Interpreting with typeof

The `typeof` function gives the type of an `ABE` expression if it is well-typed and generates an error message if it is not.  We can now predict type errors before we evaluate an `ABE` expression.  We call `typeof` before `eval` and only call `eval` if `typeof` results in a type.  Here is one way to do that:

```haskell
interpTyped :: String -> Maybe ABE
interpTyped e = let p=(parseABE e) in
                  case (typeof p) of
                    (Just _) -> (eval p)
                    Nothing -> Nothing
```

`interpTyped` is does exactly what we need.  It parses its input and calls `typeof` on the result.  The `case` chooses between `Just` that contains a type and `Nothing` that indicates an error.  `eval` is called on the parsed input if a type is returned while an error message is simply returned in the error case.  Note that we're using `Maybe` the same way we did with the runtime error interpreter.  This is simply for consistency and will help us when  we start testing.

We can also define `interpTyped` monadically:

```haskell
interpTypedM :: String -> Maybe ABE
interpTypedM s = do ast <- return (parseABE s)
                    typeof ast
                    (eval ast)
```

Using the `do` notation in this case doesn't have a great deal of benefit, but the function is certainly useful as an alternative to the traditional `interpTyped`.

### QuickCheck

QuickCheck is going to serve us exceptionally well again.  We can test the type checker in the same manner as we have tested interpreters by calling `typeof` on random inputs.  However let's skip that step and move to something more interesting.

The first property we would like to check is whether `typeof` statically predicts runtime errors.  We can do this by generating random inputs, calling `typeof` on each input, and `eval` on the result only when `typeof` does not generate an error message.  If `eval` never crashes, then typeof is catching at least all the errors `eval` crashed on.

```haskell
testTypedEval :: Int -> IO ()
testTypedEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> case typeof t of
           (Just _) -> eval (parseABE (pprint t)) == (eval t)
           Nothing -> True)
```

Note that we're calling `eval` as before by parsing the printed arbitrary term.  This is not entirely necessary, but allows us to do some sanity checking in this set of tests.

Interestingly, `typeof` may not be correct even though it prevents crashs and otherwise seems to work.  If our `typeof` function were defined as:

```haskell
typeof e = Nothing
```

it would pass the above QuickCheck test!  Thus, it is not sufficient to run just this test.  Correctness testing is also necessary.

When we wrote `evalErr` we tested it against our original evaluation function.  Let's test our `interpErr` function against `interpTyped` to see if the type checker catches the same errors that are caught at run time.  Let's try the simple solution first and compare the results of the two interpreters on the same input.  Recall that we defined both to return `Monad ABE` so we can simply compare their results directly:

```haskell
(\t -> (interpTyped t) == (interpErr t))
```

Even a small number of test cases reveals a problem.  If both interpreters produce a value and not an error message, everything is fine.  If not, the error messages don't match and the equality test fails.  What we want to know is whether both interpreters produce the same result *modulo specific error messages*.[^1]

Instead of using strict equality, we can use a weaker comparison:

```haskell
eqInterp :: Maybe ABE -> Maybe ABE -> Bool
eqInterp s t =
  case s of
    (Just x) -> case t of
                  Just y -> x == y
                  Nothing -> False
    Nothing -> False
```

In `eqInterp` interpretation results are compared directly for values and specific messages ignored for errors.  We can now use this in a proposition for checking:

```haskell
(\t -> let t' = pprint t in (eqInterp (interpTyped t') (interpErr t')))
```

This proposition calls `interpTyped` and `interpErr` on the same arbitrary term and determines if they both generate an error or `eval` and `evalErr` both generate the same value:

```haskell
testTypedErrEval :: Int -> IO ()
testTypedErrEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in (eqInterp (interpTyped t') (interpErr t')))
```

Effectively, `testTypedErrEval` determines if the two interpreters are equivalent.  Let's call `testTypedErrEval` on 1000 test cases to make sure.  Here's what I got on my first attempt:

```haskell
Main> testTypedErrEval 1000
*** Failed! Falsifiable (after 92 tests):
If (Boolean False) (Boolean False) (Num 59)
```

An error is not what we expected!  We know already that `eval` and `evalErr` produce the same value in cases where a term does not generate an error.  Why is this different?  Examining the counterexample QuickCheck provides gives us a clue.

The concrete syntax for the counterexample is:

```
if false then true else 59
```

This term does not have a static type because its two outcomes have different types. The first term is Boolean as required, but the second two terms do not have the same type.  Thus, `typeof` throws an error.  However, according to the implementation of `interpErr`, this term evaluates to 59.  The condition is `false`, thus the `else` expression is evaluated.

What gives?  Which is correct?

In a very real sense, both are.  Error checking at runtime as implemented in our `interpErr` is what languages like Scheme do.  It's quite common to have constructs like:

```racket
(if x 3 "oops")
```

in Scheme.  The calling code must deal with all possible outcomes of evaluating `if`.

What `interpType` does is what languages like Haskell do. Both interpreters implement the same language.  What should we check?  Let's break the equality in half.  Specifically: (i) if `interpErr` returns a value see if `interpTyped` returns the same value; and (ii) if `interpTyped` returns a value see if `interpErr` returns the same value.  Here are the QuickCheck properties:

```haskell
testErrThenTyped :: Int -> IO ()
testErrThenTyped n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in
           case (interpErr t') of
             (Just v) -> (Just v) == interpTyped t'
             Nothing -> True)

testTypedThenErr :: Int -> IO ()
testTypedThenErr n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in
           case (interpTyped t') of
             (Just v) -> (Just v) == interpErr t'
             Nothing -> True)
```

Running each on 1000 cases reveals the first property does not hold, while the second does.  Static type checking is *more conservative* that run-type type checking.  An interesting result that we will revisit later.

## Discussion

We end this chapter with two interpreters for `ABE` that are not equivalent.  Our first QuickCheck experiment quickly demonstrated that the `if` expression is handled differently when checked dynamically than when checked statically.  Our last two tests established that checking statically is more conservative than checking dynamically.  Specifically, handling errors at run time allowed more programs to execute than static type checking.

Which interpreter is correct?  As it turns out, both are correct for two reasons.  First, the definition of `ABE` does not describe failure.  The original definition only describes successful computations leaving failure completely up to the implementer.  Thus, neither of our `ABE` implementations violate the language definition.

Second, `ABE` and `ABE` with types are two different languages.  `ABE` with types (`ABET`[^2]) uses the same definition for evaluation, but implicitly says that only languages satisfying its associated `typeof` function should be interpreted.  The `typeof` definition becomes a part of the `ABET` language.  We'll discuss this further in later chapters.

An interesting question is whether the `ABE` interpreters can be made equivalent.  The issue is in the `if` statement where the original interpreter does not require the true and false cases to have the same type while the typed interpreter does.  The simple answer is no.  Making the `ABE` interpreter with dynamic error handling catch cases where the true and false cases are not the same type requires having the interpreter predict types or execute both arms.  Making the type checker allow cases where the types are different, but the interpreter does not crash requires predicting how the result of the `if` will be used.  In essence, each interpreter would be required to implement the other.

## Definitions

- Static - Before execution
- Dynamic - During execution
- Well-typed - An expression is well-typed if its type can be calculated
- Type inference - Predicting the type of an expression without running it

## Exercises

1. Modify `interpErr` to use the `Either` monad rather than `Maybe`.  If you're not familiar with the `Either` type constructor, there is ample documentation of its use.[^3]  `Either` provides two constructors, `Right` and `Left` that encapsulate two different types.  Use the `Either` type to return either an `ABE` value, `v` (`Right v`) or a string error message, `s` (`Left s`).  Now we get an actual error message when failing rather than `Nothing`.  When making this change, remember that `Either` is a monad and that `return == Right`.
1. Modify `interpErr` to make error messages values in `ABE` rather than use the `Maybe` type.  You should update the `ABE` AST to include a new constructor, `Error` that is a constant like `Num` and `Boolean`.
3. Using `typeof` implement a function `typecheck` that accepts an expression and a type and returns true if the expression has that type.
4. Add multiplication and division to the `ABE` interpreter with run-time error handling.  Update definitions for concrete syntax, abstract syntax, inference rules for `eval`, and implementations.  If you can implement divide by zero error catching, do so.
5. Add multiplication and division to the `ABE` interpreter with type checking.  Update definitions for concrete syntax, abstract syntax, inference rules for `eval` and `typeof`, and implementations.  If you can implement divide by zero error catching in `typeof`, do so.

## Source

* Download [source]({{site.baseurl}}/haskell/abe.hs) for all interpreter code from this chapter.
* Download [source]({{site.baseurl}}/haskell/parserUtils.hs) for the parser utilities used by the interpreters.

## Notes

[^1]:Modulo error messages implies the values are the same except for differences in the specific message.  (Left "Error message 1") and (Left "Different error message") are equivalent modulo error message.

[^2]:The `ABET` is a tribute to my friend and colleague Nancy Kinnersley who passed away unexpectedly.  She was committed to service through the ABET accreditation organization.  Seems only fitting.

[^3]:https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Either.html "Either Monad"
