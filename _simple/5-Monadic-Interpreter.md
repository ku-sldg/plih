---
layout: frontpage
title: Monadic Interpreter
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


> In addition to it begin useful, it is also cursed and the curse of the monad is that once you get the epiphany, once you understand - "oh that's what it is" - you lose the ability to explain it to anybody.
>        -- Douglas Crockford

# Monadic Interpreters

The `Either` type class used in both the interpreter and type inference routine for `ABE` is built in to Haskell.  We used `Either` as a construction for pairs using the convention that constructions with `Right` contain values and constructions with `Left` contain error messages.  This made it trivial to use a `case` expression to determine what kind of thing is returned by `eval` or `typeof`.

`Either` is also an instance of class `Monad`.  Our original `eval` definition does not take advantage of this, but we can write tighter functions by taking advantage of `Monad` properties.

## Either and bind

To understand how the `Either` monad is used, let's take a quick look at the monad instance for `Either`:

{% highlight haskell %}
instance Monad (Either e) where
        return = Right
        Right m >>= k = k m
        Left e  >>= _ = Left e
{% endhighlight %}

Recall that all monads define `return` and `>>=`, the infix representation for `bind`.  `return` is defined as the `Right` constructor, so `return x` is the same as `Right x`.  In our initial implementation using `Either`, remember that we used `Right` to construct good values and `Left` to construct error messages.  `Right (Num 1)` returns `1` while `Left "Error Will Robinson"` returns a string.  Hold that thought.  The choice is not at all arbitrary given that we used the built-in `Either` implementation.

Two cases define the behavior of `>>=` for `Either`'s two constructors.  The first says that given `(Right m)` and a function `k` over the type of `m`, call `k` on `m`.  Pretty simple, but lets say it again.  `(Right m) >>= k`  simply takes the `m` and calls `k` on it.

The second case says that given `(Left e)`, just return `(Left e)`.  Again pretty simple, but lets say it again.  `(Left e) >>= k` will simply return `(Left e)` and return it regardless of what `k` is.  `(Left e)` simply passes through the bind operation as if `k` were an identity function.

Remember the choice of `Right` for values and `Left` for errors?
Thinking about `>>=` in those terms it woudl seem `>>=` applies a function to a value and passes an error through.  This is exactly the behavior we want if we're executing operations in sequence.

Let's look at the concept abstractly and then get concrete with some examples. If `x` is a value and `a`, `b`, and `c` were a sequence of 3 operations that might throw errors, the `>>=` behavior is exactly what we want:

1. Apply `a` to `x`.
2. If successful apply `b` to `(a x)`.
3. If not, generate an error, don't apply `b`, and pass the error forward.
4. If applying `(b (a x))` is successful, apply `c` to the result. 
5. If not, generate an error, don't apply `c` and pass the error forward.

If applying `a` generates an error, it will be passed through as if `b` generated it.  If `b` geneates an error, it will be passed through as if `c` generated it.  Keep going and what you'll end up with is either `Right c(b(a(x)))` or `(Left "error message")`.  But *you don't write the code to manage errors*.  The `Either` monad takes care of it for you in the background.  In essence, this is what a monad always does.  It takes care of something in the background that is inherent to the computation being performed.  A monad instance implements a model of computation.

Now we're getting weird.  Let's get a bit more concrete and look at using the `Either` monad and some notations that make it more comfortable.

First let's write an expression that starts with a value, subtracts 10 and squares the result and adds 5:

$$(x-10)^2+5$$

If the original difference is negative or the result of the square is odd we want to throw an error.  First define the three operations as functions using `Either` to encode values and error messages:

{% highlight haskell %}
a = \x -> if x<10 then (Left "less than 10") else (Right (x-10))
b = \y -> if (y `mod` 2)==0 then (Right (y*y)) else (Left "odd result")
c = \z -> (Right (z-5))
{% endhighlight %}

Hopefully it's clear these expressions perform the three operations and check for local errors.  Names for the expressions aren't necessary, but will make things a bit simpler.  Without using either, we can compose these operations to do what we want on the value 10:

{% highlight haskell %}
case (a 10) 
  (Left m) -> (Left m)
  (Right y) -> case (b y)
                 (Left m) -> (Left m)
                 (Right z) -> (c z)
== Right -5
{% endhighlight %}

Not horrible, but when composing the operations you must worry about pushing around the error messages.  Now let's use the `Either` as a monad and take advantage of bind:

{% highlight haskell %}
(Right 10) >>= a >>= b >>= c
== Right (-5)
{% endhighlight %}

Can you tell the difference in the two code sequences?  Let's try two additional cases:

{% highlight haskell %}
(Right 11) >>= a >>= b >>= c
== Left "odd value"

(Right 9) >>= a >>= b >>= c
== Left "less than 10"
{% endhighlight %}

The only price to pay is putting `10` in the `Either` type using `Right` before beginning.  We call this _lifting_ 10 into the `Either` type.  Small price to pay for not managing all of the error handling.  We can even get rid of that by embedding the expression in a function:

{% highlight haskell %}
f x = (Right x) >>= a >>= b >>= c
{% endhighlight %}

Pretty cool.

One more thing - the `do` notation.  I mentioned that as something we would use earlier.  I used names for the various functions I composed in the `Either` example above.  Let's pull the names off and use the expressions directly in our composition:

{% highlight haskell %}
(Right 10)
>>= \x -> if x<10 then (Left "less than 10") else (Right (x-10))
>>= \y -> if (y `mod` 2)==0 then (Right (y*y)) else (Left "odd result")
>>= \z -> (Right (z-5))
{% endhighlight %}

With a bit of formatting magic we get:

{% highlight haskell %}
(Right 10) >>= \x ->
    if x<10 then (Left "less than 10") else (Right (x-10)) >>= \y ->
       if (y `mod` 2)==0 then (Right (y*y)) else (Left "odd result") >>= \z ->
          (Right (z-5))
{% endhighlight %}

Literally nothing changed other than how the expression is indented.  Try it for yourself and see how it works.

The reason for the reformatting is to associate the input parameter for each function with the expression it is bound to by the function call.  `x` takes `Right 10`, `y` takes the first big `if` and `z` takes the second big `if`.  If we use `<-` as a kind of assignment operation and ditch the bind operation we can break up the functions to look like this:

{% highlight haskell %}
x <- (Right 10)
y <- if x<10 then (Left "less than 10") else (Right (x-10))
z <- if (y `mod` 2)==0 then (Right (y*y)) else (Left "odd result")
(Right (z-5))
{% endhighlight %}

Do you recognize that?  Maybe with a few more decorations:

{% highlight haskell %}
do 
  x <- (Right 10) ;
  y <- if x<10 then (Left "less than 10") else (Right (x-10)) ;
  z <- if (y `mod` 2)==0 then (Right (y*y)) else (Left "odd result")  ;
  (Right (z-5))
{% endhighlight %}

Bingo!  We have the `do` notation working backwards from the `>>=` notation.  Of course we usually go the other way, but this explains the "magic" of the `do`.  It's just syntax that will work with any monad.

One other thing to point out that is rather important.  Using the named functions has one restricting side effect that the do notation and the bind notation it is derived from do not.  Consider this:

{% highlight haskell %}
do 
  x <- (Right 10) ;
  y <- if x<10 then (Left "less than 10") else (Right (x-10)) ;
  z <- if (y `mod` 2)==0 then (Right (y*y)) else (Left "odd result")  ;
  (Right (z+x+y))
{% endhighlight %}

where `x` and `y` are used later than in the original expression.  If you use named functions this won't work because of the statically scoped nature of Haskell.  We'll discuss this later, but for now just try to rewrite the last `do` notation using the explicitly named functions and see what happens.

## Monadic eval

Time to come back from the land of the monad and talk evaluation and type prediction again.  Why go there in the first place?  Other than numeric and Boolean constants, all terms in `BAE` are evaluated by evaluating their subterms and combining the results to give a value for the term.  Unless of course evaluating any of the subterms results in an error.  If an error occurs it should be passed back to the user and return as the result. Hopefully the role of the `Either` will become clear after looking at  few terms.

The signature does not change, but we will call the evaluation function `evalM` to designate that this is a monadic version.

{% highlight haskell %}
evalM :: ABE -> Either String ABE
{% endhighlight %}

Constants evaluate as they always have.  Nothing can fail, so the constant values are simply returned.

{% highlight haskell %}
evalM (Num x) = (Right (Num x))
evalM (Boolean x) = (Right (Boolean x))
{% endhighlight %}

The evaluation of `Plus` terms gives a pattern for all other terms.  We will use the `do` notation to first evaluate both subterms, `t1` and `t2`, storing the results in `t1'` and `t2'`:

{% highlight haskell %}
evalM (Plus t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Num v1) -> case t2' of
                  (Num v2) -> (Right (Num (v1+v2)))
                  (Boolean _) -> (Left "Type Error in +")
    (Boolean _) -> (Left "Type Error in +")
{% endhighlight %}

The final term in the `do` sequence is a `case` expression that looks at the subterm evaluation results.  All we check in the case is the type of the term resulting from a successful evaluation.  If evaluating either subterm fails and results in a `Left` construction, it will fall through just like our example above.  What we do is make sure the subterm evaluation results are of compatible types.  If they are we return `(Num (v1+v2))`.  If they are not, we generate a fresh error message using `Left`.

Note how similar the `do` sequence is to the inference rule describing evaluation of addition.  This is a nice side effect of using the monad.

Remaining binary and unary operations use the same pattern as plus.  Specifically, evaluate the subterms and return the result of combining evaluation results.  Go through several of these to make sure you understand what the `do` notation is doing.  (No pun intended.)

{% highlight haskell %}
evalM (Minus t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Num v1) -> case t2' of
                  (Num v2) -> (Right (Num (v1-v2)))
                  (Boolean _) -> (Left "Type Error in -")
    (Boolean _) -> (Left "Type Error in -")
evalM (And t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Boolean v1) -> case t2' of
                  (Boolean v2) -> (Right (Boolean (v1 && v2)))
                  (Num _) -> (Left "Type Error in &&")
    (Num _) -> (Left "Type Error in &&")
evalM (Leq t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Num v1) -> case t2' of
                  (Num v2) -> (Right (Boolean (v1 <= v2)))
                  (Boolean _) -> (Left "Type Error in <=")
    (Boolean _) -> (Left "Type Error in +")
evalM (IsZero t) = do
  t' <- (evalM t)
  case t' of
    (Num v) -> (Right (Boolean (v == 0)))
    (Boolean _) -> (Left "Type Error in isZero")
{% endhighlight %}

Evaluating `If` is worth a bit of discussion, but isn't dramatically different than earlier expressions.  Only the conditional is evaluated using the `do` notation.  We do not evaluate the arms as only one should be evaluated based on the result of the conditional.  The `case` has two options, one that returns an error if the condition is a number and one that uses a Haskell `if` expression to choose the expression that should be returned.

{% highlight haskell %}
evalM (If t1 t2 t3) = do
  t1' <- (evalM t1)
  case t1' of
    (Num v) -> (Left "Type Error in if")
    (Boolean v) ->  if v then (evalM t2) else (evalM t3)
{% endhighlight %}

Finally we put `evalM` together with the original parser to get `interpM`.

{% highlight haskell %}
interpM = evalM . parseABE
{% endhighlight %}

## Monadic typeof

Programs are data structures.  We showed we can write a program that predicts failure by finding the types of expressions.  We called it `typeof`.  If we can find the type of an expression, then we don't need to handle type-related errors at run-time.  Nothing here should be surprising.  Same pattern as `evalM` - find the types of subterms and use those types to determine the term type or generate a fresh error.

{% highlight haskell %}
typeofM :: ABE -> Either String TABE
typeofM (Num x) = (Right TNum)
typeofM (Plus l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if (l'==TNum && r'==TNum) then (Right TNum) else (Left "Type Error in +")
typeofM (Minus l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if (l'==TNum && r'==TNum) then (Right TNum) else (Left "Type Error in -")
typeofM (Boolean b) = (Right TBool)
typeofM (And l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if (l'==TBool && r'==TBool) then (Right TBool) else (Left "Type Error in &&")
typeofM (Leq l r) = do
  l' <- typeof l ;
  r' <- typeof r ;
  case l' of
    TNum -> case r' of
              TNum -> (Right TBool)
              _ -> (Left "Type mismatch in <=")
typeofM (IsZero v) = do
  v' <- (typeof v) ;
  if v' == TNum then (Right TBool) else Left "Type mismatch in IsZero"
typeofM (If c t e) = do
  c' <- (typeof c) ;
  t' <- (typeof t) ;
  e' <- (typeof e) ;
{% endhighlight %}

Putting parsers and interpreters and type checkers together has always been an afterthought.  Let's take a look at it once again.  Again we'll take advantage of the `Either` monad.  Both the `typeofM` and `evalM` return `Either` monads of slightly different types.  But their error cases both use `Left` on strings.  Thus, if `typeofM t` fails and generates an error, bind passes the error through and does not call `evalM`.

{% highlight haskell %}
interpM t = do
  te <- Right parseBAE t	
  ty <- typeofM t
  evalM te
{% endhighlight %}

No such luck with parsers generated with Parsec.  They are monadic, but generate their own errors that are incompatible with errors generated using `Left`.  Bundling the parser result up with `Right` makes a successful parse compatible with `typeofM` and `evalM`.  The `do` notation strings things together nicely!

## Discussion

## Exercises

1. Rewrite `evalM` and `typeofM` to use the `Maybe` monad rather than the `Either` monad.  You will not be able to return error messages using `Maybe`.