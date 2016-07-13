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

Before moving ahead too functions, let's spend a bit of time looking at type checking when adding Booleans to BAE.  Let's define a new language BAE with Booleans (`BBAE`) that simply adds the same boolean operators added to `AE` to get `ABE`.  Let's quickly walk through our standard methodology for extending a language and add at the same time add type checking.

## Concrete Syntax and Values

Concrete syntax for `BBAE` is literally the concrete syntax of `ABE` and `BAE` composed:

$$\begin{align*}
t ::= & \NUM \mid t + t \mid t - t \\
      & \mid \bbind \ID=t\; \iin t \\
      & \mid \ttrue \mid \ffalse \mid \iif t \tthen t \eelse t \\
      & \mid t \lleq t \mid t \aand t \mid \iisZero t \\
\end{align*}$$

There are no new values:

$$\begin{align*}
v := \NUM \mid \ttrue \mid \ffalse \\
\end{align*}$$

## Abstract Syntax, Parser, Pretty Printer, and Generator Definition

Abstract syntax follows immediately from concrete syntax.  Copy-and-paste the elements of `ABE` and `BAE` into a single AST:

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

## Eval Definition

Now for implementation details.  Here things are not quite as simple as composing things from previous interpreter.  Previously we've implemented interpreters that return:

1. Haskell values
2. AST values
3. Either error messages or AST values

The plan is to write a type checker, so AST values are sufficient for this `eval` implementation.  Haskell values would be okay, but we want to use our earlier technique for implementing error messages.  The `Either` implementation was specifically for catching errors at runtime and that is not required for a type checking interpreter.

The `Env` type remains unchanged from previous implementations and cases for the AST elements are taken verbatim from the `ABE` and `BAE` interpreters.

{% highlight haskell %}
type Env = [(String,BBAE)]
    
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
                     Nothing -> error "Varible not found"
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

Note that `eval` uses the Haskell `error` function rather than returning value.  Can you argue that type checking will prevent the error condition from occurring?

The interpretation function is identical to that for `BAE`:

{% highlight haskell %}
interp = (eval []) . parseBBAE
{% endhighlight %}

## Testing Eval

By defining `interp` in the same manner as `BAE`, the test function for `eval` literally does not change:

{% highlight haskell %}
testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval [] t))
{% endhighlight %}

With everything in place, we evaluate over just a few cases and quickly learn that `eval` without any dynamic error checking or static type checking fails quite quickly.  Thus, the next step is writing a type checker to statically determine if a `BBAE` expression will execute.

## Type Rules

Type rules for `BAE` and `ABE` expressions remain unchanged:

$$\frac{}{\typeof \NUM = \tnum}\; [NumT]$$

$$\frac{}{\typeof \ttrue = \tbool}\; [TrueT]$$

$$\frac{}{\typeof \ffalse = \tbool}\; [FalseT]$$

$$\frac{\typeof t_1 = \tnum,\; \eval t_2 = \tnum}{\typeof t_1 + t_2 = TNum}\; [PlusT]$$

$$\frac{\typeof t_1 = \tnum,\; \eval t_2 = \tnum}{\typeof t_1 + t_2 = \tnum}\; [MinusT]$$

$$\frac{\typeof t_1 = \tbool,\; \typeof t_2 = \tbool}{\typeof t_1 \aand t_2 = \tbool}\; [AndT]$$

$$\frac{\typeof t_1 = \tnum,\; \typeof t_2 = \tnum}{\eval t_1 \lleq t_2 = \tbool}\; [LeqT]$$

$$\frac{\typeof t = \tnum}{\typeof \iisZero t = \tbool}\; [isZeroT]$$

$$\frac{\typeof t_0 = \tbool,\;\typeof t_1 = T,\;\typeof t_2 = T
}{\typeof \iif t_0 \tthen t_1 \eelse t_2 = T}\;[IfT]$$

Two new type rules are added for identifiers and `bind`.

$$\frac{(i,T)\in \Gamma}{\typeof i = T}\;[IdT]$$

$$\frac{\typeof v=T}{\typeof \bbind i=v \iin b = \typeof (i,T):\Gamma b}\;[BindT]$$

## Typeof Definition

{% highlight haskell %}
data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)
{% endhighlight %}



## Testing

## Discussion

## Exercises

1. Write the function `evalErr :: Env -> BBAE -> Either String BBAE` for for `BBAE` that returns either a string error message or a value following interpretation.
2. Compare the `evalErr` function with the `evalType` function using the same techniques used for `ABE`