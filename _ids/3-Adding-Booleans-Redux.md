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

Before moving ahead too functions, let's spend a bit of time looking at type checking when adding Booleans to BAE.  Let's define a new language BAE with Booleans (`BBAE`) that simply adds the same boolean operators added to `AE` to get `ABE`.  Let's quickly walk through our standard methodology for extending a language

## Concrete Syntax and Values

$$\begin{align*}
t ::= & \NUM \mid t + t \mid t - t \\
      & \mid \bbind \ID=t\; \iin t \\
      & \mid \ttrue \mid \ffalse \mid \iif t \tthen t \eelse t \\
      & \mid t \lleq t \mid t \aand t \mid \iisZero t \\
\end{align*}$$

$$\begin{align*}
v := \NUM \mid \ttrue \mid \ffalse \\
\end{align*}$$

## Abstract Syntax and Pretty Printer

{% highlight haskell %}
data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)
{% endhighlight %}

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

{% highlight haskell %}
pprint :: BBAE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ " + " ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ " - " ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ " && " ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ " <= " ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero " ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"
pprint (Id s) = s
pprint (Bind n v b) = "(bind " ++ n ++ " = " ++ pprint v ++ " in " ++ pprint b ++ ")"
{% endhighlight %}

## Evaluation Rules

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

## Type Rules

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

$$\frac{(i,T)\in \Gamma}{\typeof i = T}\[IdT\]$$

$$\frac{\typeof v=T}{\typeof let i=v in b = \typeof (i,T):\Gamma b}$$

## Typeof Definition

## Eval Definition

## Generator Definition

## Testing

## Discussion

## Exercises

1. Write the function `evalErr :: Env -> BBAE -> Either String BBAE` for for `BBAE` that returns either a string error message or a value following interpretation.
2. Compare the `evalErr` function with the `evalType` function using the same techniques used for `ABE`