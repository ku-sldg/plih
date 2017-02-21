---
layout: frontpage
title: Adding Environments
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

# Adding Environments

The substitution mechanism implemented in `evals` is a faithful implementation of our eval definition, but it is not optimized in any way. Let's try a far more efficient implementation of the `BAE` interpreter that waits to perform substitution until an identifier is evaluated rather that immediately substituting when an identifier is bound.  To understand the approach, consider a simple example from the previous chapter:

{% highlight text %}
bind x = 4 in
  bind y = 5 in
    x+y-4
{% endhighlight %}

Using substitution, we saw that when evaluating `bind x=4` all occurrences of `x` in scope were replaced immediately by `4`.  Instead of immediate substitution, let's simply remember that in the outermost scope, the value of `x` is 4:

{% highlight text %}
bind x = 4 in    [(x,4)]
  bind y = 5 in
    x+y-4
{% endhighlight %}

Evaluating the bind calculates the value of `x`, but does not immediately substitute.  It simply remembers that `x` is `4`.  The notation `[(x,4)]` depicts an *environment* containing the *binding* of `x` to `4`.  The decorations after each `in` show the environment in each `bind`'s scope.  Each `bind` adds a binding to the current environment.  As shown above, following the first `in`, the environment contains the binding of `4` to `x`.

Evaluation does the same thing for the inner bind, but this time it remembers that `y` is bound to `5` by adding it to the front of the list representing the enviornment:

{% highlight text %}
bind x = 4 in    [(x,4)]
  bind y = 5 in  [(y,5),(x,4)]
    x+y-4
{% endhighlight %}

After the second `in`, the environment contains bindings for both `y` and `x`.  As we will see later, order is important here.  New bindings are always added to the beginning of the environment.

Now we are ready to evaluate the body of the innermost `bind`.  The interpreter evaluates each identifier by simply looking it up in the current environment:

{% highlight text %}
x+y-4     [(y,5),(x,4)]
== 4+5-4
== 5
{% endhighlight %}

If constructed correctly, the environment should contain a binding for every bound instance.  Unlike `evals`, this new evaluator visits each element of the expression one time making evaluation far more efficient than constantly walking the code to perform substitution.

Summarizing, an *environment* is a data structure containing *bindings* generated from binding instances.  As of now, binding instances are only create by `bind` expressions, thus environments are altered only by executing `bind`.  A binding is simply an identifier/value pair.  A binding is added to the environment each time a `bind` expression is evaluated.

## Nesting Bind - Redux

Before charging into a new `eval` implementation, let's revisit examples from the previous chapter to get a feel for using environments.  First, let's look a the remaining two examples from the previous chapter.

The first example has 3 `bind` instances that add to the environment.  The first binds `y` to 4, thus the `y` immediately following will evaluate to 4.  The second binds `x` to 4. `y` will evaluate to `4` based on the binding from the outermost `bind` to give `x` its value.

{% highlight text %}
bind y = 4 in        [(y,4)]
  y + bind x = y in  [(x,4),(y,4)]
    bind x = x+2 in  [(x,6),(x,4),(y,4)]
      x+y-4
    + x
{% endhighlight %}

The third `bind` once again adds a binding for `x`, but this time to `x+2`.  The `x` in the value expression evaluates to `4` based on the environment at that point.  Evaluation of the third `bind` has not completed, thus the new value for `x` has not been added.  When we do reach the body of the innermost bind, the environment includes two bindings for `x` - one for the current scope and another for the previous scope.  In such cases the innermost instance is correct.  Searching for `x` from the beginning of the list gives exactly the result we want.  Including the first `y`, the resulting expression to be evaluated is `4+6+4-4+6 == 16`.  While the last `x` is on its own line, it is still in the scope created by the innermost bind.

Let's make a small change and close the innermost `bind` before evaluating the last `x`.  Everything proceeds as before except the final evaluation.  When the closing parenthesis ends the expression defined for the innermost `bind`, it places the last `x` in the scope created by the second `bind`.

{% highlight text %}
bind y = 4 in         [(y,4)]
  y + bind x = y in   [(x,4),(y,4)]
    (bind x = x+2 in  [(x,6),(x,4),(y,4)]
       x+y-4)         
    + x               [(x,4),(y,4)]
{% endhighlight %}

In the example, the environment from the second `bind` is repeated for the last `x` for emphasis.  It is not created by somehow removing the outermost binding for `x`, but is simply returned to when the expression `x+y-4` finishes evaluating.  Thus the result is  14 rather than 16.

When a free variable is encountered, it will have no binding in the current environment.
{% highlight haskell %}
bind x=5 in  [(x,5)]
  y+x
{% endhighlight %}

In this example the environment created by `bind` creates a binding for `x`, but not for `y`.  When `y` is evaluated, an undefined identifier error results.

Another experiment tries to use `x` recursively in its own definition:

{% highlight haskell %}
bind x = x+1 in x
{% endhighlight %}

This definition seems odd from the outset, but try and use an environment to evaluate.  The environment resulting from the `bind` should contain a value for `x`, but what does the environment contain *before* `bind`?  That is the environment used to find the value of `x` in `x+1`.  As one might guess, the environment is empty and contains no bindings.  Thus, when `x` is evaluated in `x+1`, an undefined identifier error results.

## Defining an Environment

As noted earlier, an environment is simply a list of identifier/value pairs.  The simplest way to represent this in Haskell uses a list of `String`/`Int` pairs.  The `Env` type provides a shorthand for this type:

{% highlight haskell %}
type Env = [(String,Int)]
{% endhighlight %}

`eval` becomes a function from an environment and expression to an integer.

{% highlight haskell %}
eval :: Env -> BAE -> Int
{% endhighlight %}

The cases for numbers, addition and subtraction are the same as for `evals` and earlier evaluators, except recursive calles to `eval` must include the environment, `env`:

{% highlight haskell %}
eval env (Num x) = x
eval env (Plus l r) = (eval env l) + (eval env r)
eval env (Minus l r) = (eval env l) - (eval env r)
{% endhighlight %}

Cases for `Bind` and `Id` manage and use the environment respectively:

{% highlight haskell %}
eval env (Bind i v b) =
  let v' = eval env v in
    eval ((i,v'):env) b
eval env (Id id) = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
{% endhighlight %}

The `Bind` case evaluates the value expression associated with the new identifier using the current environment.  The expression `((i,v'):env)` creates a new pair and adds it to the front of the original environment using cons.

The `Id` case uses the builtin Haskell `lookup` function to search the environment for the first pair whose first element is equal to `id`, the name of the identifier.  `lookup` returns a `Maybe` instance where `Just a` is a value if if found and `Nothing` indicates and error.[^1]  The `case` statement implements a check for `Just a` or `Nothing` and returns `a` or throws and error respectively.

Finally, the interpretation function is defined by composing `eval []` and the parser.  Note that the parser, pretty printer, and term generator from the immediate substitution example are completely unchanged.

{% highlight haskell %}
interp = (eval []) . parseBAE
{% endhighlight %}

Remember that `eval []` results in a function with a single parameter of type `BAE`.  In effect, an instance of `eval` with an empty initial environment.  Still, it is a function and can be treated like any other function.

## Testing

{% highlight haskell %}
testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval [] t))
{% endhighlight %}

Remember that we called the immediate substituting interpreter a *normative implementation* that implements a correct interpreter that is not optimized.  If the new interpreter properly implements the `BAE` language, then the two interpreters should always produce the same value for valid input.  QuickCheck can easily check this property by calling both interpreters on arbitrary `BAE` expressions and comparing results:

{% highlight haskell %}
testCompare :: Int -> IO ()
testCompare n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (eval [] t) == (evals t))
{% endhighlight %}

Trying this on large sets of test cases results in no errors, giving us strong evidence that the new interpreter implements `BAE` in the same way as the original interpreter.

To make testing more meaningful, `evals` and `eval` should be implemented with an `Either` return type like `ABE` earlier.  This is left as an exercise and will be explored in a later chapter.

## Discussion

Unlike most of our earlier interpreters, the objective of adding an environment is not to add a new language feature or change what `eval` does.  The objective is to make evaluation more efficient by caching substitutions in the environment data structure.  In fact, we hope that adding and environment does not change the evaluation result at all.

`bind` is a different kind of expression than `+` or `-`.  Arithmetic expressions calculate values and return them.  `2+1` evaluates to `3` and the new value is returned to whatever mechanism involved `eval`.  `bind` on the other hand doesn't calculate anything.  Its body does, but the `bind` itself is wrapped around the body.  The real work of the `bind` is performed on the environment.  Arithmetic expressions calculate values and in a sense, `bind` calculates a new environment by creating identifiers.  It's not necessary to make this distinction, but grouping expressions by their purpose can prove useful in both understanding and implementing interpreters.

## Definitions

* Environment - A list of bindings
* Binding - An identifier/value pair added to an environment when evaluating a binding instance.
* Lifting - Moving a term from the host language to the implemented language
* Lowering - The opposite of lifting

## Exercises

1. Write a version of `eval` called `evalErr` that uses the `Either` construct to return either a `BAE` construct or an error message.
2. Write a version of `interp` that uses a *prelude*.  The prelude is an initial environment with a collection of identifiers that are always available without explicit `bind` definitions.

## Source

Download [source]({{site.baseurl}}/haskell/bae.hs) for all interpreter code from this chapter.

## Notes

[^1]: You should be familiar with the Maybe type (sometimes called option).  If not, there are a number of Haskell tutorials that cover it quite well, such as XXXX
