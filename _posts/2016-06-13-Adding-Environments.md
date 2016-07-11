---
layout: frontpage
title: Adding Environments
use_math: true
category: chapter
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

Let's try a far more efficient implementation of the `BAE` interpreter that waits to perform substitution until an identifier is evaluated.  The substitution mechanism implemented in `evals` is a faithful implementation of our eval definition, but it is not optimized in any way.

An *environment* holds *bindings* generated from binding instances.  A binding is simply an identifier, value pair.  A binding is added to the environment each time a `bind` expression is evaluated.

## Nesting Bind - Redux

{% highlight text %}
bind x = 4 in    []
  bind y = 5 in  [(x,4)]
    x+y-4        [(y,5),(x,4)]
{% endhighlight %}

{% highlight text %}
bind x = 4 in      []
  bind y = 5+x in  [(x,4)]
    x+y-4          [(y,9),(x,4)]
{% endhighlight %}

{% highlight text %}
bind y = 4 in        []
  y + bind x = y in  [(y,4)]
    bind x = x+2 in  [(x,4),(y,4)]
      x+y-4          [(x,6),(x,4),(y,4)]
    + x
{% endhighlight %}

{% highlight text %}
bind y = 4 in         []
  y + bind x = y in   [(y,4)]
    (bind x = x+2 in  [(x,4),(y,4)]
       x+y-4)         [(x,6),(x,4),(y,4)]
    + x               [(x,4),(y,4)]
{% endhighlight %}

## Using an Environment

{% highlight haskell %}
type Env = [(String,Int)]
{% endhighlight %}

{% highlight haskell %}
eval :: Env -> BAE -> Int
eval env (Num x) = x
eval env (Plus l r) = (eval env l) + (eval env r)
eval env (Minus l r) = (eval env l) - (eval env r)
eval env (Bind i v b) =
  let v' = eval env v in
    eval ((i,v'):env) b
eval env (Id id) = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
{% endhighlight %}
                                            
{% highlight haskell %}
interp = (eval []) . parseBAE
{% endhighlight %}

## Testing

{% highlight haskell %}
testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval [] t))
{% endhighlight %}

{% highlight haskell %}
testCompare :: Int -> IO ()
testCompare n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (eval [] t) == (evals t))
{% endhighlight %}

## Definitions

* Environment - A list of bindings
* Binding - An identifier/value pair added to an environment when evaluating a binding instance.

## Exercises

1. Write a version of `eval` called `evalErr` that uses the `Either` construct to return either a `BAE` construct or an error message. 

## Source

Download [source]({{site.baseurl}}/haskell/bae.hs) for all interpreter code from this chapter.

## Notes

