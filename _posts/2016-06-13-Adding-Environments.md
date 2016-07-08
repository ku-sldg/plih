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

## Nesting Bind - Redux

{% highlight text %}
[]            bind x = 4 in
[(x,4)]         bind y = 5 in
[(y,5),(x,4)]     x+y-4
{% endhighlight %}

{% highlight text %}
[]           bind x = 4 in
[(x,4)]        bind y = 5+x in
[(y,9),(x,4)]    x+y-4
{% endhighlight %}

{% highlight text %}
[]                  bind y = 4 in
[(y,4)]               y + bind x = y in
[(x,4),(y,4)]           bind x = x+2 in
[(x,6),(x,4),(y,4)]       x+y-4
                        + x
{% endhighlight %}

{% highlight text %}
[]                  bind y = 4 in
[(y,4)]               y + bind x = y in
[(x,4),(y,4)]           (bind x = x+2 in
[(x,6),(x,4),(y,4)]       x+y-4)
[(x,4),(y,4)]           + x
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
testCompare :: Int -> IO ()
testCompare n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (eval [] t) == (evals t))
{% endhighlight %}

## Definitions

* Environment - A list of identifier bindings

## Exercises

## Source

Download [source]({{site.baseurl}}/haskell/bae.hs) for all interpreter code from this chapter.

## Notes

