---
layout: frontpage
title: Programs Are Data Structures
use_math: true
categories: chapter ch4
---
# Programs Are Data Structures


{% highlight haskell %}
optimize :: BBAE -> BBAE
optimize (Num x) = (Num x)
optimize (Plus (Num 0) r) = optimize r
optimize (Plus l (Num 0)) = optimize l
optimize (Plus l r) = let l' = (optimize l)
                          r' = (optimize r)
                      in (Plus l' r')
optimize (Minus l (Num 0)) = optimize l
optimize (Minus l r) = let l' = (optimize l)
                           r' = (optimize r)
                       in (Minus l' r')
optimize (Bind i v b) = let v' = optimize v in
                          optimize b
optimize (Id id) = (Id id)
optimize (Boolean b) = (Boolean b)
optimize (And l r) = let (Boolean l') = (optimize l)
                         (Boolean r') = (optimize r)
                      in (Boolean (l' && r'))
optimize (Leq l r) = let (Num l') = (optimize l)
                         (Num r') = (optimize r)
                      in (Boolean (l' <= r'))
optimize (IsZero v) = let (Num v') = (optimize v)
                      in (Boolean (v' == 0))
optimize (If c t e) = let (Boolean c') = (optimize c)
                      in if c' then (optimize t) else (optimize e)
{% endhighlight %}
