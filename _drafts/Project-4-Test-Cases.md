---
layout: blog
category: blog
title: Project 4 Test Cases
---
Following are a few test cases for project 4.  I've run these through
my solution for Project 4, so I'm confident they are correct.

Note that in the
project description I used `seq` rather than `seqn` as the concrete
syntax for sequence.  The test cases use `seq` - if you used `seqn` it
is a trivial fix.

{% highlight scheme %}
{% raw %}
(test (eval-cfwaes '{with {y 0}
                       {with {inc {fun x {+ x 1}}}
                         {seq {seq {assign y {inc y}}
                                   {assign y {inc y}}}
                              {seq {assign y {inc y}}
                                   {assign y {inc y}}}}}}) (numV 4))

(test (eval-cfwaes '{with {y 1}
                       {with {inc {fun x {+ x y}}}
                         {inc 3}}}) (numV 4))

(test (eval-cfwaes '{with {y 1}
                       {with {inc {fun x {+ x y}}}
                         {seq {assign y 2} {inc 3}}}}) (numV 5))

(test (eval-cfwaes '{with {y 1}
                       {with {inc {seq {assign y 2} {fun x {+ x y}}}}
                         {inc 3}}}) (numV 5))

(test (eval-cfwaes '{with {x 3}
                       {seq x {assign x {+ x 1}}}}) (numV 4))

(test (eval-cfwaes '{with {x 3}
                       {seq
                         {assign x {+ x 1}} {assign x {+ x 1}}}}) (numV 5))

(test (eval-cfwaes '{with {x 3}
                       {seq
                        {seq
                         {assign x {+ x 1}} {assign x {+ x 1}}}
                        {assign x {+ x 1}}}}) (numV 6))
{% endraw %}
{% endhighlight %}
