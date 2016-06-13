---
layout: blog
category: blog
title: Project 2 Test Cases
---
Here are some test cases for Project 2.  Note that any test case using a real value may generate different results based on how many signficant figures you defined `pi` to have.  I will take this into account when I grade, but be aware of it.

Test cases for CFAE:

{% highlight scheme %}
{% raw %}
(test (eval-cfae '{+ 1 2}) (num 3))
(test (eval-cfae '{+ 2 {* 2 3}}) (num 8))
(test (eval-cfae '{{fun x x} 3}) (num 3))
(test (eval-cfae '{{fun x {+ x 1} } 1}) (num 2))
(test (eval-cfae '{if0 0 1 2}) (num 1))
(test (eval-cfae '{if0 {{fun x {- x 2}} 3} {{fun x {* 2 x}} 10} {{fun x {/ x 2}} 8}}) (num 4))
(test (eval-cfae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0}) (num 1))
(test (eval-cfae '{{fun x {{fun y {+ x y}} 3}} 1}) (num 4))
{% endraw %}
{% endhighlight %}

Test cases for CFWAE:

{% highlight scheme %}
{% raw %}
(test (eval-cfwae '{+ 1 2}) (num 3))
(test (eval-cfwae '{+ 2 {* 2 3}}) (num 8))
(test (eval-cfwae '{{fun x x} 3}) (num 3))
(test (eval-cfwae '{{fun x {+ x 1}} 1}) (num 2))
(test (eval-cfwae '{if0 0 1 2}) (num 1))
(test (eval-cfwae '{if0 {{fun x {- x 2}} 3} {{fun x {* 2 x}} 10} {{fun x {/ x 2}} 8}}) (num 4))
(test (eval-cfwae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0}) (num 1))
(test (eval-cfwae '{with {x 10} {+ x 5}}) (num 15))
(test (eval-cfwae '{with {f {fun x {+ x 1}}} {f 2}}) (num 3))
(test (eval-cfwae '{cond0 {1 2} {0 15} 0}) (num 15))
(test (eval-cfwae '{with {add1 {fun x {+ x 1}}} {cond0 {{add1 0} 5} {3 4} {0 {add1 2}} 52} 2}) (num 3))
(test (eval-cfwae '{inc pi}) (num 4.141592653589793))
(test (eval-cfwae '{with {x 2} {with {inc {fun x {+ x 2}}} {inc x}}}) (num 4))
(test (eval-cfwae '{area 2}) (num 12.566370614359172))
(test (eval-cfwae '{{fun x {{fun y {+ x y}} 3}} 1}) (num 4))
(test (eval-cfwae '{with {g {fun f {f 3}}} {g {fun x {+ x 1}}}}) (num 4))
{% endraw %}
{% endhighlight %}
