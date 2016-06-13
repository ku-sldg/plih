---
layout: blog
categories: blog
title: Project 1 Test Cases
---
Here are a few test cases for the interpreters for Project 1.  First for WAE:

{% highlight racket %}
{% raw %}
(test (eval-wae '1) 1)
(test (eval-wae '{+ 1 1}) 2)
(test (eval-wae '{- 1 1}) 0)
(test (eval-wae '{with {x 3} {+ x x}}) 6)
(test (eval-wae '{with {x 3} {with {y 4} {+ x y}}}) 7)
(test (eval-wae '{with {x 3} {with {y 4} {+ x y}}}) 7)
(test (eval-wae '{with {x 3} {with {y {+ x x}} {+ x y}}}) 9)
(test (eval-wae '{with {x 3} {with {y {+ x x}} {with {x 1} {+ x y}}}}) 7)
(test (eval-wae '{with {x 1} {with {y 2} {with {z x} {with {x x} {with {z {+ z 1}} {+ z y}}}}}}) 4)
{% endraw %}
{% endhighlight %}

Now for WAEE:

{% highlight racket %}
{% raw %}
  (test (eval-waee '1) 1)
  (test (eval-waee '{+ 1 1}) 2)
  (test (eval-waee '{- 1 1}) 0)
  (test (eval-waee '{* 2 2}) 4)
  (test (eval-waee '{/ 4 2}) 2)
  (test (eval-waee '{with {{x 3}} {+ x x}}) 6)
  (test (eval-waee '{with {{x 3} {y 4}} {+ x y}}) 7)
  (test (eval-waee '{with {{x 3} {y 4}} {+ x y}}) 7)
  (test (eval-waee '{with {{x 3}} {with {{y 4}} {+ x y}}}) 7)
  (test (eval-waee '{with {{x 3}} {with {{y {+ x x}}} {+ x y}}}) 9)
  (test (eval-waee '{with {{x 3}} {with {{y {+ x x}}} {with {{x 1}} {+ x y}}}}) 7)
  (test (eval-waee '{with {{x 1} {y 2}} {with {{z x} {x x}} {with {{z {+ z 1}}} {+ z y}}}}) 4))
{% endraw %}
{% endhighlight %}

I believe all test cases should execute successfully.  If you put them at the bottom of your implementation files they will execute each time you load your projects.  Also, the `test` function executes its first argument and compares the result to the second argument.  Racket will let you know if they don't match.
