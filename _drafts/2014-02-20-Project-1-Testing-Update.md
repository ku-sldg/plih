---
layout: blog
category: blog
title: Project 1 Testing - Updated
---
My original testing post had operators that are not legal in the
current version of WAE.  Sorry about that.  I've removed the offending
test cases.

Also, I will use more test cases that the ones provided below.  I'll
use the same function, just more test cases.  I will *not* test for
syntax errors.

I'm going to test your code with the following function:

{% highlight scheme %}
(define test-wae
  (lambda (f)
    (begin
      (test (f '1) 1)
      (test (f '{+ 1 1}) 2)
      (test (f '{- 1 1}) 0)
      (test (f '{with {x 3} {+ x x}}) 6)
      (test (f '{with {x 3} {with {y 4} {+ x y}}}) 7)
      (test (f '{with {x 3} {with {y {+ x x}} {+ x y}}}) 9)
      (test (f '{with {x 3} {with {y {+ x x}} {with {x 1} {+ x y}}}}) 7))))
{% endhighlight %}

`test` is a function that compares the result of an execution with an
expected result.  It executes its first argument, compares the
result with its second and idicates if they match.  Nothing magic, but
pretty useful.

I will load your code and then load `test-wae` then execuate the
following command:

{% highlight scheme %}
(test-wae eval-wae)
{% endhighlight %}

where `eval-wae` is the name of your evaluation function.  You might
want to test your code with my testing function before submitting to
make sure it tests your function properly.  I have a similar function
for `eval-waee`, but I'll let you cook that one up yourself.

