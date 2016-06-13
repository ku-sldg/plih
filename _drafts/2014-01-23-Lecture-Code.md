---
layout: blog
category: blog
title: Lecture Code January 23
---
Here's a bit of code from class on January 23rd showing a parser,
interpreter, and the abstract syntax they operate over.  We didn't
cover the parser in class, but will in the near future.   These are good
usage examples of `define-type` and `type-case`.

{% highlight scheme %}
(define-type AE
  (num (n number?))
  (add (lhs AE?) (rhs AE?))
  (sub (lhs AE?) (rhs AE?)))

(define parse
  (lambda (x)
    (cond ((number? x) (num x))
          ((list? x)
           (cond ((equal? (car x) '+) (add (parse (cadr x)) (parse (caddr x))))
                 ((equal? (car x) '-) (sub (parse (cadr x)) (parse (caddr x)))))))))

(define interp
  (lambda (expr)
    (type-case AE expr
       (num (n) n)
       (add (l r) (+ (interp l) (interp r)))
       (sub (l r) (- (interp 1) (interp r))))))
{% endhighlight %}
