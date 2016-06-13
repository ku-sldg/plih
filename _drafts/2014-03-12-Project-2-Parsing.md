---
layout: blog
category: blog
title: Project 2 parsing
---
I promised no more parsers.  Well, I lied.  Sort of.  Project 2 does
call for a parser, but in a moment of weakness I decided to give you
code for the data types and parsers for both languages.  Here they are.

For CFAE constructs:

{% highlight scheme %}
;;; Define an AST type for CFAE constructs.  Standard expression elements plus functions and if0.
(define-type CFAE
  (num (n number?))
  (add (lhs CFAE?) (rhs CFAE?))
  (sub (lhs CFAE?) (rhs CFAE?))
  (mul (lhs CFAE?) (rhs CFAE?))
  (div (lhs CFAE?) (rhs CFAE?))
  (id (name symbol?))
  (if0 (cond CFAE?) (tarm CFAE?) (farm CFAE?))
  (fun (arg-name symbol?) (body CFAE?))
  (app (fun-expr CFAE?)(arg CFAE?)))

;;; Define a parser for CFAE constructs.  This parser does no error checking at all. Simply converts
;;; concrete syntax to AST.
(define parse-cfae
  (lambda (expr)
    (cond ((symbol? expr) (id expr))
          ((number? expr) (num expr))
          ((list? expr)
           (case (car expr)
             ((-) (sub (parse-cfae (cadr expr)) (parse-cfae (caddr expr))))
             ((+) (add (parse-cfae (cadr expr)) (parse-cfae (caddr expr))))
             ((*) (mul (parse-cfae (cadr expr)) (parse-cfae (caddr expr))))
             ((/) (div (parse-cfae (cadr expr)) (parse-cfae (caddr expr))))
             ((if0) (if0 (parse-cfae (cadr expr)) (parse-cfae (caddr expr))
                         (parse-cfae (cadddr expr))))
             ((fun) (fun (cadr expr) (parse-cfae (caddr expr))))
             (else (app (parse-cfae (car expr)) (parse-cfae (cadr expr))))))
          (else 'parse-cfae "Unexpected token"))))

{% endhighlight %}

And for CFWAE constructs:[^1]

{% highlight scheme %}
;;; Define an AST type for CFWAE constructs
(define-type CFWAE
  (numw (n number?))
  (addw (lhs CFWAE?) (rhs CFWAE?))
  (subw (lhs CFWAE?) (rhs CFWAE?))
  (mulw (lhs CFWAE?) (rhs CFWAE?))
  (divw (lhs CFWAE?) (rhs CFWAE?))
  (withw (name symbol?) (named-expr CFWAE?) (body CFWAE?))
  (condw (arms list?) (default CFWAE?))
  (idw (name symbol?))
  (if0w (cond CFWAE?) (tarm CFWAE?) (farm CFWAE?))
  (funw (arg-name symbol?) (body CFWAE?))
  (appw (fun-expr CFWAE?)(arg CFWAE?)))

;;; Define a parser for CFWAE constructs
(define parse-cfwae
  (lambda (expr)
    (cond ((symbol? expr) (idw expr))
          ((number? expr) (numw expr))
          ((list? expr)
           (case (car expr)
             ((-) (subw (parse-cfwae (cadr expr)) (parse-cfwae (caddr expr))))
             ((+) (addw (parse-cfwae (cadr expr)) (parse-cfwae (caddr expr))))
             ((*) (mulw (parse-cfwae (cadr expr)) (parse-cfwae (caddr expr))))
             ((/) (divw (parse-cfwae (cadr expr)) (parse-cfwae (caddr expr))))
             ((if0) (if0w (parse-cfwae (cadr expr)) (parse-cfwae (caddr expr)) 
                          (parse-cfwae (cadddr expr))))
             ((fun) (funw (cadr expr) (parse-cfwae (caddr expr))))
             ((with) (withw (car (cadr expr)) 
                            (parse-cfwae (cadr (cadr expr))) 
                            (parse-cfwae (caddr expr))))
             ((cond0) (condw (parse-arms (cdr (drop-right expr 1))) (parse-cfwae (last expr))))
             (else (appw (parse-cfwae (car expr)) (parse-cfwae (cadr expr))))))
          (else 'parse-cfwae "Unexpected token"))))

;;; Utility function to parse the arms of a cond0
(define parse-arms
  (lambda (arms)
    (cond ((empty? arms) '())
          (else (cons (list (parse-cfwae (car (car arms))) (parse-cfwae (cadr (car arms))))
                       (parse-arms (cdr arms)))))))
{% endhighlight %}

[^1]:Note that I append an `w` to the constructor name rather than a `1` as we did in class. Just a naming convention and nothing more.
