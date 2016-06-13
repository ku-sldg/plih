#lang plai
;;; EECS 662 - Miniproject 3 AST and Parser
;;;
;;; Author: Perry Alexander
;;; Date: 4/22/16
;;;
;;; Description:
;;; CFWAER - The abstract data type for representing CFWAER ASTs
;;; parse-cfwaer - Parser translating concrete CFWAER syntax into CFWAER AST
;;;
;;; At the bottom of the file are a collection of test cases for the parser.
;;; parse-cfwaer is called on test cases and output printed.
;;;

;;; Define an AST type for CFWAER constructs.
(define-type CFWAER
  (num (n number?))
  (add (lhs CFWAER?) (rhs CFWAER?))
  (sub (lhs CFWAER?) (rhs CFWAER?))
  (mul (lhs CFWAER?) (rhs CFWAER?))
  (div (lhs CFWAER?) (rhs CFWAER?))
  (id (name symbol?))
  (with (name symbol?) (named-expr CFWAER?) (body CFWAER?))
  (rec (name symbol?) (named-expr CFWAER?) (body CFWAER?))
  (if0 (cond CFWAER?) (tarm CFWAER?) (farm CFWAER?))
  (fun (arg-name symbol?) (body CFWAER?))
  (app (fun-expr CFWAER?)(arg CFWAER?)))

;;; Define a parser for CFWAER constructs.  This parser does no error checking at all. Simply converts
;;; concrete syntax to AST.
(define parse-cfwaer
  (lambda (expr)
    (cond ((symbol? expr) (id expr))
          ((number? expr) (num expr))
          ((list? expr)
           (case (car expr)
             ((-) (sub (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((+) (add (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((*) (mul (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((/) (div (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((with) (with (car (cadr expr)) 
                            (parse-cfwaer (cadr (cadr expr))) 
                            (parse-cfwaer (caddr expr))))
             ((rec) (rec (car (cadr expr)) 
                            (parse-cfwaer (cadr (cadr expr))) 
                            (parse-cfwaer (caddr expr))))
             ((if0) (if0 (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))
                         (parse-cfwaer (cadddr expr))))
             ((fun) (fun (cadr expr) (parse-cfwaer (caddr expr))))
             (else (app (parse-cfwaer (car expr)) (parse-cfwaer (cadr expr))))))
          (else 'parse-cfwaer "Unexpected token"))))

(begin (print (parse-cfwaer '{{fun x {+ 1 3}} 1}))
       (print (parse-cfwaer '{with {y 1} {{fun x {+ y x}} 3}}))
       (print (parse-cfwaer '{with {y 1} {with {f {fun x {+ y x}}} {f 3}}}))
       (print (parse-cfwaer '{with {y 1} {with {f {fun x {+ y x}}} {with {y 100} {f 3}}}}))
       (print (parse-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 0}})
             )
       (print (parse-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 3}})
             )
       (print (parse-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 5}})
             )
       (print (parse-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 1} 1}})
             )
       (print (parse-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 2} 2}})
             )
       (print (parse-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 3} 3}})
             )
       (print (parse-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 0} 3}})
             )
       (print (parse-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 3} 0}})
             )
       (print (parse-cfwaer '{{fun x {+ 1 3}} 1}))
       (print (parse-cfwaer '{rec {y 1} {{fun x {+ y x}} 3}}))
       (print (parse-cfwaer '{rec {y 1} {with {f {fun x {+ y x}}} {f 3}}}))
       (print (parse-cfwaer '{with {y 1} {rec {f {fun x {+ y x}}} {with {y 100} {f 3}}}}))
       (print (parse-cfwaer '{rec {y 1} {rec {f {fun x {+ y x}}} {rec {y 100} {f 3}}}})))