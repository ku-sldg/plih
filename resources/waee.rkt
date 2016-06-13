#lang plai
;;;
;;;-----
;;;
;;; Project 1 Solution
;;;
;;; Author: Perry Alexander
;;; Date: Spring 2016
;;;
;;; A parser, interpreter and evaluator for WAEE as defined in Project 1.
;;;


;;; Code for binary operation lookup

(define-type Binop-rec
  (binop-rec (name symbol?) (op procedure?)))

(define lookup
  (lambda (op-name op-table)
    (cond ((empty? op-table) (error 'lookup "Operator not found"))
          (else (if (symbol=? (binop-rec-name (car op-table)) op-name)
                    (binop-rec-op (car op-table))
                    (lookup op-name (cdr op-table)))))))

;;; WAEE AST type

(define-type WAEE
  (num (n number?))
  (binop (op symbol?) (lhs WAEE?) (rhs WAEE?))
  (with (bindings list?) (exp WAEE?))
  (id (name symbol?)))

;;; Parsing function

(define parse-waee
  (lambda (expr)
    (cond ((symbol? expr) (id expr))
          ((number? expr) (num expr))
          ((list? expr)
           (case (car expr)
             ((-) (binop 'minus (parse-waee (cadr expr)) (parse-waee (caddr expr))))
             ((+) (binop 'plus (parse-waee (cadr expr)) (parse-waee (caddr expr))))
             ((*) (binop 'times (parse-waee (cadr expr)) (parse-waee (caddr expr))))
             ((/) (binop 'div (parse-waee (cadr expr)) (parse-waee (caddr expr))))
             ((with) (with (map parse-binding (cadr expr)) 
                           (parse-waee (caddr expr))))))
          (else 'parse-waee "Unexpected token"))))

;;; Parse binding for mapping onto a list of with bindings

(define parse-binding
  (lambda (binding)
    (cond ((list? binding)
           (if (symbol? (car binding))
               (list (car binding) (parse-waee (cadr binding)))
               (error 'parse-binding "illegal binding")))
          (else (error 'parse-binding "illegal binding")))))

;;; Binding substitution operation for use with map

(define subst-binding
  (lambda (sub-id val)
    (lambda (binding)
      (list (car binding) (subst (cadr binding) sub-id val)))))

;;; Substitution operation

(define subst
  (lambda (expr sub-id val)
    (type-case WAEE expr
       (num (n) expr)
       (binop (o l r) (binop o (subst l sub-id val) (subst r sub-id val)))
       (with (bindings bound-body)
             (if (member sub-id (map car bindings))
                 (with (map (subst-binding sub-id val) bindings) bound-body)
                 (with (map (subst-binding sub-id val) bindings)
                       (subst bound-body sub-id val))))
       (id (v) (if (symbol=? v sub-id) val expr)))))

;;; interp-waee
(define interp-waee
  (lambda (expr)
    (type-case WAEE expr
       (num (n) n)
       (binop (o l r) ((lookup o op-list) (interp-waee l) (interp-waee r)))
       (with (b e) (interp-waee (foldr subst-waee-with e b)))
       (id (n) (error 'id "unbound identifier")))))

;;; Subsitution operator for foldr above
(define subst-waee-with
  (lambda (y expr)
    (let ((var (car y))
          (val (cadr y)))
      (subst expr var val))))

;;; Operator definition table
(define op-list
  (list (binop-rec 'plus +)
        (binop-rec 'minus -)
        (binop-rec 'times *)
        (binop-rec 'div /)))

;;; Define interp
(define eval-waee
  (lambda (waee)
    (interp-waee (parse-waee waee))))

(define test-waee
  (lambda (f)
    (begin
      (test (f '1) 1)
      (test (f '{+ 1 1}) 2)
      (test (f '{- 1 1}) 0)
      (test (f '{* 2 2}) 4)
      (test (f '{/ 4 2}) 2)
      (test (f '{with {{x 3}} {+ x x}}) 6)
      (test (f '{with {{x 3} {y 4}} {+ x y}}) 7)
      (test (f '{with {{x 3} {y 4}} {+ x y}}) 7)
      (test (f '{with {{x 3}} {with {{y 4}} {+ x y}}}) 7)
      (test (f '{with {{x 3}} {with {{y {+ x x}}} {+ x y}}}) 9)
      (test (f '{with {{x 3}} {with {{y {+ x x}}} {with {{x 1}} {+ x y}}}}) 7)
      (test (f '{with {{x 1} {y 2}} {with {{z x} {x x}} {with {{z {+ z 1}}} {+ z y}}}}) 4))))