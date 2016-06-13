#lang plai
;;; EECS 662 - Miniproject 2 Solution
;;;
;;; Author: Perry Alexander
;;; Date: 4/14/10
;;; Revised: 4/14/14, 4/22/16
;;;
;;; Description:
;;; This is an example solution for miniproject 2 implementing both the cfae 
;;; and cfwae interpreters. Note that there is no error checking included in
;;; this code.  The code has been tested with roughly the same test cases as
;;; your project submissions, but please don't take correctnesss for granted.
;;;
;;; If you use this code in your implementation of a later project, you must 
;;; indicate this in the comments for your submission.  Citation is the
;;; currency of acdemia.  It is the way we pay eachother for our work.  Always
;;; cite your sources!

;;;; CFAE Interpreter

;;; Define an AST type for CFAE constructs.  Standard expression elements plus
;;; functions and if0.
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

;;; Define a type and lookup function for deferred substitution.  Code is
;;; taken from the PLAI text.

(define-type DefrdSubst
  (mtSub)
  (aSub (id symbol?)
        (value CFAE?)
        (ds DefrdSubst?)))

(define lookup
  (lambda (id ds)
    (type-case DefrdSubst ds
       (mtSub () (error 'lookup "identifier ~s not found" id))
       (aSub (bound-id bound-value ds)
             (if (symbol=? id bound-id)
                 bound-value
                 (lookup id ds))))))

;;; Define a parser for CFAE constructs.  This parser does no error checking
;;; at all. Simply converts concrete syntax to AST.
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

;;; Define an interpreter for CFAE AST structures.
(define interp
  (lambda (expr ds)
    (type-case CFAE expr
       (num (n) (num n))
       (add (l r) (num (+ (num-n (interp l ds)) (num-n (interp r ds)))))
       (sub (l r) (num (- (num-n (interp l ds)) (num-n (interp r ds)))))
       (mul (l r) (num (* (num-n (interp l ds)) (num-n (interp r ds)))))
       (div (l r) (num (/ (num-n (interp l ds)) (num-n (interp r ds)))))
       (id (v) (lookup v ds))
       (if0 (c t e) (if (= (num-n (interp c ds)) 0) (interp t ds) (interp e ds)))
       (fun (a b) (fun a b))
       (app (fun-expr arg-expr)
            (local ((define the-fun-def (interp fun-expr ds)))
              (interp (fun-body the-fun-def)
                      (aSub (fun-arg-name the-fun-def)
                            (interp arg-expr ds) ds)))))))

;;; Put the parser and interpreter together to construct an evaluator
(define eval-cfae
  (lambda (expr)
    (interp (parse-cfae expr) (mtSub))))

;;;; CFWAE Intepreter

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

;;; Define a prelude for the CFWAE language
(define prelude
  (aSub 'pi (num 3.14159)
    (aSub 'inc (fun 'x (add (id 'x) (num 1)))
      (aSub 'area (fun 'r (mul (num 3.141) (mul (id 'r) (id 'r)))) (mtSub)))))


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

;;; Define an elaborator from CFWAE to CFAE
(define elab
  (lambda (expr)
    (type-case CFWAE expr
      (numw (n) (num n))
      (addw (l r) (add (elab l) (elab r)))
      (subw (l r) (sub (elab l) (elab r)))
      (mulw (l r) (mul (elab l) (elab r)))
      (divw (l r) (div (elab l) (elab r)))
      (withw (name named-expr body) (app (fun name (elab body)) (elab named-expr)))
      (condw (a d) (elab-cond a (elab d)))
      (idw (n) (id n))
      (if0w (c t f) (if0 (elab c) (elab t) (elab f)))
      (funw (a b) (fun a (elab b)))
      (appw (f a) (app (elab f) (elab a))))))

;;; Define a helper function for elaborating cond structures
(define elab-cond
  (lambda (arms default)
    (cond ((empty? arms) default)
          (else (if0 (elab (car (car arms)))
                     (elab (cadr (car arms)))
                     (elab-cond (cdr arms) default))))))

;;; Put everything together to define an evaluator for CFWAE constructs
(define eval-cfwae
  (lambda (expr)
    (interp (elab (parse-cfwae expr)) prelude)))