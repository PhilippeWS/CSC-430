#lang typed/racket
(require typed/rackunit)
;;l68k2e
;; Question 1
;; Defines a parser expecting exactly a list of 3 items, a real, 'chris, and a symbol
;; returns false if pattern is not matched
(define (parse000 [s : Sexp]) : Boolean
  (match s
    [(list (? real? r) 'chris (? symbol? s)) #t]
    [else #f]))

(check-equal? (parse000 (list 4 'chris 'p)) #t)
(check-equal? (parse000 5) #f)

;; Question 1
;; Defines a parser expecting exactly a list of 3 items, a real, 'chris, and a symbol
;; returns false if pattern is not matched
(define (parse001 [s : Sexp]) : (U Symbol Boolean)
  (match s
    [(list (? real? r) 'chris (? symbol? s)) s]
    [else #f]))

(check-equal? (parse001 (list 4 'chris 'p)) 'p)
(check-equal? (parse001 5) #f)

;; Question 2
;; Defines a parser expecting a list of 3 elements, where the first and last are irrelevant but
;; the middle element needs to be a (Listof Real)
(define (parse002 [s : Sexp]) : (U (Listof Real) Boolean)
  (match s
    [(list e1 (list (? real? nl) ...) e2) (cast nl (Listof Real))]
    [else #f]))

(check-equal? (parse002 (list 'chris '(1 2 3) 'p)) '(1 2 3))
(check-equal? (parse002 5) #f)

;; Question 3
;; Defines a function that takes any value and returns 'okay if it is a Real,
;; otherwise raises an exception.
(define (ohno [value : Any]) : 'okay
  (match value
    [(? real? value) 'okay]
    [else (error 'ohnoError "Expected a Real, instead got: ~e" value)]))


(check-equal? (ohno 13) 'okay)
(check-exn (regexp (regexp-quote "Expected a Real, instead got: 'notgood"))
           (lambda () (ohno 'notgood)))


;; Question 4
(define-type ArithC (U numC plusC multC))
(struct numC ([n : Real])#:transparent)
(struct plusC ([l : ArithC] [r : ArithC])#:transparent)
(struct multC ([l : ArithC] [r : ArithC])#:transparent)

;; Question 5
;; Defines an interpreter that takes an Expression from our defined language and interprets it
(define (interp [a : ArithC]) : Real
    (match a
      [(numC n) n]
      [(plusC l r) (+ (interp l) (interp r))]
      [(multC  l r) (* (interp l) (interp r))]))

(check-equal? (interp (plusC (multC (numC 1) (numC 3)) (numC 2))) 5)


;; Question 6
;; Defines a function that determines the number of plusC operations that exist
;; within an AST
(define (num-adds [a : ArithC]) : Real
    (match a
      [(numC n) 0]
      [(plusC l r) (+ (num-adds l) (num-adds r) 1)]
      [(multC l r) (+ (num-adds l) (num-adds r))]))


(check-equal? (num-adds (multC (numC 0) (plusC (plusC (numC 3) (numC 6)) (numC 4)))) 2)