#lang typed/racket
(require typed/rackunit)
(require racket/math)

;;Question 1
;;total-profit
;;Param: {attendees} (Real) | Return: (Real)
;;Returns profit based on number of attendees
(define operation-cost : Real -20)
(define attendence-cost : Real 5)
(define attendee-operation-cost -0.5)
(define (total-profit [attendees : Real]) : Real
  (+ operation-cost
     (* attendence-cost attendees)
     (* attendee-operation-cost attendees)))

(check-equal? (total-profit 10) 25.0)


;;Question 2
;;area-cylinder
;;Param: {radius} (Real), {height} (Real) | Return: (Real)
;;Returns volume of a cylinder with passed radius and passed volume
(define (area-cylinder [radius : Real] [height : Real]) : Real
  (+ (* pi (expt radius 2) 2)
   (* height (* 2 pi radius))))

(check-= (area-cylinder 1 3) 25.1 .1)

; represents a writing implement
(define-type Writer (U Pen Pencil))
; ink volume in ml, how-full a number in the range 0.0 to 1.0
(struct Pen ([capacity : Real] [how-full : Real]) #:transparent)
; length in cm
(struct Pencil ([length : Real]) #:transparent)

;;Question 3
;;how-far-to-write
;;Param: {writer} (Writer) | Return: (Real)
;;Returns how far the writer would be able to travel before depletion
(define (how-far-to-write [writer : Writer]) : Real
  (match writer
      [(Pencil l) (* l 56)]
      [(Pen c p) (* 150 c p)]))

(check-= (how-far-to-write (Pen 10 .2)) 300 0)
(check-= (how-far-to-write (Pencil 10)) 560 0)


;;Represents a quadratic and linear form polynomial
(define-type Polynomial (U Linear Quadratic))

(struct Linear ([A : Real] [B : Real])#:transparent)
(struct Quadratic ([A : Real] [B : Real] [C : Real])#:transparent)

;;Question 4
;;interp
;;Param: {polynomial} (Polynomial), {x} (Real) | Return: (Real)
;;Returns polynomial evaluated at x
(define (interp [polynomial : Polynomial] [x : Real]) : Real
  (match polynomial
    [(Linear A B) (+ (* A x) B)]
    [(Quadratic A B C) (+ (* A (expt x 2)) (* B x) C)]))

(check-= (interp (Linear 2 3) 4) 11 0)
(check-= (interp (Quadratic 2 3 4) 2) 18 0)

;;Question 5
;;derivative
;;Param: {polynomial} (Polynomial) | Return: (Real)
;;Returns a derivative polynomial representation of the passed polynomials
(define (derivative [polynomial : Polynomial]) : Polynomial
  (match polynomial
    [(Linear A B) (Linear 0 A)]
    [(Quadratic A B C) (Linear (* A 2) B)]))

(check-equal? (derivative (Linear 3 4)) (Linear 0 3))
(check-equal? (derivative (Quadratic 5 6 2)) (Linear 10 6))


;;Question 6
;;Represents chaining tree struct
(define-type BTree (U Leaf Node))
(struct Node ([left : BTree] [right : BTree])#:transparent)
(struct Leaf ([value : Symbol])#:transparent)

(define bt1 : BTree (Node (Node (Leaf 'a) (Leaf 'b)) (Leaf 'c)))
(define bt2 : BTree (Node (Node (Leaf 'a) (Leaf 'b)) (Node (Leaf 'c) (Leaf 'd))))
(define bt3 : BTree (Node (Leaf 'a) (Leaf 'b)))


;;Question 7
;;zz-tree
;;Param: {btree} (BTree) | Return: (BTree)
;:Takes a btree and mirrors it with 'zz as every value
(define (zz-tree [btree : BTree]) : BTree
  (match btree
    [(Leaf v) (Leaf 'zz)]
    [(Node l r) (Node (zz-tree l) (zz-tree r))]))

(check-equal? (zz-tree bt1) (Node (Node (Leaf 'zz) (Leaf 'zz)) (Leaf 'zz)))

;;Question 8
;;min-depth
;;Param: {btree} (BTree) | Return: (Real)
;;Takes a btree and returns length of path to nearest leaf
(define (min-depth [btree : BTree]) : Real
  (match btree
    [(Leaf v) 0]
    [(Node l r) (min (+ (min-depth l) 1)  (+ (min-depth r) 1))]))

(check-equal? (min-depth bt1) 1)


;;Question 9
;;contains?
;;Param: {btree} (BTree), {symbol} (Symbol) | Return: (Boolean)
;;Takes a btree and searchs for target symbol to see if it is contained;
(define (contains? [btree : BTree] [symbol : Symbol]) : Boolean
  (match btree
    [(Leaf v) (eq? symbol v)]
    [(Node l r) (or (contains? l symbol) (contains? r symbol))]))

(check-equal? (contains? bt1 'a) #t)
(check-equal? (contains? bt1 'z) #f)

;;Question 10
;;subst
;;Param: {btree} (BTree), {symbol} (Symbol), {replacement} (BTree) | Return: (BTree)
;;Takes a btree and replaces all instances of that symbol with a new BTree
(define (subst [btree : BTree] [symbol : Symbol] [replacement : BTree]) : BTree
  (match btree
    [(Leaf v) (cond
                [(eq? symbol v) replacement]
                [else (Leaf v)])]
    [(Node l r) (Node (subst l symbol replacement) (subst r symbol replacement))]))

(check-equal? (subst bt3 'a bt1) (Node bt1 (Leaf 'b)))


;;Question 11
;;all-path-lengths
;;Param: {btree} (BTree) {count} (Real) | Return: (BTree)
;;Returns list of all path lengths of the passed btree
(define (all-path-lengths [btree : BTree]) : (Listof Real)
  (all-path-lengths-helper btree 0))

;;The helper function that utilizes an accumulator to count the path lengths.
(define (all-path-lengths-helper [btree : BTree] [count : Real]) : (Listof Real)
  (match btree
    [(Leaf v) (list count)]
    [(Node l r) (append (all-path-lengths-helper l  (+ count 1)) (all-path-lengths-helper r (+ count 1)))]))

(check-equal? (all-path-lengths bt1) (list 2 2 1))


