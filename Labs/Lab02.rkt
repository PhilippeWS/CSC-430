#lang typed/racket
(require typed/rackunit)
;;j85sqn

;; QUESTION 1
;;Param: {los} (ListOf String) | Return: String
;;Appends all strings in the list into one string in reverse order
(define (rev-str-app [los : (Listof String)]) : String
  (match los
    ['() ""]
    [(cons v r) (string-append (rev-str-app r) v)]))

(check-equal? (rev-str-app '()) "")
(check-equal? (rev-str-app '("1" "2" "3")) "321")

;;QUESTION 2
#|
The type of our function is simply '(String).
Makes sense since it evaluates to '(String) as an expression.

The type of the + "operator" is very long because there are many types that can
be the result of an expression utilizing the + "operator".
|#

;;QUESTION 3
#|
(define-type BiType (U "Trek" "Bianchi" "Gunnar"))
(Struct Bicycle ([Model : BiType] [d : Real]))
|#

(struct Trek ([d : Real]) #:transparent)
(struct Bianchi ([d : Real]) #:transparent)
(struct Gunnar ([d : Real]) #:transparent)

(define-type Bicycle (U Trek Bianchi Gunnar))

;;QUESTION 5
;;Param: {lob} (Listof Bicycle) | Return: (Listof Trek)
;;Filters all non-trek bicycles out of the lsist
(define (only-treks [lob : (Listof Bicycle)]) : (Listof Trek)
  (match lob
    ['() '()]
    [(cons v r) (cond
                  [(Trek? v) (cons v (only-treks r))]
                  [else (only-treks r)])]))

(define testTrekList : (Listof Bicycle) (list (Trek 2) (Bianchi 3) (Trek 4) (Gunnar 5) (Trek 6)))

(define exp : (Listof Trek) (list (Trek 2) (Trek 4) (Trek 6)))

(check-equal? (only-treks testTrekList) exp)
(check-equal? (only-treks '()) '())


;;QUESTION 6
;;Param: {lob} (Listof Bicycle) | Return: (Listof Bianchis)
;;Filters all non-bianchis bicycles out of the lsist
(define (only-bianchis [lob : (Listof Bicycle)]) : (Listof Bianchi)
  (match lob
    ['() '()]
    [(cons v r) (cond
                  [(Bianchi? v) (append (list v) (only-bianchis r))]
                  [else (only-bianchis r)])]))

(define testBianchiList : (Listof Bicycle) (list (Bianchi 2) (Bianchi 4) (Trek 7) (Gunnar 5) (Bianchi 6)))

(define exp2 : (Listof Bianchi) (list (Bianchi 2) (Bianchi 4) (Bianchi 6)))

(check-equal? (only-bianchis testBianchiList) exp2)
(check-equal? (only-bianchis '()) '())


;;QUESTION 7
;;Param: {lob} (Listof Bicycle), {pred} (-> Any Boolean) | Return: (Listof Type)
;;Filters all non-bianchis bicycles out of the lsist
(define (onlyThese [lob : (Listof Bicycle)] [pred : (-> Any Boolean)]) : (Listof Bicycle)
  (match lob
    ['() '()]
    [(cons v r) (cond
                  [(pred v) (cons v (onlyThese r pred))]
                  [else (onlyThese r pred)])]))


(define testTheseList : (Listof Bicycle) (list (Bianchi 2) (Bianchi 3) (Trek 4) (Trek 5) (Gunnar 6) (Gunnar 7)))

(define exp3 : (Listof Bianchi) (list (Bianchi 2) (Bianchi 3)))
(define exp4 : (Listof Trek) (list (Trek 4) (Trek 5)))
(define exp5 : (Listof Gunnar) (list (Gunnar 6) (Gunnar 7)))

(check-equal? (onlyThese testTheseList Bianchi?) exp3)
(check-equal? (onlyThese testTheseList Trek?) exp4)
(check-equal? (onlyThese testTheseList Gunnar?) exp5)
(check-equal? (onlyThese '() Trek?) '())


;;QUESTION 8
;;Param: {list1} (Listof Any), {list2} (Listof Any) | Return: (Listof Any)
;;Appends two list together
    ;;More effecient way to do it would be find the pointer at the end of list1
    ;;and change its reference from empty list to list2, but unsure how to do it.
(define (my-append [list1 : (Listof Any)] [list2 : (Listof Any)]) : (Listof Any)
  (match list1
    [`() list2]
    [(cons v r)  (cons v (my-append r list2))]))

(check-equal? (my-append (list 1 2 3) (list 4 5 6)) '(1 2 3 4 5 6))
(check-equal? (my-append '() '()) '())

;;QUESTION 9
;;Param: {list} (Listof Any), {n} (Integer) | Return: (Listof Any)
;;Returns first n elements of the passed list
(define (my-take [list : (Listof Any)] [n : Integer]) : (Listof Any)
  (cond
    [(> n (length list)) '()]
    [(<= n 0) '()]
    [else (cons (first list) (my-take (rest list) (- n 1)))]))

(check-equal? (my-take (list 1 2 3 4 5 6) 3) '(1 2 3))
(check-equal? (my-take (list 1 2) 3) '())
(check-equal? (my-take '() 0) '())