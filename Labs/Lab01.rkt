#lang typed/racket
;; Submit Code : 3t5iet
(require typed/rackunit)

; ==>: Function returns false upon sunny=true and friday=true, else true;
(define (==> [sunny : Boolean] [friday : Boolean]) : Boolean
  (or (not sunny) friday))

(check-equal? (==> false true) true)
(check-equal? (==> true true) true)
(check-equal? (==> false false) true)
(check-equal? (==> true false) false)

(: OOBError String)
(define OOBError "Paramter is out of bounds.") 

; string-insert: Inserts an underscore into desired string at desired location.
(define (string-insert [str : String] [i : Integer]) : String
  (cond[(> i (string-length str)) OOBError]
       [(< i 0) OOBError]
       [else (string-append (substring str 0 i) "_" (substring str i))]
       )
  )

(check-equal? (string-insert "String" 3) "Str_ing")
(check-equal? (string-insert "String" -1) OOBError)
(check-equal? (string-insert "String" 7) OOBError)

(define BASE_ATTENDANCE : Real 120)

(define BASE_TICKET_PRICE : Real 5.0)

(define CHANGE_IN_ATTENDANCE : Real 15)

(define COST_PER_ATTENDANCE_CHANGE : Real 0.1)

(define BASE_PERFORMANCE_COST : Real 180)

(define BASE_ATTENDEE_COST : Real 0.04)

(define (attendees [ticket-price : Real]) : Real
  (- BASE_ATTENDANCE (* (- ticket-price BASE_TICKET_PRICE) (/ CHANGE_IN_ATTENDANCE COST_PER_ATTENDANCE_CHANGE))))

(define (revenue [ticket-price : Real]) : Real
  (* ticket-price (attendees ticket-price)))

(define (cost [ticket-price : Real]) : Real
  (+ BASE_PERFORMANCE_COST (* BASE_ATTENDEE_COST (attendees ticket-price))))

(define (profit [ticket-price : Real]) : Real
  ( - (revenue ticket-price)
     (cost ticket-price)))

;;interest: returns the total interest earned on deposited amount
(define (interest [deposit : Real]) : Real
  (cond[(<= deposit 1000) (* deposit 0.04)]
       [(<= deposit 5000) (* deposit 0.045)]
       [else (* deposit 0.05)]))

(check-equal? (interest 1000) 40.0)
(check-equal? (interest 5000) 225.0)
(check-equal? (interest 10000) 500.0)

(define-type NULL 'null)

;;This type is encompassed within ?Furniture
(define-type ?Desk (U Desk NULL))
(struct Desk ([width : Real] [height : Real] [depth : Real]) #:transparent)

(define exampleDesk (Desk 3 4 5))

;;This type is encompassed within ?Furniture
(define-type ?Bookshelf (U Bookshelf NULL))
(struct Bookshelf ([width : Real] [depth : Real] [noShelves : Real]) #:transparent)

(define exampleBookshelf (Bookshelf 4 3 6))

(define-type ?Furniture (U Desk Bookshelf NULL))

;; Consumes a?Desk, returns the floor space
(define (furniture-footprint [f : ?Furniture]) : (U Real NULL)
  (match f
    [(Desk w _ d) (* w d)]
    [(Bookshelf w d _) (* w d)] 
    [NULL 'null]))

(check-equal? (furniture-footprint exampleDesk) 15)
(check-equal? (furniture-footprint exampleBookshelf) 12)
(check-equal? (furniture-footprint 'null) 'null)


