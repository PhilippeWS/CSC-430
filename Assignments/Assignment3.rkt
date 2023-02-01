#lang typed/racket
(require typed/rackunit)

;;LANGUAGE Definitions -----------------------------------------------------------------------------------------------
(define-type ExprC (U NumC IdC BinopC leq0? AppC))
(struct NumC ([n : Real])#:transparent)
(struct IdC ([s : Symbol])#:transparent)
(struct BinopC ([op : Symbol] [l : ExprC] [r : ExprC])#:transparent)
(struct leq0? ([suspect : ExprC] [iftrue : ExprC] [iffalse : ExprC])#:transparent)
(struct AppC ([fun : Symbol] [arg : ExprC])#:transparent)
(struct FundefC ([name : Symbol] [par : Symbol] [body : ExprC])#:transparent)


;;HELPER Functions --------------------------------------------------------------------------------------------------
;;IKEU-divide
;;Defines a version of division that returns a custom error upon division by 0
(define (IKEU-divide [n1 : Real] [n2 : Real]) : Real
  (cond
    [(equal? 0 n2) (error "IKEU-divide by 0 error")]
    [else (/ n1 n2)]))

(check-equal? (IKEU-divide 1 1) 1)
(check-exn (regexp (regexp-quote "IKEU-divide by 0 error"))
           (lambda () (IKEU-divide 1 0)))


;;get-op
;;Returns the operator function to match a specific symbol for BinopC
(define (get-op [s : Symbol]) : (-> Real Real Real)
  (match s
    ['+ +]
    ['- -]
    ['* *]
    ['/ IKEU-divide]
    [_ (error "IKEU-get-op error")]))

(check-equal? (get-op '+) +)
(check-equal? (get-op '-) -)
(check-equal? (get-op '*) *)
(check-equal? (get-op '/) IKEU-divide)
(check-exn (regexp (regexp-quote "IKEU-get-op error"))
           (lambda () (get-op 'fail)))


;;lookup
;;Searchs a list of FundefC for a function with a name matching passed symbol
(define (lookup [target : Symbol] [funs : (Listof FundefC)]) : FundefC
  (cond
    [(empty? funs) (error "IKEU-lookup error")]
    [(symbol=? target (match (first funs)
                       [(FundefC fname _ _) fname])) (first funs)]
    [else (lookup target (rest funs))]))

(check-equal? (lookup 'main (list (FundefC 'main 'init (NumC 0)))) (FundefC 'main 'init (NumC 0)))
(check-equal? (lookup 'main (list
                             (FundefC 'notmain 'init (NumC 0))
                             (FundefC 'main 'init (NumC 0))))
              (FundefC 'main 'init (NumC 0)))
(check-exn (regexp (regexp-quote "IKEU-lookup error"))
           (lambda () (lookup 'fail '())))


;;subst
;;Takes an ExperC and substitutes all target symbols with another ExperC
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(AppC f a) (AppC f (subst what for a))]
    [(BinopC op l r) (BinopC op (subst what for l) (subst what for r))]
    [(leq0? suspect iftrue iffalse) (leq0? (subst what for suspect) (subst what for iftrue) (subst what for iffalse))]))


(check-equal? (subst (IdC 'not)'not (NumC 0)) (NumC 0))
(check-equal? (subst (NumC 0) 'x (IdC 'x)) (NumC 0))
(check-equal? (subst (IdC 'x) 'y (IdC 'x)) (IdC 'x))
(check-equal? (subst (NumC 0) 'x (BinopC '+ (IdC 'x) (NumC 0))) (BinopC '+ (NumC 0) (NumC 0)))
(check-equal? (subst (NumC 0) 'x (AppC 'main (IdC 'x))) (AppC 'main (NumC 0)))
(check-equal? (subst (NumC 0) 'x (leq0? (IdC 'x) (NumC 0) (NumC 1))) (leq0? (NumC 0) (NumC 0) (NumC 1)))


;;ch-op?
;;Checks if the symbol passed is a valid mathematical operator
(define (ch-op? [s : Symbol]) : Boolean
  (cond
    [(member s '(+ - * /)) #t]
    [else #f]))

(check-equal? (ch-op? '+) #t)
(check-equal? (ch-op? 'true) #f)


;;ch-not-keyword?
;;Checks if the passed object is a keyword in the IKEU language
(define (ch-not-keyword? [s : Any]) : Boolean
  (cond
    [(not (symbol? s)) #t]
    [else (cond
            [(and  (not (ch-op? s)) (not (member s '(leq0? fundef)))) #t]
            [else #f])]))

(check-equal? (ch-not-keyword? 0) #t)
(check-equal? (ch-not-keyword? '+) #f)
(check-equal? (ch-not-keyword? 'True) #t)


;;PARSER Functions ---------------------------------------------------------------------------------------------------
;;Parse
;;Takes in an Sexp and parses it into an ExprC which is interpretable by the IKEU interpreter
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(and (and (? symbol? id) (? ch-not-keyword? id)) (? ch-not-keyword? id)) (IdC id)]
    [(list (and (? symbol? op) (? ch-op? op)) (? ch-not-keyword? l) (? ch-not-keyword? r))
     (BinopC op (parse l) (parse r))]    
    [(list 'leq0? s 'then ift 'else iff) (leq0? (parse s) (parse ift) (parse iff))]
    [(list (and (? symbol? id) (? ch-not-keyword? id)) exp) (AppC id (parse exp))]
    [_ (error "IKEU-parse error")]))

(check-equal? (parse 5) (NumC 5))
(check-equal? (parse 'x) (IdC 'x))
(check-equal? (parse '{+ x 1}) (BinopC '+ (IdC 'x) (NumC 1)))
(check-equal? (parse '{fun 0}) (AppC 'fun (NumC 0)))
(check-equal? (parse '{leq0? x then 0 else 1}) (leq0? (IdC 'x) (NumC 0) (NumC 1)))
(check-exn (regexp (regexp-quote "IKEU-parse error"))
           (lambda () (parse "fail")))


;;Parse-fundef
;;Parses a function definition written in the IKEU syntax
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'fundef
           (and (? symbol? funname) (? ch-not-keyword? funname))
           (list (and (? symbol? argument) (? ch-not-keyword? argument))) ': funbody)
     (FundefC funname argument (parse funbody))]
    [_ (error "IKEU-parse-fundef error")]))

(check-equal? (parse-fundef '{fundef addone {x} : {+ x 1}}) (FundefC 'addone 'x (BinopC '+ (IdC 'x) (NumC 1))))
(check-exn (regexp (regexp-quote "IKEU-parse-fundef error"))
           (lambda () (parse-fundef "fail")))

;;parse-prog
;;Takes in an entire IKEU program and transforms it into an AST (list of FundefCs)
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    ['() '()]
    [(cons (? list? fd) remainingFunctions) (cons (parse-fundef fd) (parse-prog remainingFunctions))]
    [_ (error "IKEU-parse-prog error")]))

(check-equal? (parse-prog '{{fundef main {init} : {addone 2}} {fundef addone {x} : {+ x 1}}})
              (list
               (FundefC 'main 'init (AppC 'addone (NumC 2)))
               (FundefC 'addone 'x (BinopC '+ (IdC 'x) (NumC 1)))))
(check-exn (regexp (regexp-quote "IKEU-parse-prog error"))
           (lambda () (parse-prog 'x)))


;;INTERPRETER Functions ----------------------------------------------------------------------------------------------
;;interp
;;Interprets and evaluates an AST representation parsed from the IKEU Parser
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC op l r) ((get-op op) (interp l funs) (interp r funs))]
    [(leq0? s ift iff)
     (cond
       [(<= (interp s funs) 0) (interp ift funs)]
       [else (interp iff funs)])]
    [(AppC f arg)
     (define argval (NumC (interp arg funs)))
       (match (lookup f funs)
         [(FundefC _ param body)
          (define new-body (subst argval param body))
          (interp new-body funs)])]
    [(IdC _) (error "IKEU-interp-IdC error")]))

(check-equal? (interp (NumC 5) '()) 5)
(check-equal? (interp (BinopC '+ (NumC 6) (NumC 5)) '{}) 11)
(check-equal? (interp (leq0? (NumC 5) (NumC 10) (NumC 3)) '{}) 3)
(check-equal? (interp (leq0? (NumC -1) (NumC 10) (NumC 3)) '{}) 10)
(check-equal? (interp (AppC 'add (NumC 21)) (list (FundefC 'add 'x (BinopC '+ (IdC 'x) (NumC 11))) )) 32 )
(check-exn (regexp (regexp-quote "IKEU-interp-IdC error"))
           (lambda () (interp (IdC 'a) '{})))


;;interp-fns
;;Runs the main function from a list of functions parsed by the IKEU parser
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (match (lookup 'main funs)
    [(FundefC 'main 'init body) (interp (subst (NumC 0) 'init body) funs)]))

(check-equal? (interp-fns (list
               (FundefC 'main 'init (AppC 'addone (NumC 2)))
               (FundefC 'addone 'x (BinopC '+ (IdC 'x) (NumC 1))))) 3)


;;top-interp
;;Takes in an IKEU program and parsers + interprets it for an actual answer
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))

(check-equal? (top-interp '{{fundef main {init} : {addone 2}} {fundef addone {x} : {+ x 1}}}) 3)
