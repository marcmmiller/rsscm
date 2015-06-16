;;
;; Minimal Scheme library that tries its best to adhere to R6RS spec
;; and assumes a Scheme interpreter with the following features:
;;
;; Special forms:
;;  lambda (with varargs support)
;;  if
;;  and
;;  or
;;  define
;;  define-macro (most basic scheme macro support)
;;
;; Builtin functions:
;;  apply
;;  cons
;;  car
;;  cdr
;;  eq?  (assumed to behave like eqv?)
;;  null?
;;  pair?
;;  + * / - mod
;;  < > =
;;  display
;;  newline
;;  error
;;  assertion-violation
;;

;; This implementation assumes that eq? === eqv?
(define (eqv? a b)
  (eq? a b))

(define-macro begin
  (lambda (stm . rest)
    (cons (cons 'lambda
                (cons '()
                      (cons stm
                            rest)))
          '())))

(define (list first . rest)
  (cons first rest))

(define (not x) (if x #f #t))

(define (caar p) (car (car p)))
(define (cadr p) (car (cdr p)))
(define (cdar p) (cdr (car p)))
(define (cddr p) (cdr (cdr p)))

(define (caaar p) (car (caar p)))
(define (caadr p) (car (cadr p)))
(define (cadar p) (car (cdar p)))
(define (caddr p) (car (cddr p)))
(define (cdaar p) (cdr (caar p)))
(define (cdadr p) (cdr (cadr p)))
(define (cddar p) (cdr (cdar p)))
(define (cdddr p) (cdr (cddr p)))

(define (caaaar p) (car (caaar p)))
(define (caaadr p) (car (caadr p)))
(define (caadar p) (car (cadar p)))
(define (caaddr p) (car (caddr p)))
(define (cadaar p) (car (cdaar p)))
(define (cadadr p) (car (cdadr p)))
(define (caddar p) (car (cddar p)))
(define (cadddr p) (car (cdddr p)))
(define (cdaaar p) (cdr (caaar p)))
(define (cdaadr p) (cdr (caadr p)))
(define (cdadar p) (cdr (cadar p)))
(define (cdaddr p) (cdr (caddr p)))
(define (cddaar p) (cdr (cdaar p)))
(define (cddadr p) (cdr (cdadr p)))
(define (cdddar p) (cdr (cddar p)))
(define (cddddr p) (cdr (cdddr p)))

(define (even? n)
  (= 0 (mod n 2)))

(define (equal? a b)
  (if (pair? a)
      (and (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (eq? a b)))

;; not in R6RS, but useful utility until we have 'exists?'
(define some?
  (lambda (func list)
    ;; returns #f if (func x) returns #t for some x in the list
    (and (pair? list)
         (or (func (car list))
             (some? func (cdr list))))))

(define map
  (lambda (func list1 . more-lists)
    (define map1
      (lambda (func list)
        ;; non-variadic map.  Returns a list whose elements the result
        ;; of calling func with corresponding elements of list
        (if (null? list)
            '()
            (cons (func (car list))
                  (map1 func (cdr list))))))
    ;; Non-variadic map implementation terminates when any of the
    ;; argument lists is empty.
    ((lambda (lists)
       (if (some? null? lists)
           '() ;; TODO: R6RS implies error should be thrown when lists
               ;; are different lengths.
           (cons (apply func (map1 car lists))
                 (apply map func (map1 cdr lists)))))
     (cons list1 more-lists))))

(define (append l . loo)
  (define (simple-append l m)
    ;; cond hasn't been defined yet (and uses append)
    (if (null? l)
        m
        (if (pair? l)
            (cons (car l) (simple-append (cdr l) m))
            l)))
  (if (and (pair? loo)
           (pair? (car loo))
           (pair? (cdr loo)))
      (simple-append l (apply append (car loo) (cdr loo)))
      (apply simple-append l loo)))

;; parallel-binding "let"
(define-macro let
  (lambda (forms . body)
    (cons (append (cons 'lambda (list (map car forms)))
                  body)
          (map cadr forms))))

;; We've waited this long to define `assert` because it needs
;; `list` and `let`, which needs `append` and `map`.
(define-macro assert
  (lambda (tst)
    (list 'let (list (list '__asres tst))
          (list 'or '__asres
                (list 'assertion-violation
                      #f
                      "Assertion failed"
                      (list 'quote tst)
                      '__asres)))))

(assert (equal? '(a (b c)) '(a (b c))))
(assert (eq? (append) '()))
(assert (equal? (append '(a b) '(c d)) '(a b c d)))

;;
;; List Utilities
;;

(define (exists pred . lists)
  (if (some? null? lists)
      ;; TODO per spec, base case needs to return last result from pred
      #f ;; TODO: error when all lists aren't the same length
      (or (apply pred (map car lists))
          (apply exists pred (map cdr lists)))))

(assert (eq? #f (exists even? '(3 1 1 5 9))))
(assert (eq? #t (exists even? '(3 1 1 6 9))))
(assert (eq? #t (exists < '(1 2 4) '(2 3 4))))
(assert (eq? #f (exists > '(1 2 3) '(2 3 4))))

(define (for-all pred . lists)
  (if (some? null? lists)
      ;; TODO per spec, base case needs to return last result from proc
      ;; TODO: need to error when all lists aren't the same length
      #t
      (and (apply pred (map car lists))
           (apply for-all pred (map cdr lists)))))

(assert (eq? #t (for-all even? '(2 4 6))))
(assert (eq? #f (for-all even? '(1 2 3))))
(assert (eq? #t (for-all < '(1 2 3) '(2 3 4))))
(assert (eq? #f (for-all < '(1 2 4) '(2 3 4))))

(define (memp pred? list)
  (if (null? list)
      #f
      (if (pred? (car list))
          list
          (memp pred? (cdr list)))))

(define (member obj list)
  (memp (lambda (i) (equal? obj i)) list))

(define (memq obj list)
  (memp (lambda (i) (eq? obj i)) list))

(define (memv obj list)
  (memp (lambda (i) (eqv? obj i)) list))

(assert (equal? (memp even? '(3 1 4 1 5 9) '(4 1 5 9))))


(define (filter proc l)
  (if (null? l)
      '()
      (if (proc (car l))
          (cons (car l) (filter proc (cdr l)))
          (filter proc (cdr l)))))


(define (fold-left combine nil . lists)
  (if (some? null? lists)
      nil
      (apply fold-left
             combine
             (apply combine nil (map car lists))
             (map cdr lists))))

(assert (= 0 (fold-left + 0 '())))
(assert (= 6 (fold-left + 0 '(1 2 3))))
(assert (= 21 (fold-left + 0 '(1 2 3) '(4 5 6))))


;; "cond" macro
(define-macro cond
  (lambda (f . rest)
    (define (inner clauses)
      (if (null? clauses)
          '()
          (let ((clause (car clauses))) ;; clause is like ((eq? a b) a)
            (if (eq? (car clause) 'else)
                (cadr clause)
                (append (append (list 'if (car clause)) (cdr clause))
                        (list (inner (cdr clauses))))))))
    (inner (cons f rest))))

;;
;; Association Lists
;;
(define (assp proc alist)
  (if (null? alist)
      #f
      (if (proc (caar alist))
          (car alist)
          (assp prod (cdr alist)))))

(define (assoc obj alist)
  (assp (lambda (i) (equal? obj i)) alist))

(define (assq obj alist)
  (assp (lambda (i) (eq? obj i)) alist))

(define (assv obj alist)
  (assp (lambda (i) (eqv? obj i)) alist))


;; non-standard
(define (print l . r)
  (display l)
  (if (pair? r)
      (apply print r)))

;; non-standard
(define (println l . r)
  (apply print l r)
  (newline))

