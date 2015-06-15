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
;;  eq?
;;  null?
;;  pair?
;;  + * / - mod
;;  < > =
;;  display
;;  newline
;;  error
;;  assertion-violation
;;
(define-macro begin
  (lambda (stm . rest)
    (cons (cons 'lambda
                (cons '()
                      (cons stm
                            rest)))
          '())))

(define (list first . rest)
  (cons first rest))

(define-macro assert
  (lambda (tst)
    (list 'let (list (list '__asres tst))
          (list 'or '__asres
                (list 'assertion-violation
                      #f
                      "Assertion failed"
                      (list 'quote tst)
                      '__asres)))))

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

(define map
  (lambda (func list1 . more-lists)
    (define some?
      (lambda (func list)
        ;; returns #f if (func x) returns #t for some x in the list
        (and (pair? list)
             (or (func (car list))
                 (some? func (cdr list))))))
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

(define (filter proc l)
  (if (null? l)
      '()
      (if (proc (car l))
          (cons (car l) (filter proc (cdr l)))
          (filter proc (cdr l)))))

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

(assert (eq? (append) '()))
(assert (equal? (append '(a b) '(c d)) '(a b c d)))

;; parallel-binding "let"
(define-macro let
  (lambda (forms . body)
    (cons (append (cons 'lambda (list (map car forms)))
                  body)
          (map cadr forms))))

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

(define (equal? a b)
  (if (pair? a)
      (and (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (eq? a b)))

(assert (equal? '(a (b c)) '(a (b c))))

(define (even? n)
  (= 0 (mod n 2)))

;; non-standard
(define (print l . r)
  (display l)
  (if (pair? r)
      (apply print r)))

;; non-standard
(define (println l . r)
  (apply print l r)
  (newline))

