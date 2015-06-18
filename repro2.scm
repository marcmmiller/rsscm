
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

(define (even? n)
  (= 0 (mod n 2)))

(define (exists pred . lists)
  (if (some? null? lists)
      ;; TODO per spec, base case needs to return last result from pred
      #f ;; TODO: error when all lists aren't the same length
      (or (apply pred (map car lists))
          (apply exists pred (map cdr lists)))))

(exists even? '(3 1 1 5 9))
