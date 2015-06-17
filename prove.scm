;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prove: A resolution refutation theorem prover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import "lib.scm")

(define axioms
  '((forall ?x (-> (and (person ?x) (in-sanitarium ?x)) (crazy ?x)))
    (forall ?x (-> (or (man ?x) (woman ?x)) (person ?x)))
    (man Scott)
    (in-sanitarium Scott)))



;;------------------------------------------------------------------------------
;; Conjunctive Normal Form normalization routines
;;------------------------------------------------------------------------------

(define (normalize axiom)
   (normalize-disjunctions
    (remove-universal-quants
     (atomize-negations
      (eliminate-implications axiom)))))

;;
;; Rewrite (-> a b) as (or (not a) b)
;;
(define (eliminate-implications axiom)
  (if (pair? axiom)
      (if (eq? (car axiom) '->)
          (list 'or
                (list 'not (cadr axiom))
                (caddr axiom))
          (cons (eliminate-implications (car axiom))
                (eliminate-implications (cdr axiom))))
      axiom))

;;
;; Apply DeMorgan's Law and double negation principles recursively
;; until all of the negations are atomized.
;;
(define (atomize-negations axiom)
  (if (pair? axiom)
      (if (eq? (car axiom) 'not)
          (let ((arg (cadr axiom)))
            (cond
             ;; Already literalized.
             ((not (pair? arg)) (list 'not arg))
             ;; DeMorgan's Law (not (or a b)) -> (and ((not a) (not b)))
             ((eq? (car arg) 'or)
              (cons 'and
                    (map (lambda (i) (list 'not i))
                         (atomize-negations (cdr arg)))))
             ;; DeMorgan's Law (not (and a b)) -> (or ((not a) (not b)))
             ((eq? (car arg) 'and)
              (cons 'or
                    (map (lambda (i) (list 'not i))
                         (atomize-negations (cdr arg)))))
             ;; (not (not a)) -> a
             ((eq? (car arg) 'not)
              (atomize-negations (cadr arg)))
             ;; (not a) -> (not a)
             (#t (cons 'not (atomize-negations arg)))))
          (cons (atomize-negations (car axiom))
                (atomize-negations (cdr axiom))))
      axiom))

(assert (equal? (atomize-negations (quote (not (or a b)))) '(and (not a) (not b))))
(assert (equal? (atomize-negations (quote (not (and a b)))) '(or (not a) (not b))))
(assert (equal? (atomize-negations (quote (not (not a)))) 'a))
(assert (equal? (atomize-negations (quote (not a))) '(not a)))

;;
;; Rewrite (forall ?x (stuff)) as (stuff)
;; TODO: support existential quantifiers by creating Skolem functions.
;;
(define (remove-universal-quants axiom)
  (if (pair? axiom)
      (if (eq? (car axiom) 'forall)
          (remove-universal-quants (caddr axiom))
          (cons (remove-universal-quants (car axiom))
                (remove-universal-quants (cdr axiom))))
      axiom))

(define (print l . r)
  (display l)
  (if (pair? r)
      (apply print r)))

(define (println l . r)
  (apply print l r)
  (newline))

(define-macro rd
  (lambda (o)
    (list 'println "(define " (list 'quote o) " '" o ")")))

;;
;; Puts an axiom in conjunctive normal form by recursive application
;; of the following rule:
;;
;; Rewrites:
;;   (or a (and b c)) -> (and (or a b) (or (a c)))
;;   (or a (or b c)) -> (or a b c)
;;   (and a (and b c)) -> (and a b c)
;;
(define (normalize-disjunctions axiom)
  ;; TODO move helpers for this function here once everything is debugged
  (if (pair? axiom)
      (if (eq? (car axiom) 'or)
          ;; TODO move to let* once there is support for that
          (let ((arg-list (map normalize-disjunctions (cdr axiom))))
            (let ((neutral-subs (filter (lambda (i)
                                          (or (not (pair? i))
                                              (and (not (eq? (car i) 'and))
                                                   (not (eq? (car i) 'or)))))
                                        arg-list))
                  (or-subs (map cdr (filter (lambda (i)
                                              (and (pair? i)
                                                   (eq? (car i) 'or)))
                                            arg-list)))
                  (and-subs (map cdr (combine-conjunctions
                                      (filter (lambda (i)
                                                (and (pair? i)
                                                     (eq? (car i) 'and)))
                                              arg-list)))))
              (let ((combined-or (apply append neutral-subs or-subs)))
                (if (pair? and-subs)
                    (let ((permuted (map (lambda (i) (append combined-or i))
                                         (permute and-subs))))
                      (cons 'and (distribute 'or permuted)))
                    (cons 'or combined-or)))))
          (map normalize-disjunctions (combine-conjunctions axiom)))
      axiom))

;; (and a (and b c) d (and e f) g) -> (and a b c d e f g)
(define (combine-conjunctions axiom)
  (if (and (pair? axiom)
           (eq? (car axiom) 'and))
      (let ((simple (filter (lambda (i)
                              (or (and (pair? i)
                                       (not (eq? (car i) 'and)))
                                  (not (pair? i))))
                            (cdr axiom)))
            (iand (map cdr (filter (lambda (i)
                                     (and (pair? i)
                                          (eq? (car i) 'and)))
                                   (cdr axiom)))))
        (cons 'and (apply append (cons simple iand))))
      axiom))


(assert (equal? (combine-conjunctions '(and a (and b c) d (and e f) g))
                '(and a d g b c e f)))


;;
;; Given a list-of-lists, generates a list of unordered permutations.
;;
;; ((a b) (c d)) -> ((a c) (a d) (b c) (b d))
;;
(define (permute lol)
  (if (pair? lol)
      (if (pair? (cdr lol))
          ;; recursive case
          (let ((r (permute (cdr lol))))
            (apply append
                   (map (lambda (i) (distribute i r)) (car lol))))
          ;; base case: only one list in lol
          (map (lambda (i) (list i)) (car lol)))
      lol))

;;
;; Distributes 'thing' across list-of-lists
;;
;;  (thing ((a b) (c d) (e f))) -> ((thing a b) (thing c d) (thing e f))
;;
(define (distribute thing lol)
  (map (lambda (i) (cons thing i)) lol))

(assert (equal? (permute '((a b) (c d)))
                '((a c) (a d) (b c) (b d))))

(assert (equal? (normalize-disjunctions '(or a b (and c d) (and e f)))
                '(and (or a b c e) (or a b c f) (or a b d e) (or a b d f))))

(assert (equal? (normalize-disjunctions '(or a b (and c d) (and e f)))
                '(and (or a b c e) (or a b c f) (or a b d e) (or a b d f))))

(assert (equal? (normalize-disjunctions '(or a b)) '(or a b)))

(assert (equal? (normalize-disjunctions '(or (and a b) c))
                '(and (or c a) (or c b))))

(assert (equal? (normalize-disjunctions '(or a (or b c) (and d e) (or f g) (and h i) j))
                '(and (or a j b c f g d h) (or a j b c f g d i)
                      (or a j b c f g e h) (or a j b c f g e i))))
(assert (equal? (normalize-disjunctions '(or a b (or d c))) '(or a b d c)))

(assert (equal? (normalize-disjunctions '(or a b (and c d) (and e f)))
                '(and (or a b c e) (or a b c f) (or a b d e) (or a b d f))))


;; (normalize-disjunctions '(and (or a b (and c d) (and e f)) g h))


(map println (map normalize axioms))

