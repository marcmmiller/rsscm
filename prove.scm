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
;; Helper functions for axiom manipulation.
;;------------------------------------------------------------------------------

;;
;; Copy tree t with a substituted for b, using eq? as a test.
;;
(define (subst a b t)
  (cond ((null? t) t)
        ((pair? t) (cons (subst a b (car t))
                         (subst a b (cdr t))))
        ((eq? t a) b)
        (#t t)))

(assert (equal? (subst 'a 'b '((a c a) a ((b a c))))
                '((b c b) b ((b b c)))))


;;
;; Removes optional negation from a  primitive clause.
;;
(define (get-pred clause)
  (if (eq? (car clause) 'not)
      (cadr clause)
      clause))

;;
;; Return the name of the predicate, which can be negated.
;;
(define (pred-name clause)
  (car (get-pred clause)))

;;
;; Return the "object" of the predicate, which can be negated.
;;
(define (pred-obj clause)
  (cadr (get-pred clause)))

(define (negated? clause)
  (eq? (car clause) 'not))

;;
;; Work around not having atom->string conversion (or any string
;; functions)
;;
(define *vars* '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p
                    ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z))

(define (variable? x)
  (memq x *vars*))

(assert (variable? '?x))
(assert (variable? '?y))
(assert (not (variable? 'x)))

;;
;; Returns a list of predicates with optional 'or removed
;;
(define (get-preds ax)
  (if (eq? (car ax) 'or)
      (cdr ax)
      (list ax)))

;;
;; Find the first predicate tha thas pred-name pred in the axiom, or null.
;;
(define (find-pred pred axioms)
  (define (iter ax)
    (cond ((null? ax) '())
          ((eq? (pred-name (car ax)) pred) (car ax))
          (#t (iter (cdr ax)))))
  (iter (get-preds axioms)))


;;------------------------------------------------------------------------------
;; Theorem Prover
;;------------------------------------------------------------------------------


;;
;; Find predicates that match between axioms, where the predicate is negated
;; in axiom 1 but asserted in axiom 2.  These are candidates for resolution.
;;
(define (find-matches a1 a2)
  (let ((matches
         (map (lambda (a1-pred)
                (map (lambda (a2-pred)
                       (if (and (eq? (pred-name a1-pred) (pred-name a2-pred))
                                (or (and (negated? a1-pred)
                                         (not (negated? a2-pred)))
                                    (and (not (negated? a1-pred))
                                         (negated? a2-pred))))
                           (pred-name a1-pred)
                           '()))
                     (get-preds a2)))
              (get-preds a1))))
    (filter (lambda (i) (not (null? i))) (apply append matches))))

(assert (equal? (find-matches '(or (person ?x) (not (man ?x)))
                              '(man Bob))
                '(man)))

(assert (equal? (find-matches '(or (person ?x) (not (man? x)))
                              '(woman Susan))
                '()))


;;
;; Combines two axioms, removing remove-pred, to make a single axiom.
;;
;;  a1 = (or (person Bob) (not (man Bob)))
;;  a2 = (man Bob)
;;  remove-pred = man
;;  ==> (person Bob)
;;
;; TODO: verify whether this logic holds:
;;
;;    (or x (not y)) + (or y z) =?=> (or x z) ????
;;
;; It seems to in all tests.
;;
(define (combine-axioms a1 a2 remove-pred)
  (let ((new-ax (filter (lambda (i) (not (eq? (pred-name i) remove-pred)))
                        (append (get-preds a1) (get-preds a2)))))
    (let ((len (length new-ax)))
      (cond ((= len 0) '())
            ((= len 1) (car new-ax))
            (#t (cons 'or new-ax))))))

(assert (equal? (combine-axioms '(or (person Bob) (not (man Bob))) '(man Bob) 'man)
                '(person Bob)))

(assert (null? (combine-axioms '(man Bob) '(not (man Bob)) 'man)))

(assert (equal? (combine-axioms '(or (crazy Bob) (not (person Bob)) (not (in-sanitarium Bob)))
                                '(person Bob)
                                'person)
                '(or (crazy Bob) (not (in-sanitarium Bob)))))

;;
;; Unify two axioms, finding one match between them and combining into a new
;; theorem.  Returns 'not-unifiable if we cannot unify.  Null means contradiction.
;;
(define (unify a1 a2)
  (let ((matches (find-matches a1 a2)))
    (if (null? matches)
        'not-unifiable
        ;; TODO: process more than one match??
        (let* ((match (car matches))
               (a1pred (find-pred match a1))
               (a2pred (find-pred match a2)))
          (cond ((and (variable? (pred-obj a1pred))
                      (not (variable? (pred-obj a2pred))))
                 (combine-axioms (subst (pred-obj a1pred)
                                        (pred-obj a2pred)
                                        a1)
                                 a2
                                 match))
                ((and (variable? (pred-obj a2pred))
                      (not (variable? (pred-obj a1pred))))
                 (combine-axioms a1
                                 (subst (pred-obj a2pred)
                                        (pred-obj a1pred)
                                        a2)
                                 match))
                ((and (not (variable? (pred-obj a1pred)))
                      (not (variable? (pred-obj a2pred)))
                      (eq? (pred-obj a1pred) (pred-obj a2pred)))
                 (combine-axioms a1 a2 match))
                (#t 'not-unifiable))))))

(assert (equal? (unify '(man Scott) '(not (man Scott))) '()))
(assert (equal? (unify '(or (person ?x) (not (man ?x)))
                       '(man Scott))
                '(person Scott)))
(assert (equal? (unify '(or (foo Bar) (snoo Bar))
                       '(gronk Baz))
                'not-unifiable))

;;
;; Unify one axiom against a set of axioms.  Returning new axioms or 'contradiction.
;;
(define (unify-with ax axioms sofar)
  (cond ((null? axioms)
         sofar)
        ((equal? ax (car axioms))
         (unify-with ax (cdr axioms) sofar))
        (#t
         (let ((u (unify ax (car axioms))))
           (if (null? u)
               'contradiction
               (unify-with ax (cdr axioms)
                           (if (eq? u 'not-unifiable)
                               sofar
                               (cons u sofar))))))))

(define (tr-unify-with ax axioms sofar)
  ;;(println "Unify-with " ax)
  (let ((ret (unify-with ax axioms sofar)))
    ;;(println " --> " ret)
    ret))

(define (unify-all axioms)
  (define (iter axs sofar)
    (if (null? axs)
        sofar
        (let ((new-thms (tr-unify-with (car axs) (cdr axs) '())))
          (if (eq? new-thms 'contradiction)
              'contradiction
              (iter (cdr axs) (append sofar new-thms))))))
  (iter axioms '()))

(define (think axioms)
  (let ((new-thms (unify-all axioms)))
    (cond ((eq? new-thms 'contradiction)
           'contradiction)
          ((null? new-thms) axioms)
          (#t (begin
                (println "New theorems:")
                (map println new-thms)
                (think (append new-thms axioms)))))))

;;------------------------------------------------------------------------------
;; Conjunctive Normal Form normalization routines
;;------------------------------------------------------------------------------

(define (normalize-all axioms)
  (promote-conjunctions (map normalize axioms)))

(define (normalize axiom)
   (normalize-disjunctions
    (remove-universal-quants
     (atomize-negations
      (eliminate-implications axiom)))))

;;
;; Removes conjunctions, promoting them to top-level.
;;
;; ((or a b)        ((or a b)
;;  (and c d)  -->   c
;;  (or e f)         d
;;  (g))               (or e f)
;;                   (g))
;;
(define (promote-conjunctions axioms)
  (if (null? axioms)
      '()
      (if (eq? (caar axioms) 'and)
          (append (cdar axioms)
                  (promote-conjunctions (cdr axioms)))
          (cons (car axioms)
                (promote-conjunctions (cdr axioms))))))


(assert (equal? '((or a b) c d (or e f) (g))
                (promote-conjunctions '((or a b) (and c d) (or e f) (g)))))

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


(println "Axioms: ")

(define normalized-axioms (normalize-all axioms))
(map println normalized-axioms)

;;(println "Theorems: ")
;;(define thms (unify-all normalized-axioms))
;;(map println thms)

(think normalized-axioms)
