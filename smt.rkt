#lang racket
(require relation)
(require (except-in racket/list group-by))

(display 1)

; term sharing? no just accept simple equalities atm

; invariant that (eqs-cls eqs) do not share variables
(define eq-cls (list (set 0 1 2) (set 3 4)))

(define dis-eqs (list (cons 0 1)))
; instead want set intersect between any pair to not wor

;(define (compose2 . fxns) (lambda (x y) (f (g x y))))

;(findf (compose not set-empty? set-intersect))
(define (check-cls cls) (set-empty? (apply set-intersect cls)))
(define (check-deqs cls deqs)
  (empty? (filter (lambda (x) (not (member x (range (length cls)))))
                  (flatten deqs))))

(struct eqs (cls deqs)
  #:transparent
  #:guard
  (lambda (cls deqs name)
    (if (not (check-cls cls)) (error "non-empty equiv-class intersection")
        (if (not (check-deqs cls deqs))
            (error "invalid dis-eqs -- not in range of equiv-classes")
            (values cls deqs)))))
(define (p eqs) (display (eqs-cls eqs)) (newline) (display (eqs-deqs eqs)))

(define (enumerate l) (map cons (range (length l)) l))
(define (findi f l)
  (let ([result (findf (compose f cdr) (enumerate l))])
    (if result (car result) result)))

(define (find-class cls val)
  (findi (app set-member? _ val) cls))

(define (remove-indexes l indexes)
  (let ([new-indexes (filter (compose not (partial set-member? indexes))
                             (range (length l)))])
    (map (partial list-ref l) new-indexes)))

(define (merge cls i1 i2)
  (cons (set-union (list-ref cls i1) (list-ref cls i2))
        (remove-indexes cls (list i1 i2))))

(define inc (partial + 1))
(define (pair-apply f p) (cons (f (car p)) (f (cdr p))))
(define (cons-eqs eqs-struct x)
  (eqs (cons (set x) (eqs-cls eqs-struct))
       (map (partial pair-apply inc) (eqs-deqs eqs-struct))))

(define (opt-add l x)
  (if x (cons (set x) l) l))

; just do in case both are found first
(define (accept-eq a b eqs)
  (letrec ([c1 (find-class (eqs-cls eqs) a)]
           [c2 (find-class (eqs-cls eqs) b)]
           [new-eqs (opt-add (opt-add (eqs-cls eqs) c1) c2)])
    (merge new-eqs c1 c2)))
; unclear how to get
; would be nice to have eqs as a structure
; racket structure
