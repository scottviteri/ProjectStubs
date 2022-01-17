#lang racket
(require relation/function)

; want to iterate over all functions from fn -> fn
; can't do with polynomials naively
; a fxn is a list
(define (take-mod n) (λ (x) (modulo x n)))
(define (lift f n) (map (compose (take-mod n) f) (range n)))
(define apply-f list-ref)
(define (eq-sqrt f n) (equal? (lift (λ (x) (- (* x x) 1)) n) (lift f n)))
(define (comp lst) (map (lambda (i) (list-ref lst (list-ref lst i)))
                        (range (length lst))))

;(define (f x n))
; n-ary product
;(define (product l1 l2) (map (λ (x) (map (λ (y) (cons y x)) l2)) l1))
(define fxns (cartesian-product (range 3) (range 3)))
(define (get-fxns n) (apply cartesian-product (make-list n (range n))))

(define (check n)
  (let ([fxns (get-fxns n)]
        [lst (lift (λ (x) (- (* x x) 1)) n)])
    (ormap (partial equal? lst) (map comp fxns))))
;(findf (λ (x) (equal? (list 5 0 3 2 3 0) (comp x))) (get-fxns 6))
;'(2 3 5 0 1 3)
; so there are solutions ∈ finite fields
