#lang racket
(require rebellion/collection/multiset)
(require relation/function)

(module+ test
  (require rackunit)
  (check-true #t))

;(define (ga ))
; a pure term is a list of nat and a coeff: 121 means e₁e₂e₁
; choice of list or set depending on pushing into operations
;  no has to be list
(define (reduce l)
  (match l
    ['() '()]))
; what is relation to multinomial repr -- same on outer level
; what about indiv terms without coeffs? -- all pows are 0 or 1
;  but also order matters
; so multiset of List Nat
; do I need Nat? Will not use addition -- just need distinct things
;  yeah but won't hurt either
; datatypes can be used so that equality is just the naive version
;  but really want an equality function that is complete

;(define (pure= v₁ v₂) ()) ; need to use reduce ∈here for completeness
;(define example (multiset (list 1 2 2) (list 1 2 2)))

;(define (f y) q)

; this brought up core confusions about datatypes
;  settled on using assoc list of fixed length boolean vectors to coeffs
;  xor is multiplication?
