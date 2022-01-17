#lang racket
(require racket/hash)
(require relation/function)
(require rebellion)

(define (lift f) (partial hash-union #:combine f))

(let ([t₁ (hash (list 0 0) 1 (list 1 1) 1)]
      [t₂ (hash (list 0 1) 1 (list 1 1) 1)])
  ((lift +) t₁ t₂))

(list 0 2) (list 0 1) (list 1 2)
(define (create-op lsts)
  1)

(define (m-mul m₁ m₂)
  ; ij,jk->ik
  (multiset-remove
   (for*/multiset ([k₁ (in-multiset m₁)] [k₂ (in-multiset m₂)])
     (if (= (cadr k₁) (car k₂)) (list (car k₁) (cadr k₂)) 'miss))
   'miss #:copies +inf.0))

(let ([m₁ (multiset (list 0 0) (list 0 0) (list 1 1))]
      [m₂ (multiset (list 0 0) (list 1 1) (list 1 1))])
  (m-mul m₁ m₂))

; now want to make the multiset operation more generic
; ij-> sum values of A
;(define (m-sum m) (transduce (in-multiset m) (mapping (const 1)) #:into into-sum))
(define m-sum multiset-size)
(define (transpose m)
  (transduce (in-multiset m)
             (mapping (match-lambda [(list x y) (list y x)]))
             #:into into-multiset))
(define (diag m)
  (transduce (in-multiset m)
             (filtering (match-lambda [(list x x) #t] [else #f]))
             #:into into-multiset))
(define (trace m)
  (transduce (in-multiset m)
             (filtering (match-lambda [(list x x) #t] [else #f]))
             #:into into-count))
(define (sum-axis m i)
  (transduce (in-multiset m)
             (batching (filtering ))
             (filtering (match-lambda [(list x y) ]))))

(define equivs
  (make-reducer
   #:starter (lambda () (variant #:consume false))
   #:consumer
   #:emitter
   #:half-claser
   #:half-closed-emitter
   #:finisher (λ (x) x)
   ))
