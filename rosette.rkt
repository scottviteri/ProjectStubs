#lang rosette

;(require racket/base)

;(require rosette/lib/synthax)

(display 1)

(define int32? (bitvector 32))
(define (int32 i)
  (bv i int32?))

(define (bvmid lo hi)  ; (lo + hi) / 2
  (bvsdiv (bvadd lo hi) (int32 2)))

(define (check-mid impl lo hi)     ; Assuming that
  (assume (bvsle (int32 0) lo))    ; 0 ≤ lo and
  (assume (bvsle lo hi))           ; lo ≤ hi,
  (define mi (impl lo hi))         ; and letting mi = impl(lo, hi) and
  (define diff                     ; diff = (hi - mi) - (mi - lo),
    (bvsub (bvsub hi mi)
           (bvsub mi lo)))         ; we require that
  (assert (bvsle lo mi))           ; lo ≤ mi,
  (assert (bvsle mi hi))           ; mi ≤ hi,
  (assert (bvsle (int32 0) diff))  ; 0 ≤ diff, and
  (assert (bvsle diff (int32 1)))) ; diff ≤ 1.

(define (bvmid-no-overflow lo hi)
 (bvadd lo (bvsdiv (bvsub hi lo) (int32 2))))
(define-symbolic l h int32?)
(define cex (verify (check-mid bvmid l h)))
(define cl (evaluate l cex))
(define ch (evaluate h cex))

(verify (check-mid bvmid-no-overflow l h))

(require rosette/lib/synthax)

(define-grammar (fast-int32 x y)  ; Grammar of int32 expressions over two inputs:
  [expr
   (choose x y (?? int32?)        ; <expr> := x | y | <32-bit integer constant> |
           ((bop) (expr) (expr))  ;           (<bop> <expr> <expr>) |
           ((uop) (expr)))]       ;           (<uop> <expr>)
  [bop
   (choose bvadd bvsub bvand      ; <bop>  := bvadd  | bvsub | bvand |
           bvor bvxor bvshl       ;           bvor   | bvxor | bvshl |
           bvlshr bvashr)]        ;           bvlshr | bvashr
  [uop
   (choose bvneg bvnot)])         ; <uop>  := bvneg | bvnot

(define (bvmid-fast-1 lo hi)
  (fast-int32 lo hi #:depth 2))

(define sol
   (synthesize
    #:forall    (list l h)
    #:guarantee (check-mid bvmid-fast-1 l h)))

(define (bvmid-fast lo hi)
  (bvlshr (bvadd hi lo) (bv 1 32)))

(define sol2
    (solve
     (begin
       (assume (not (equal? l h)))
       (assume (bvsle (int32 0) l))
       (assume (bvsle l h))
       (assert (equal? (bvand l h) (bvmid-fast l h))))))

(define (bvmid-and? lo hi)
  (fast-int32 lo hi #:depth 2))

(define sol3
   (synthesize
    #:forall (list l h)
    #:guarantee
    (begin
       (assume (not (equal? l h)))
       (assume (bvsle (int32 0) l))
       (assume (bvsle l h))
       (assert (<=> (bvmid-and? l h)
                    (equal? (bvand l h) (bvmid-fast l h)))))))

(define (check-mid-slow impl lo hi)      ; Assuming that
  (assume (bvsle (int32 0) lo))          ; 0 ≤ lo and
  (assume (bvsle lo hi))                 ; lo ≤ hi,
  (assert                                ; we require that
   (equal?
    (bitvector->integer (impl lo hi))    ; ⌈impl(lo, hi)⌉ =
    (quotient                            ; (⌈lo⌉ + ⌈hi⌉) / 2, where
     (+ (bitvector->integer lo)          ; ⌈e⌉ stands for the mathematical
        (bitvector->integer hi))         ; integer corresponding to the
     2))))                               ; 32-bit integer e.

;(time (verify (check-mid bvmid l h)))
;(time (verify (check-mid-slow bvmid l h)))

(define fuel (make-parameter 5))

(define-syntax-rule
  (define-bounded (id param ...) body ...)
  (define (id param ...)
    (assert (> (fuel) 0) "Out of fuel.")
    (parameterize ([fuel (sub1 (fuel))])
      body ...)))

(define-bounded (bvsqrt n)
  (cond
    [(bvult n (int32 2)) n]
    [else
     (define s0 (bvshl (bvsqrt (bvlshr n (int32 2))) (int32 1)))
     (define s1 (bvadd s0 (int32 1)))
     (if (bvugt (bvmul s1 s1) n) s0 s1)]))

; Correctness specification for bvsqrt:
(define (check-sqrt impl n)
  (assume (bvsle (int32 0) n))          ; Assuming n ≥ 0,
  (define √n (impl l))
  (define √n+1 (bvadd √n (int32 1)))    ; we require that
  (assert (bvule (bvmul √n √n) n))      ; (√n)^2 ≤ n and
  (assert (bvult n (bvmul √n+1 √n+1)))) ; n < (√n + 1)^2.

(clear-vc!)
;(current-bitwidth #f)
(fuel 16)
(define cex2 (verify (check-sqrt bvsqrt l)))

;(define n (evaluate l cex2))


;(bitvector->integer (bvsqrt (evaluate l cex2)))
; 41024
; getting a counterexamble?
; doesn't seem like it is actually a counterexample?
;  even with a higher fuel?
; getting mixed results. Issue with parameterize?
