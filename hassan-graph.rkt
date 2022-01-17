#lang racket
(require graph)
(require graph graphviz)


(define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))
(define (choose n k) (/ (fact n) (* (fact (- n k)) (fact k))))

(define (random-graph n)
  (letrec ([verts (range n)]
           [pairs (filter (lambda (x) (not (eq? (car x) (cadr x))))
                          (cartesian-product verts verts))])
    (map cons
         (build-list (length pairs) (lambda _ (random)))
         pairs)))

(define g (weighted-graph/undirected (random-graph 3)))

;(define (strat1 n graph)
;  ; send a node to the most central
;  ())

;(define (gen prev-graph)
;  ())

(define (runif) (- (* 2 (random)) 1))
;(define (runif) (+ (runif) (runif) (runif) (runif)))
(define xs0 (build-list 1000 (λ _ (runif))))
(define ys0 (build-list 1000 (λ _ (runif))))
(define zs0 (build-list 1000 (λ _ (runif))))
(define mags (map (λ (x y z) (sqrt (+ (sqr x) (sqr y) (sqr z))))
                  xs0 ys0 zs0))
(define xs (map / xs0 mags))
(define ys (map / ys0 mags))
(define zs (map / zs0 mags))

;(define snip (plot-snip (function sin) #:x-min -5 #:x-max 5))

;(define (plot-graph))

(define f (open-output-file "hi.txt" #:exists 'replace))
(graphviz g #:output f)
(close-output-port f)
