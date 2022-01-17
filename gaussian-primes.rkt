#lang racket
(require racket/plot racket/math)

(require srfi/1)

(require (only-in racket/list cartesian-product flatten))

(plot (function sin (- pi) pi #:label "hi"))

(parameterize ([plot-width    150]
                [plot-height   150]
                [plot-x-label  #f]
                [plot-y-label  #f])
   (define xs (build-list 20 (λ _ (random))))
   (define ys (build-list 20 (λ _ (random))))
   (plot (points (map vector xs ys))))

(define (prod l1 l2)
  (concatenate (map (lambda (x) (map (lambda (y) (cons x y)) l2)) l1)))

(define (mult p1 p2)
  (let ([a (car p1)]
        [b (cdr p1)]
        [c (car p2)]
        [d (cdr p2)])
    (cons (- (* a c) (* b d))
          (+ (* a d) (* b c)))))

(define (is-unit c)
  (or (equal? (cons 1 0) c)
      (equal? (cons 0 1) c)
      (equal? (cons -1 0) c)
      (equal? (cons 0 -1) c)))

(define (is-prime p)
  (letrec ([m (max (abs (car p)) (abs (cdr p)))]
           [rng (iota (+ 1 (* 2 m)) (* -1 m))]
           [pts (prod rng rng)]
           [pairs (prod pts pts)])
    (every (lambda (pair)
             (or
              (is-unit (car pair))
              (is-unit (cdr pair))
              (not (equal? p (mult (car pair) (cdr pair))))))
           pairs)))

(define (get-primes n)
  (letrec ([rng (iota (+ 1 (* 2 n)) (* -1 n))]
           [pts (prod rng rng)])
    (filter is-prime pts)))

(define (plot-primes n)
  (plot (points (map (lambda (p) (vector (car p) (cdr p)))
                     (get-primes n)))))
