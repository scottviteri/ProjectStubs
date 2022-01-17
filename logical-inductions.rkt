
(require racket/math plot)
(require racket/list srfi/1)

(plot (function sin (- pi) pi))

(plot (list (function (lambda (n) (/ 1 (expt n n))) 1 5
                      #:label "P(all x, p(x) =n q(x))" #:style 'dot)
            (function (lambda (n) (- 1 (expt (- 1 (/ 1 n)) n))) 1 5
                      #:label "P(exists x, p(x) =n q(x))")))

(define (prob-forall n k) (/ 1 (expt n (- n k))))

(define (prob-exists n k) (- 1 (expt (- 1 (/ 1 n)) (- n k))))

(plot (list (function (lambda (k) (prob-forall 3 k)) 0 3
                      #:label "P(all x, p(x) =n q(x))" #:style 'dot)
            (function (lambda (k) (prob-exists 3 k)) 0 3
                      #:label "P(exists x, p(x) =n q(x))")))

(plot (list (function (lambda (k) (prob-forall 6 k)) 0 6
                      #:label "P(all x, p(x) =n q(x))" #:style 'dot)
            (function (lambda (k) (prob-exists 6 k)) 0 6
                      #:label "P(exists x, p(x) =n q(x))")))


(define coeffs (cartesian-product (iota 4) (iota 4) (iota 4) (iota 4) (iota 4)
                                  (iota 4) (iota 4) (iota 4)))

(define (coeffs-to-poly coeffs)
  (lambda (x) (if (= 1 (length coeffs)) (car coeffs)
                  (+ (car coeffs) (* x ((coeffs-to-poly (cdr coeffs)) x))))))


(filter (lambda (x) (equal? '(1 0 0 0) x))
        (map (lambda (coeffs)
               (map (lambda (x) (modulo ((coeffs-to-poly coeffs) x) 4)) (iota 4)))
             coeffs))
