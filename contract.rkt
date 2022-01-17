#lang racket

(require racket/contract)
(define/contract (our-div num denom)
  (number? (and/c number? (not/c zero?)) . -> . number?)
  (/ num denom))

(module divs racket
  (require racket/contract)
  (provide
   (contract-out
    [/ (integer? integer? . -> . integer?)]))
  (define (int-div num denom)
    (/ num denom))
  (with-handlers ([exn:fail? (λ (e) 'inner-breach)])
    (displayln (int-div 42 2.5))))

(require (submod "." divs))

(with-handlers ([exn:fail? (λ (e) 'outer-breach)])
  (displayln (/ 42 2.5)))
