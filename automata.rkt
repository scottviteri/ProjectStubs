#lang racket
(require plot)
(require racket/generator)
(require (except-in racket/gui/base yield))
(require (except-in relation/function function))


;(define game (set (cons 1 2) (cons 2 3)))
(define inc (curry + 1))
(define dec (app - _ 1))
(define (list->cons l) (cons (car l) (cadr l)))
(define (cons->list p) (list (car p) (cdr p)))
(define (all-pairs rng-x rng-y) (map list->cons (cartesian-product rng-x rng-y)))

(define (pair+ p₁ p₂) (cons (+ (car p₁) (car p₂)) (+ (cdr p₁) (cdr p₂))))

(define (neighbors game p)
  (remove p (filter (curry set-member? game)
                    (map (curry pair+ p)
                         (all-pairs (range -1 2) (range -1 2))))))

(define (all-inds lower-x upper-x lower-y upper-y)
  (all-pairs (range lower-x (inc upper-x)) (range lower-y (inc upper-y))))

(define (get-lower-x G) (foldl min (car (set-first G)) (set-map G car)))
(define (get-upper-x G) (foldl max (car (set-first G)) (set-map G car)))
(define (get-lower-y G) (foldl min (cdr (set-first G)) (set-map G cdr)))

(define (get-upper-y G) (foldl max (cdr (set-first G)) (set-map G cdr)))

(define (step G)
  (if (set-empty? G) (set)
   (apply set (filter-map (λ (p) (let ([num-neighbors (length (neighbors G p))]
                                       [alive (set-member? G p)])
                               (if (if alive
                                       (set-member? (set 2 3) num-neighbors)
                                       (= num-neighbors 3)) p #f)))
                      (let ([lower-x (get-lower-x G)]
                            [upper-x (get-upper-x G)]
                            [lower-y (get-lower-y G)]
                            [upper-y (get-upper-y G)])
                           (all-inds (dec lower-x) (inc upper-x)
                                     (dec lower-y) (inc upper-y)))))))

(define (steps n G) (if (= n 0) G (steps (dec n) (step G))))

(define (block x y) (set (cons x y) (cons x (inc y))
                         (cons (inc x) y) (cons (inc x) (inc y))))

(equal? (block 2 3) (step (block 2 3))) ;#t

(define (blinker x y) (set (cons x y) (cons x (inc y)) (cons x (inc (inc y)))))
(define (glider x y) (set (cons x y) (cons (inc x) y) (cons (inc (inc x)) y)
                          (cons (inc (inc x)) (inc y)) (cons (inc x) (inc (inc y)))))

(let ([b (blinker 0 0)])
  (and (not (equal? b (step b))) (equal? b (step (step b))))) ;#t

(define (grid lower-x upper-x lower-y upper-y)
  (map (λ (x) (lines x))
       (append (map (λ (y) (list (list lower-x y) (list upper-x y)))
                    (range lower-y (inc upper-y)))
               (map (λ (x) (list (list x lower-y) (list x upper-y)))
                    (range lower-x (inc upper-x))))))

(define (deep-add c lst)
  (if (list? lst) (map (curry deep-add c) lst) (+ c lst)))

(plot (cons
       (points #:sym 'square #:x-min 0 #:x-max 5 #:y-min 0
               #:y-max 5 #:size 30 #:line-width 40
               (deep-add 0.5 (list (list 1 2) (list 3 4))))
       (grid 1 5 0 5)))

(define (plot-automata G)
  (let* ([lower-x (get-lower-x G)] [upper-x (get-upper-x G)]
         [lower-y (get-lower-y G)] [upper-y (get-upper-y G)]
         [side-length (- upper-x lower-x)])
    (plot (list
           (points #:sym 'square
                   #:x-min lower-x #:x-max (inc upper-x)
                   #:y-min lower-y #:y-max (inc upper-y)
                   (deep-add 0.5 (map cons->list (set->list G))))
           (grid lower-x (inc upper-x) lower-y (inc upper-y))))))

(define (plot-automata-fixed G lower-x upper-x lower-y upper-y)
  ;(plot-snip (automata-fixed (blinker 0 -1) -2 2 -2 2))
  ;(plot-snip (automata-fixed (step (blinker 0 -1)) -2 2 -2 2))
  (let ([side-length (- upper-x lower-x)])
    (plot-snip
     (list
      (points #:sym 'square
              #:x-min lower-x #:x-max (inc upper-x)
              #:y-min lower-y #:y-max (inc upper-y)
              (deep-add 0.5 (map cons->list (set->list G))))
      (grid lower-x (inc upper-x) lower-y (inc upper-y))))))

; also do this with torus shape

(define (create-random-config lower-x upper-x lower-y upper-y)
  (apply set (filter (λ (_) (= 0 (random 2)))
                     (all-inds lower-x upper-x lower-y upper-y) )))

;(define game (blinker 0 -1))
;(define game (glider 0 0))
(define game (create-random-config -10 10 -10 10))

#;
(define g
  (generator () (let loop ([x game])
                  (begin (yield x)
                         (loop (step x))))))

; faster if not on a timer, but rather just loop and do when done
#;
(define (loop)
  (send canvas on-paint)
  (set! x (remainder (+ x 1) 500))
  (sleep/yield 0.02)
  (loop))

(define my-pasteboard%
  (class pasteboard%
    (init-field current-game)
    (init-field current-snip)
    (init-field [step-count 0])
    (define/public (update)
      (let ([replot? (eq? 0 step-count)]
            [next-game (step current-game)])
        (begin (if replot?
                   (begin
                     (send this delete current-snip)
                     (set! current-snip
                           (plot-automata-fixed next-game -10 10 -10 10))
                     (send this insert current-snip 0 0)) #f)
               (set! current-game next-game)
               (set! step-count (modulo (inc step-count) 10)))))
    #;
    (define (on-event event)
      (cond [(equal? 'left-down (send event get-event-type))
             (begin (send this delete current-snip)
                    (set! current-game (step current-game))
                    (set! current-snip (plot-automata-fixed current-game -2 2 -2 2))
                    (send this insert current-snip 0 0))]))
    (super-new)))

(define nil '())
(define frame (new frame% [label "Example"]))
(define cv (new editor-canvas% [parent frame]))

(define pb (new my-pasteboard%
                [current-game game]
                [current-snip (plot-automata-fixed game -10 10 -10 10)]
                ;[current-snip (plot-automata game)]
                ))

(new timer% [interval 10] [notify-callback (λ () (send pb update))])

(send cv set-editor pb)
(send frame show #t)

(for/fold
    {[x 0]}
    {[str (in-list '("12" "characters"))]}
  (define n (string-length str))
  (+ x n))

(foldl + 0 (map string-length (list "There")))
