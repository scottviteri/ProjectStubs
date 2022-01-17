#lang racket
(require racket/gui)
(require plot)

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
;(plot3d-snip (points3d (map vector xs ys zs) #:sym 'dot) #:altitude 25)

(define (fx a b) (/ 2 (* a (+ 1 (sqr (/ b a)) (sqr a)))))
(define (fy a b) (/ (* 2 b) (+ (sqr a) (sqr b) (expt a 4))))
(define (fz a b) (/ 2 (+ (sqr a) (sqr b) (expt a 4))))

;(plot3d-frame (parametric-surface3d (lambda (t1 t2) (list (fx t1 t2)
;                                                    (fy t1 t2)
;                                                    (fz t1 t2)))
;                              0.1 1 0.1 1))

;(plot3d-snip (polar3d (lambda (t1 t2) t1)) #:altitude 25)

;(send pb insert (plot3d-frame (parametric-surface3d (lambda (t1 t2) (list (fx t1 t2)
;                                                                        (fy t1 t2)
;                                                                        (fz t1 t2)))
;                                                   0.1 1 0.1 1)))

(define snip (plot-snip (function sin) #:x-min -5 #:x-max 5))
; (define snip (plot3d-snip (points3d (map vector xs ys zs) #:sym 'dot) #:altitude 25))
(define (mouse-callback snip event x y)
   (if (and x y)
       (send snip set-overlay-renderers
             (list (vrule x)
                   (point-label (vector x (sin x)))))
       (send snip set-overlay-renderers #f)))
 (send snip set-mouse-event-callback mouse-callback)

; hmm snip has its own event handler
; what does def/public do ∈ class
(define (change-snip pasteboard)
  ;get snip from pasteboard
  (define snip (send pasteboard find-first-snip))
  (send pasteboard remove snip)
  ; want to get content from snip
  (send pasteboard insert)
  ; step it
  ; send pasteboard snip to that
  ; find insert
  )
(define (plot-circle n)
  (plot-snip (points2d)))

(define f (new frame% [label "simple edit"] [width 200] [height 200]))
(define c (new editor-canvas% [parent f]))
(define pb (new pasteboard%))
(send c set-editor pb)
(send pb insert (plot3d-snip (points3d (map vector xs ys zs) #:sym 'dot) #:altitude 25))
(send f show #t)
