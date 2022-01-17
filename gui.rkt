#lang racket
(require racket/gui/base)
(require plot)

(define my-editor-canvas%
  (class editor-canvas%
    (define/override (on-event event)
      (if (equal? 'left-down (send event get-event-type))
          (display (send event get-x))
          (send status-message set-label "Canvas mouse")))
    (super-new)))

(define nil '())
(define application-frame
  (new frame% [label "Example"] [width 400] [height 300]))
(define cv (new my-editor-canvas% [parent application-frame]))
(define pb (new pasteboard%))
(send cv set-editor pb)
(define snip (plot-snip (function (λ (x) x) 1 2)))
(send pb insert snip)

(define status-message
  (new message%
    [parent application-frame]
    [label "No events so far..."]
   	[auto-resize #t]))


(send application-frame show #t)
