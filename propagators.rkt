#lang racket

; how to maintain state with closures?
(define (make-agent)
  (let ([x 2])
    (define (f1) (set! x (+ x 1)) x)
    (define (f2) (set! x (+ x 2)) x)
    (define (me message) (if (= message 1) f1 f2))
    me))
; this is a great programming construct!
;  can choose between getters and setters by passing a message
; fundamentally exposing interface to internal methods by returning an
;  interface to call internals

(define (f')
  (let ([x 2])
    (define (f1) (set! x (+ x 1)) x)
    (define (f2) (set! x (+ x 2)) x)
    (define (me message) (if (= message 1) (f1) (f2)))
    me))
; so how is this different than classes?
;  could just put a macro around passing the message as the method name
;  and contains internal state

; can do overloading with optional arguments
; whatever happens when first evaluating fxn is initialization
;  or pass state as args

; could make inheritance and such
; animal can make-noise, dog can bark
;  so child can do anything that a parent can do by default, and replace methods

(define (parent x)
  (define (f1) (set! x (+ x 1)) x)
  (define (f2) (set! x (+ x 1)) x)
  (define (me message) (parameterize ([current-namespace (current-namespace)])
                         (eval message)))
  ; doesn't work because eval is defined with lexical scope
  ; maybe can pass in some kind of namespace
  me)

; bleh I am not sure how to do something like inheritance
;(define (inherit parent)
;  (define me parent)
;  (define (child x) (parameterize ([x x]) me))
;  child)

(define (inherit parent new-methods)
  (define (child x) (parent x))
  child)

; can I make two agents play tug-of-war with shared state?
; maybe with recursive definition

(define-syntax self-as-string
  (lambda (stx) (datum->syntax stx (format "~s" (syntax->datum stx)))))

(self-as-string (+ 1 2))

((lambda (stx) (datum->syntax stx (format "~s" (syntax->datum stx))))
 #'(+ 1 2))

(define test-f (lambda (stx) (datum->syntax stx (format "~s" (syntax->datum stx)))))
; so if you pass something to a lambda it must be a syntax object
; define-syntax changes evaluation order and makes rhs a syntax object
;  before applying

; macros are all about delaying argument evaluation, and adding source information
;  namely source code location and bindings
; syntax-case allows using pieces of source to manipulate syntax object
;  why can't just use match statement? I guess just can't
; so syntax-case is match

(syntax->datum
 (syntax-case #'(+ 1 2) ()
   [(op n1 n2) #'(- n1 n2)]))

;(define-syntax (swap stx)
;  (syntax-case stx ()
;    [(swap a b) #'(cons b a)]))

;(syntax->datum (syntax-case #'(cons 1 2) () [(cons a b) #'(cons b a)]))
;'(cons 2 1)

;(define-syntax (swap stx)
;  (syntax-case stx ()
;    [(swap x y) #'(let ([tmp x]) (set! x y) (set! y tmp))]))


; abort early
(define (find-multiple factor)
  (let/cc return
    (for ([i (in-range 100)])
      (let ([num (random 2000)])
        ;(display num) (display " ")
        (when (zero? (modulo num factor))
          (return num))))))

; can learn how to make macros with Beautiful Racket
;  or simple languages with reference

; how to make simple propagators?

(define l (box 1)) (define r (box #f)) (define out (box #f))
; boxed values
(define (add l r out) ; l,r,out are boxes
  (let ([lc (unbox l)] [rc (unbox r)])
    (if (and lc rc) (begin (set-box! out (+ lc rc)) #t) #f)))

(define (l-and x y) (and x y))
(define all (curry foldl l-and #t))

; should I do this with stateful boxes or with local variables
(define (f ins outs))
;(define (add2 ins outs)
;  (if (all (map unbox ins))
;      ()))
