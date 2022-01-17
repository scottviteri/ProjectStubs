#lang racket

(require relation/function racket/random data/collection)

(define alphabet-bottom 32)
(define alphabet-top 127)
(define alphabet-size (- alphabet-top alphabet-bottom))

(define (sequence->vector seq) (vector->immutable-vector (extend #() seq)))

(define char->num (compose (app - _ alphabet-bottom) char->integer))
(define num->char (compose integer->char (curry + alphabet-bottom)))

(define string->nums (curry map char->num))
(define nums->string (compose sequence->string (curry map num->char)))

;(nums->string (string->nums "tsra")) ; "tsra"

(define (autokey-encode primer plaintext)
  (let* ([plainnums (string->nums plaintext)]
         [key (take (length plainnums) (append (string->nums primer) plainnums))]
         [encoded (map (compose (app modulo _ alphabet-size) +) plainnums key)])
    (nums->string encoded)))

(define inverse (compose (app modulo _ alphabet-size) (curry - alphabet-size)))
(define (l- l₁ l₂) (map (app modulo _ alphabet-size) (map - l₁ l₂)))

(define (autokey-decode-2 primer ciphertext)
  (define (autokey-decode-aux key cipher-nums out)
    (if (null? cipher-nums) out
        (let ([next-out (modulo (- (car cipher-nums) (car key)) alphabet-size)])
          (autokey-decode-aux (append (cdr key) (list next-out))
                              (cdr cipher-nums)
                              (cons next-out out)))))
  (nums->string
   (reverse
    (autokey-decode-aux (string->nums primer) (string->nums ciphertext) '()))))

(define (autokey-decode primer ciphertext)
  ((compose nums->string reverse)
   (let loop ([key (string->nums primer)]
              [cipher-nums (string->nums ciphertext)]
              [out '()])
     (if (empty? cipher-nums) out
         (let ([next-out (modulo (- (first cipher-nums) (first key)) alphabet-size)])
           (loop (append (rest key) (list next-out))
                 (rest cipher-nums)
                 (cons next-out out)))))))

(define (rand-string len)
  (nums->string (map (λ (_) (random alphabet-size)) (range len))))

(define public-key "ferbie")
(define secret-key "I ate a large hamburger")
(define msg (autokey-encode public-key secret-key))
(define out (autokey-decode public-key msg))

; perm stuff may be wrong because using not using whole range
; but entering ∈ with nums->string putting to bottom

(define (apply-perm perm elem) (dict-ref perm elem))
(define (apply-perm-to-list perm lst) (map (curry ref perm) lst))

(define (rand-perm size)
  (sort (sequence->list (range size))
        (λ (x y) (eq? 0 (random 2)))))

(define perm (rand-perm alphabet-size))

(apply-perm-to-list perm (range alphabet-size))

(define (perm-encode perm str)
  (nums->string (apply-perm-to-list perm (string->nums str))))

(define (reverse-perm perm)
  (sequence->vector (map (curry index-of perm) (range (length perm)))))

(define (compose-perms p₁ p₂)
  (apply-perm-to-list p₁ (apply-perm-to-list p₂ (range (length p₁)))))

(define (perm-decode perm encoded) (perm-encode (reverse-perm perm) encoded))

(perm-decode perm (perm-encode perm "hello"))

(define (constant-shift-perm k)
  (build-sequence alphabet-size (λ (i) (modulo (+ k i) alphabet-size))))

(let ([perm (constant-shift-perm 5)])
  (perm-decode perm (perm-encode perm "hello")))

(define in (open-input-file "hi3.txt"))
(define instr (read-string 100 in))
(define words (string-split instr " "))

; get indiv letter distribution to break
; are there colletion generics like size?
