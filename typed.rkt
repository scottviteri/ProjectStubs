#lang typed/racket

(struct pt ([x : Real] [y : Real]))

(: distance (→ pt pt Real))
(define (distance p1 p2) 1)

(struct leaf ([val : Number]) #:transparent)
(struct node ([left : Tree] [right : Tree]) #:transparent)
(define-type Tree (U leaf node))

(: tree-sum (-> Tree Number))
(define (tree-sum t)
  (cond [(leaf? t) (leaf-val t)]
        [else (+ (tree-sum (node-left t))
                 (tree-sum (node-right t)))]))

(define x : Number 7)
(define (id [z : Number]) : Number z)

(let ()
  (: y Number)
  (define y 7)
  (add1 y))
(let ([x : Number 10]) (add1 x))
((λ ([x : Number] [y : String]) (+ x 5)) 3 "hi")

(define-type BinaryTree (U Number (Pair BinaryTree BinaryTree)))
(define z : BinaryTree (cons 1 2))

(: list-string-length (-> (Listof Any) Integer))
(define (list-string-length l)
  (if (null? l)
      0
      (add1 (list-string-length (cdr l)))))

(struct Expression () #:transparent)
(struct Expression-Integer Expression
  ([integer : Integer])
  #:transparent)

(struct Addition Expression
  ([operand/left : Expression]
   [operand/right : Expression])
  #:transparent)

(: pretty-print/non-extensible (-> Expression String))
(define/match (pretty-print/non-extensible expression)
  [((Expression-Integer integer)) (~a integer)]
  [((Addition operand/left operand/right))
   (~a "(" (pretty-print/non-extensible operand/left) "+"
       (pretty-print/non-extensible operand/right) ")")])

(struct Subtraction Expression
  ([operand/left : Expression]
   [operand/right : Expression])
  #:transparent)

(: pretty-print/extended (-> Expression String))
(define/match (pretty-print/extended expression)
  [((Subtraction operand/left operand/right))
   (~a "(" (pretty-print/extended operand/left) "-"
       (pretty-print/extended operand/right) ")")]
  [(k) (pretty-print/non-extensible expression)])
