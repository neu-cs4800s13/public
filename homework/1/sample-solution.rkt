#lang racket

(require
  rackunit
  "../../software/cs4800.rkt"
  "../../software/big-o.rkt")

;; A Nested-List is either:
;; - empty, or
;; - (cons Nested-Elem Nested-List)

;; A Nested-Elem is either:
;; - Number, or
;; - Nested-List

;; flatten-lists : Nested-List -> (Listof Number)
;; Produces a list containing every number in nl.
(define/cost (flatten-lists nl)
  (flatten-lists-and-append nl empty))

;; flatten-lists-and-append : Nested-List (Listof Number) -> (Listof Number)
;; Produces a list containing every number in nl added to tail.
(define/cost (flatten-lists-and-append nl tail)
  (cond
    [(empty? nl) tail]
    [else
     (flatten-elem-and-append (first nl)
       (flatten-lists-and-append (rest nl) tail))]))

;; flatten-elem-and-append : Nested-Elem (Listof Number) -> (Listof Number)
;; Produces a list containing every number in ne added to tail.
(define/cost (flatten-elem-and-append ne tail)
  (cond
    [(numeric? ne) (cons ne tail)]
    [else (flatten-lists-and-append ne tail)]))

;; I forgot to allow number?, so we have to identify numbers by elimination:
(define/cost (numeric? x)
  (cond
    [(empty? x) #false]
    [(cons? x) #false]
    [else #true]))

(check-equal? (flatten-lists (list)) (list))
(check-equal? (flatten-lists (list 2 3)) (list 2 3))
(check-equal? (flatten-lists (list 1 (list 2 3) 4)) (list 1 2 3 4))
(check-equal?
  (flatten-lists
    (list
      (list 1 (list 2 3) 4)
      (list 5 (list 6 7) 8)))
  (list 1 2 3 4 5 6 7 8))

(define simple
  (cost-model
    empty? 1
    cons? 1
    cons 1
    first 1
    rest 1))

(define (nested-list-of-size n)
  (cond
    [(<= n 1) empty]
    [else
     (define left (random n))
     (define right (- (sub1 n) left))
     (cons (nested-elem-of-size left)
       (nested-list-of-size right))]))

(define (nested-elem-of-size n)
  (cond
    [(<= n 1) (random-number)]
    [else (nested-list-of-size (sub1 n))]))

(define (cost-of-flatten n)
  (define x (nested-list-of-size n))
  (cost-of #:model simple
    (flatten-lists x)))

(define (linear n) n)

(check-equal? (O? cost-of-flatten linear 10 1 random-number) #true)
