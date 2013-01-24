#lang racket

(require rackunit "../../software/cs4800.rkt" "../../software/big-o.rkt")

;; isort-list : (Listof Number) -> (Listof Number)
;; Produces a list containing the elements of xs in ascending order.
(define/cost (isort-list xs)
  (cond
    [(empty? xs) empty]
    [else (insert-list (first xs)
            (isort-list (rest xs)))]))

;; insert-list : Number (Listof Number) -> (Listof Number)
;; Produces a list containing x inserted in the proper location among the
;; elements of xs, which must be in ascending order.
(define/cost (insert-list x xs)
  (cond
    [(empty? xs) (cons x empty)]
    [else
     (define y (first xs))
     (cond
       [(<= x y) (cons x xs)]
       [else (cons y (insert-list x (rest xs)))])]))

;; isort-vector : (Vectorof Number) -> Void
;; Rearranges the elements of xs in ascending order.
(define/cost (isort-vector xs)
  (isort-vector-from xs 0))

;; isort-vector-from : (Vectorof Number) Number -> Void
;; Rearranges the elements of xs starting at index i in ascending order.
(define/cost (isort-vector-from xs i)
  (cond
    [(= i (vector-length xs)) (void)]
    [else
     (isort-vector-from xs (add1 i))
     (insert-vector-at xs (add1 i))]))

;; insert-vector-at : (Vectorof Number) Number -> Void
;; Inserts the element of xs at index i-1 into the proper location
;; among the elements starting at index i.
(define/cost (insert-vector-at xs i)
  (cond
    [(= i (vector-length xs)) (void)]
    [else
     (define x (vector-ref xs (sub1 i)))
     (define y (vector-ref xs i))
     (cond
       [(<= x y) (void)]
       [else
        (vector-set! xs (sub1 i) y)
        (vector-set! xs i x)
        (insert-vector-at xs (add1 i))])]))

;; Tests for isort-list:

(check-equal? (isort-list (list)) (list))
(check-equal? (isort-list (list 1)) (list 1))
(check-equal? (isort-list (list 2 1)) (list 1 2))
(check-equal? (isort-list (list 1 2)) (list 1 2))
(check-equal? (isort-list (list 3 2 1)) (list 1 2 3))
(check-equal? (isort-list (list 3 1 2)) (list 1 2 3))
(check-equal? (isort-list (list 2 3 1)) (list 1 2 3))
(check-equal? (isort-list (list 2 1 3)) (list 1 2 3))
(check-equal? (isort-list (list 1 3 2)) (list 1 2 3))
(check-equal? (isort-list (list 1 2 3)) (list 1 2 3))

(define (cost-of-isort-list n)
  (define xs (build-list n (lambda {i} (random-big-number))))
  (cost-of #:model (cost-model
                     <= 1
                     cons 1
                     first 1
                     rest 1
                     empty? 1)
    (isort-list xs)))

(check-equal?
  (O? cost-of-isort-list (lambda {n} (* n n)) 10 1 random-number)
  #true)

;; Tests for isort-vector:

(define v0 (vector))
(isort-vector v0)
(check-equal? v0 (vector))

(define v1 (vector 1))
(isort-vector v1)
(check-equal? v1 (vector 1))

(define v21 (vector 2 1))
(isort-vector v21)
(check-equal? v21 (vector 1 2))

(define v12 (vector 1 2))
(isort-vector v12)
(check-equal? v12 (vector 1 2))

(define v321 (vector 3 2 1))
(isort-vector v321)
(check-equal? v321 (vector 1 2 3))

(define v312 (vector 3 1 2))
(isort-vector v312)
(check-equal? v312 (vector 1 2 3))

(define v231 (vector 2 3 1))
(isort-vector v231)
(check-equal? v231 (vector 1 2 3))

(define v213 (vector 2 1 3))
(isort-vector v213)
(check-equal? v213 (vector 1 2 3))

(define v132 (vector 1 3 2))
(isort-vector v132)
(check-equal? v132 (vector 1 2 3))

(define v123 (vector 1 2 3))
(isort-vector v123)
(check-equal? v123 (vector 1 2 3))

(define (cost-of-isort-vector n)
  (define xs (build-vector n (lambda {i} (random-big-number))))
  (cost-of #:model (cost-model
                     = 1
                     <= 1
                     void 1
                     vector-length 1
                     vector-ref 1
                     vector-set! 1
                     add1 1
                     sub1 1)
    (isort-vector xs)))

(check-equal?
  (O? cost-of-isort-vector (lambda {n} (* n n)) 10 1 random-number)
  #true)
