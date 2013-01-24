#lang racket

;; isort-list : (Listof Number) -> (Listof Number)
;; Produces a list containing the elements of xs in ascending order.
(define (isort-list xs)
  (cond
    [(empty? xs) empty]
    [else (insert-list (first xs)
            (isort-list (rest xs)))]))

;; insert-list : Number (Listof Number) -> (Listof Number)
;; Produces a list containing x inserted in the proper location among the
;; elements of xs, which must be in ascending order.
(define (insert-list x xs)
  (cond
    [(empty? xs) (cons x empty)]
    [else
     (define y (first xs))
     (cond
       [(<= x y) (cons x xs)]
       [else (cons y (insert-list x (rest xs)))])]))

;; isort-vector : (Vectorof Number) -> Void
;; Rearranges the elements of xs in ascending order.
(define (isort-vector xs)
  (isort-vector-from xs 0))

;; isort-vector-from : (Vectorof Number) Number -> Void
;; Rearranges the elements of xs starting at index i in ascending order.
(define (isort-vector-from xs i)
  (cond
    [(= i (vector-length xs)) (void)]
    [else
     (isort-vector-from xs (add1 i))
     (insert-vector-at xs i)]))

;; insert-vector-at : (Vectorof Number) Number -> Void
;; Inserts the element of xs at index i into the proper location
;; among the elements starting at index i+1.
(define (insert-vector-at xs i)
  (define i+1 (add1 i))
  (cond
    [(= i+1 (vector-length xs)) (void)]
    [else
     ()]))
