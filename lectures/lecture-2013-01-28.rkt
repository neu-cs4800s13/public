#lang racket
(require rackunit)

;; Any D is one of:
;; - empty
;; - (non-empty Number D D)
;;   where element is less than every number in left and right.
(struct non-empty [element left right])

;; empty-D : -> D
(define (empty-D) empty)

;; insert-D : Number D -> D
;; Add a number to our data structure.
(define (insert-D x D)
  (cond
    [(empty? D) (non-empty x empty empty)]
    [else (cond
            [(< x (non-empty-element D))
             (non-empty x empty D)]
            [else ???])]))

;; least-in-D : Non-Empty-D -> Number
;; Find the smallest number in our data structure.
(define (least-in-D D)
  (non-empty-element D))

;; insertion-and-selection-sort : (Listof Number) -> (Listof Number)
(define (insertion-and-selection-sort xs)
  (define D (faster-insertion-sort xs))
  (define ys (faster-selection-sort D))
  ys)

;; faster-insertion-sort : (Listof Number) -> D
(define (faster-insertion-sort xs)
  (cond
    [(empty? xs) (empty-D)]
    [(cons? xs) (insert-D (first xs)
                  (faster-insertion-sort (rest xs)))]))

;; faster-selection-sort : D -> (Listof Number)
(define (faster-selection-sort D)
  (cond
    [(empty-D? D) empty]
    [else (cons (least-in-D D)
            (faster-selection-sort (delete-least-in-D D)))]))

;; selection-sort : (Listof Number) -> (Listof Number)
(define (selection-sort xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs)
     (define x (select-least xs))
     (cons x (selection-sort (delete x xs)))]))

;; select-least : (Non-Empty-Listof Number) -> Number
(define (select-least xs)
  (cond
    [(empty? (rest xs)) (first xs)]
    [(cons? (rest xs))
     (define x (first xs))
     (define y (select-least (rest xs)))
     (if (< x y) x y)]))

;; delete : Number (Listof Number) -> (Listof Number)
(define (delete x xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs)
     (if (= (first xs) x)
       (rest xs)
       (cons (first xs)
         (delete x (rest xs))))]))
