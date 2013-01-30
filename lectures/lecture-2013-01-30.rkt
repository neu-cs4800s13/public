#lang racket
(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New today:

;; Any D is one of:
;; - empty
;; - (non-empty Number Number D D)
(struct non-empty [size element left right])
;;   where element is less than every number in left and right
;;   and size = 1 + |left| + |right|
;;   and |left| is at most size/2

;; insert-D : Number D -> D
;; Add a number to our data structure.
(define (insert-D x D)
  (cond
    [(empty? D) (node x empty empty)]
    [else
     (cond
       [(< x (non-empty-element D))
        (node x empty D)]
       [(< (size-of-D (non-empty-left D))
           (size-of-D (non-empty-right D)))
        (node (non-empty-element D)
          (insert-D x (non-empty-left D))
          (non-empty-right D))]
       [else
        (node (non-empty-element D)
          (non-empty-left D)
          (insert-D x (non-empty-right D)))])]))

;; size-of-D : D -> Number
;; Compute the size of our data structure.
(define (size-of-D D)
  (cond
    [(empty? D) 0]
    [else (non-empty-size D)]))

;; node : Number D D -> D
;; Produce a non-empty D, computing and sorting its size.
(define (node x l r)
  (non-empty
    (+ 1 (size-of-D l) (size-of-D r))
    x
    l
    r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unchanged from before:

;; empty-D : -> D
(define (empty-D) empty)

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
