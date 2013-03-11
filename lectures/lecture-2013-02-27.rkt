#lang racket

(define (bounded-integer-knapsack n w v W)
  (define table (make-table (add1 n) (add1 W) #false))
  (knapsack-items (bounded-integer-knapsack-from table 0 n w v W)))

(define (bounded-integer-knapsack-from t i n w v W)
  (define entry (table-get t i W))
  (cond
    [entry entry]
    [else
     (table-set!-and-return t i W
       (cond
         [(>= i n) (empty-knapsack)]
         [(> (vector-ref w i) W)
          (bounded-integer-knapsack-from t (add1 i) n w v W)]
         [else
          (define best-with
            (knapsack-add i (vector-ref v i)
              (bounded-integer-knapsack-from t (add1 i) n w v
                (- W (vector-ref w i)))))
          (define best-without
            (bounded-integer-knapsack-from t (add1 i) n w v W))
          (cond
            [(> (knapsack-value best-with) (knapsack-value best-without))
             best-with]
            [else
             best-without])]))]))

(struct knapsack [value items] #:transparent)

(define (empty-knapsack)
  (knapsack 0 empty))

(define (knapsack-add i v k)
  (knapsack
    (+ v (knapsack-value k))
    (cons i (knapsack-items k))))

(struct table [width height vector] #:transparent)

(define (make-table width height initial)
  (table width height (make-vector (* width height) initial)))

(define (table-get t i j)
  (vector-ref (table-vector t) (table-index i j t)))

(define (table-set!-and-return t i j v)
  (vector-set! (table-vector t) (table-index i j t) v)
  v)

(define (table-index i j t)
  (+ (* j (table-width t)) i))

(module+ test
  (require rackunit)

  (check-equal?
    (bounded-integer-knapsack
      3
      (vector 10 20 30)
      (vector 50 40 30)
      55)
    (list 0 1))

  (check-equal?
    (bounded-integer-knapsack
      4
      (vector 10 20 30 45)
      (vector 50 40 30 41)
      55)
    (list 0 3)))
