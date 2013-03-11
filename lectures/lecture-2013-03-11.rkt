#lang racket

;; Naive recursive solution:
;; Compute results, then choose item.

(define (bfk-naive I w v W)
  (max-of-numbers
    (result-for-each-i I I w v W)))

(define (result-for-each-i I* I w v W)
  (cond
    [(set-empty? I*) empty]
    [else
     (define i (set-first I*))
     (define w.i (vector-ref w i))
     (define v.i (vector-ref v i))
     (define result-for-i
       (cond
         [(<= w.i W) (+ v.i (bfk-naive (set-remove I i) w v (- W w.i)))]
         [else (* W (/ v.i w.i))]))
     (cons result-for-i
       (result-for-each-i (set-rest I*) I w v W))]))

(define (max-of-numbers ns)
  (cond
    [(empty? ns) 0] ;; correct for knapsack problem, not in general
    [else (max (first ns) (max-of-numbers (rest ns)))]))

;; Greedy solution:
;; Choose item, then compute result.

(define (bfk I w v W)
  (result-for-given-i I w v W
    (item-of-max-value I w v)))

(define (result-for-given-i I w v W i)
  (define w.i (vector-ref w i))
  (define v.i (vector-ref v i))
  (cond
    [(<= w.i W) (+ v.i (bfk (set-remove I i) w v (- W w.i)))]
    [else (* W (/ v.i w.i))]))

(define (item-of-max-value I w v)
  (cond
    [(set-empty? (set-rest I)) (set-first I)]
    [else
     (better-item (set-first I) (item-of-max-value (set-rest I) w v) w v)]))

(define (better-item i1 i2 w v)
  (define ratio1 (ratio i1 w v))
  (define ratio2 (ratio i2 w v))
  (cond
    [(> ratio1 ratio2) i1]
    [else i2]))

(define (ratio i w v)
  (/ (vector-ref v i) (vector-ref w i)))

(module+ test
  (require rackunit)

  (check-equal?
    (bfk
      (set 0 1 2)
      (vector 10 20 30)
      (vector 50 40 30)
      55)
    115)

  (check-equal?
    (bfk
      (set 0 1 2 3)
      (vector 10 20 30 45)
      (vector 50 40 30 41)
      55)
    115))
