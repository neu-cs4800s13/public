#lang racket

(define (change total coins)
  (define n (length coins))
  (define table (make-table (add1 total) (add1 n) #false))
  (result-coins (change-from table total n coins)))

(define (change-from table total n coins)
  (define entry (table-get table total n))
  (cond
    [entry entry]
    [else
     (table-set!-and-return table total n
       (cond
         [(zero? total) (empty-result)]
         [(empty? (rest coins))
          (result-add (first coins)
            (change-from table (- total (first coins)) n coins))]
         [(> (first coins) total)
          (change-from table total (sub1 n) (rest coins))]
         [else
          (define best-with
            (result-add (first coins)
              (change-from table (- total (first coins)) n coins)))
          (define best-without
            (change-from table total (sub1 n) (rest coins)))
          (cond
            [(< (result-length best-with) (result-length best-without))
             best-with]
            [else
             best-without])]))]))

(struct result [length coins] #:transparent)

(define (empty-result)
  (result 0 empty))

(define (result-add coin r)
  (result
    (add1 (result-length r))
    (cons coin (result-coins r))))

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
    (change 41 (list 25 10 5 1))
    (list 25 10 5 1))

  (check-equal?
    (change 41 (list 25 20 10 5 1))
    (list 20 20 1)))
