#lang racket

(provide
  quickselect
  stooge-sort)

(module+ main
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quickselect

;; quickselect : Natural-Number (Listof Real-Number) -> Real-Number
;; Produces the ith-smallest element of xs, indexed from zero.
;; The index i must be less than the length of xs.
(define (quickselect i xs)
  ;; Base case can simply be when the pivot is the result.
  ;; There is technically no need to test for small inputs!
  ;; Furthermore, the input can never be empty, since i < (length xs).
  (define pivot (first xs))
  (define left (all-less-than pivot xs))
  (define right (all-greater-than pivot xs))
  (define n (length xs))
  (define n1 (length left))
  (define n2 (length right))
  (cond
    [(< i n1) (quickselect i left)]
    [(>= i (- n n2)) (quickselect (- i (- n n2)) right)]
    [else pivot]))

(module+ main
  (check-equal? (quickselect 0 (list 20 10 50)) 10)
  (check-equal? (quickselect 1 (list 20 10 50)) 20)
  (check-equal? (quickselect 2 (list 20 10 50)) 50))

;; all-less-than : Real-Number (Listof Real-Number) -> (Listof Real-Number)
;; Produces every element in xs that is less than k.
(define (all-less-than k xs)
  (cond
    [(empty? xs) empty]
    [else
     (define rest-less-than
       (all-less-than k (rest xs)))
     (cond
       [(< (first xs) k) (cons (first xs) rest-less-than)]
       [else rest-less-than])]))

(module+ main
  (check-equal? (all-less-than 20 (list 20 10 50)) (list 10)))

;; all-greater-than : Real-Number (Listof Real-Number) -> (Listof Real-Number)
;; Produces every element in xs that is greater than k.
(define (all-greater-than k xs)
  (cond
    [(empty? xs) empty]
    [else
     (define rest-greater-than
       (all-greater-than k (rest xs)))
     (cond
       [(> (first xs) k) (cons (first xs) rest-greater-than)]
       [else rest-greater-than])]))

(module+ main
  (check-equal? (all-greater-than 20 (list 20 10 50)) (list 50)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stooge sort

;; stooge-sort : (Vectorof Real-Number) -> Void
;; Puts the elements of v in sorted order.
(define (stooge-sort v)
  (stooge-sort-between v 0 (vector-length v)))

(module+ main
  (let {[v (vector 1 2 3)]} (stooge-sort v) (check-equal? v (vector 1 2 3)))
  (let {[v (vector 1 3 2)]} (stooge-sort v) (check-equal? v (vector 1 2 3)))
  (let {[v (vector 2 1 3)]} (stooge-sort v) (check-equal? v (vector 1 2 3)))
  (let {[v (vector 2 3 1)]} (stooge-sort v) (check-equal? v (vector 1 2 3)))
  (let {[v (vector 3 1 2)]} (stooge-sort v) (check-equal? v (vector 1 2 3)))
  (let {[v (vector 3 2 1)]} (stooge-sort v) (check-equal? v (vector 1 2 3))))

;; stooge-sort-between :
;;   (Vectorof Real-Number) Natural-Number Natural-Number -> Void
;; Puts the elements of v between indices lo (inclusive) and hi (exclusive)
;; into sorted order.
(define (stooge-sort-between v lo hi)
  (define n (- hi lo))
  (cond
    [(< n 2) (void)]
    [else
     (vector-swap-if-necessary! v lo (sub1 hi))
     (cond
       [(< n 3) (void)]
       [else
        (define mid1 (+ lo (floor (* n 1/3))))
        (define mid2 (+ lo (floor (* n 2/3))))
        (stooge-sort-between v lo mid2)
        (stooge-sort-between v mid1 hi)
        (stooge-sort-between v lo mid2)])]))

(module+ main
  (let {[v (vector 3 2 1)]}
    (stooge-sort-between v 0 3)
    (check-equal? v (vector 1 2 3)))
  (let {[v (vector 3 2 1)]}
    (stooge-sort-between v 0 2)
    (check-equal? v (vector 2 3 1)))
  (let {[v (vector 3 2 1)]}
    (stooge-sort-between v 1 3)
    (check-equal? v (vector 3 1 2)))
  (let {[v (vector 3 2 1)]}
    (stooge-sort-between v 0 1)
    (check-equal? v (vector 3 2 1)))
  (let {[v (vector 3 2 1)]}
    (stooge-sort-between v 1 2)
    (check-equal? v (vector 3 2 1)))
  (let {[v (vector 3 2 1)]}
    (stooge-sort-between v 2 3)
    (check-equal? v (vector 3 2 1))))

;; vector-swap-if-necessary! :
;;   (Vectorof Real-Number) Natural-Number Natural-Number -> Void
;; If v[j] < v[i], swaps v[i] with v[j].  Does nothing otherwise.
(define (vector-swap-if-necessary! v i j)
  (define v.i (vector-ref v i))
  (define v.j (vector-ref v j))
  (cond
    [(<= v.i v.j) (void)]
    [else
     (vector-set! v i v.j)
     (vector-set! v j v.i)]))

(module+ main
  (let {[v (vector 3 2 1)]}
    (vector-swap-if-necessary! v 0 2)
    (check-equal? v (vector 1 2 3)))
  (let {[v (vector 1 2 3)]}
    (vector-swap-if-necessary! v 0 2)
    (check-equal? v (vector 1 2 3)))
  (let {[v (vector 3 2 1)]}
    (vector-swap-if-necessary! v 0 1)
    (check-equal? v (vector 2 3 1)))
  (let {[v (vector 1 2 3)]}
    (vector-swap-if-necessary! v 0 1)
    (check-equal? v (vector 1 2 3)))
  (let {[v (vector 3 2 1)]}
    (vector-swap-if-necessary! v 1 2)
    (check-equal? v (vector 3 1 2)))
  (let {[v (vector 1 2 3)]}
    (vector-swap-if-necessary! v 1 2)
    (check-equal? v (vector 1 2 3))))
