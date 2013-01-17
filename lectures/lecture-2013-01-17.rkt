#lang racket

(require "../software/cs4800.rkt" rackunit)

(current-print pretty-print-handler)

(define (faster-sort stuff)
  (define N (length stuff))
  (cond
    [(<= N 1) stuff]
    [else
     (define N/2 (quotient N 2))
     (define both-halves (bifurcate-list stuff N/2))
     (define first-half (first both-halves))
     (define second-half (second both-halves))
     (define sorted-first-half (faster-sort first-half))
     (define sorted-second-half (faster-sort second-half))
     (define sorted
       (combine-sorted-lists sorted-first-half sorted-second-half))
     sorted]))

;; bifurcate-list : (Listof X) Number -> (list (Listof X) (Listof X))
(define (bifurcate-list stuff size-of-first-half)
  (cond
    [(zero? size-of-first-half) (list empty stuff)]
    [else
     (define bifurcated-rest
       (bifurcate-list (rest stuff) (sub1 size-of-first-half)))
     (list
       (cons (first stuff) (first bifurcated-rest))
       (second bifurcated-rest))]))

(check-equal? (bifurcate-list (list 2 1) 0)     (list empty (list 2 1)))
(check-equal? (bifurcate-list (list 3 2 1) 1)   (list (list 3) (list 2 1)))
(check-equal? (bifurcate-list (list 4 3 2 1) 2) (list (list 4 3) (list 2 1)))

;; combine-sorted-lists : (Listof X) (Listof X) -> (Listof X)
(define (combine-sorted-lists one two)
  (cond
    [(empty? one) two]
    [(empty? two) one]
    [else
     (cond
       [(< (first one) (first two))
        (cons (first one)
          (combine-sorted-lists (rest one) two))]
       [else
        (cons (first two)
          (combine-sorted-lists one (rest two)))])]))

(check-equal?
  (combine-sorted-lists (list 2 3 7 8) (list 0 1 4 5))
  (list 0 1 2 3 4 5 7 8))

(check-equal?
  (faster-sort (list 2 8 7 3 5 4 1 0))
  (list 0 1 2 3 4 5 7 8))
