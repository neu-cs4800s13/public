#lang racket

(provide (all-defined-out))
(require rackunit)

(current-print pretty-print-handler)

;; A (Listof Number) is either:
;; - empty, or
;; - (cons Number (Listof Number))

(define (alternate-cost-of-sort stuff)
  (+ (cost-of-empty? stuff)
     (if (empty? stuff)
       0
       (+ (cost-of-first stuff)
          (cost-of-rest stuff)
          (alternate-cost-of-sort (rest stuff))
          (alternate-cost-of-put-into-stuff (first stuff)
            (sort (rest stuff)))))))

;; sort : (Listof Number) -> (Listof Number)
;; Sorts the input list in ascending order.
(define (sort stuff)
  (if (empty? stuff) ;; 1 (empty?) + 1 (stuff) + 1 (if)
    stuff ;; 1
    (put-into-stuff (first stuff) ;; 1 (stuff) + 1 (first)
      (sort (rest stuff)))))      ;; 1 (stuff) + 1 (rest)
                                  ;; recur # of times = (length stuff) = N
                                  ;; cost of sort w/o put-into-stuff =
                                  ;; 4 + N * 7
                                  ;; assuming worst case:
                                  ;; 4 + Sum [i=0 to N-1] (7 + 11 + 14(N-i))
                                  ;; 4 + 18N + Sum[0<=i<N](14(N-i))
                                  ;; is approximately 4 + 18N + 7N^2
                                  ;; best case: 4 + 7N + 6 +11(N-1) = 17 + 18(N-1)

;; conjecture:
;; ascending list is "best case" cost
;; descending list is "worst case" cost
;; average case --
;; improvements -- if input is in descending order, reverse it
;;                  cost = K * N for some K
;;                 is it worth it?
;;                 -- savings in worst case - cost in best case
;;                 -- cost vs savings in average case

(define (alternate-cost-of-put-into-stuff thing stuff)
  (+ (cost-of-empty? stuff)
     (if (empty? stuff)
       (cost-of-cons thing empty)
       (+ (cost-of-first stuff)
          (if (< thing (first stuff))
            (cost-of-cons thing stuff)
            (+ (cost-of-first stuff)
               (cost-of-rest stuff)
               (alternate-cost-of-put-into-stuff thing (rest stuff))
               (cost-of-cons (first stuff)
                 (put-into-stuff thing (rest stuff)))))))))

;; put-into-stuff : Number (Listof Number)
;; Put one thing into a bunch of sorted stuff.
(define (put-into-stuff thing stuff)
  (if (empty? stuff) ;; 3 (stuff + empty? + if)
    (cons thing empty) ;; 3 (thing + empty + cons)
    (if (< thing (first stuff)) ;; 5 (first + stuff + thing + < + if)
      (cons thing stuff) ;; 3 (cons + thing + stuff)
      (cons (first stuff) ;; 3 (cons + first + stuff)
        (put-into-stuff thing (rest stuff)))))) ;; 3 (thing + rest + stuff)
                                                ;; recur # of times = 0 - N
                                                ;; 3 cases:
                                                ;; - empty stuff: 6 steps
                                                ;; - nonempty, keep going: 3+5+3+3=14
                                                ;; - nonempty, stop: 3+5+3=11
                                                ;; (14*[0-N])+[6-11] = [6] to [11+14N]

(check-equal? (sort empty) empty) ;; 4
(check-equal? (sort (list 3 1 2)) (list 1 2 3))

(check-equal? (put-into-stuff 3 (list 1 2)) (list 1 2 3))

;; cost-of-sort : (Listof Number) -> Number
(define (cost-of-sort stuff)
  (+ 3
     (if (empty? stuff)
       1
       (+ 4
          (cost-of-sort (rest stuff))
          (cost-of-put-into-stuff (first stuff) (sort (rest stuff)))))))

(check-equal? (cost-of-sort empty) 4)

;; cost-of-sort : (Listof Number) -> Number
(define (cost-of-put-into-stuff thing stuff)
  (+ 3
     (if (empty? stuff)
       3
       (+ 5
          (if (< thing (first stuff))
            3
            (+ 6 (cost-of-put-into-stuff thing (rest stuff))))))))

(check-equal? (cost-of-put-into-stuff 1 '()) 6)

(define (cost-of-empty? x) 3000000) ;; maybe 1ms?
(define (cost-of-first x) 4000000)  ;; 2ms?
(define (cost-of-rest x) 7000000)   ;; 2ms?
(define (cost-of-cons x y) 2000000);; 10ms?

(cost-of-sort '(6 1 6 4 2))
(cost-of-put-into-stuff 6 '(1 2 4 6))

(alternate-cost-of-sort '(6 1 6 4 2))
(alternate-cost-of-put-into-stuff 6 '(1 2 4 6))

(define (best-case-cost-of-sort i)
  (cost-of-sort
    (build-list i identity)))

(define (best-case-alt-cost-of-sort i)
  (alternate-cost-of-sort
    (build-list i identity)))

(define (worst-case-cost-of-sort i)
  (cost-of-sort
    (reverse (build-list i identity))))

(define (worst-case-alt-cost-of-sort i)
  (alternate-cost-of-sort
    (reverse (build-list i identity))))

(for {[i '(0 1 2 10 100 1000 10000 100000 1000000)]}
  (newline)
  (displayln i)
  (displayln (best-case-cost-of-sort i))
  (displayln (best-case-alt-cost-of-sort i))
  (displayln (worst-case-cost-of-sort i))
  (displayln (worst-case-alt-cost-of-sort i)))
