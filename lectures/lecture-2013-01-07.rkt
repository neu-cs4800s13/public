#lang racket

(require rackunit)

;; A (Listof Number) is either:
;; - empty, or
;; - (cons Number (Listof Number))

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

(check-equal? 1 2)
