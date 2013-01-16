#lang racket

(require "../software/cs4800.rkt" rackunit)

(current-print pretty-print-handler)

#;
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
     ???]))
