#lang racket

(require "../software/cs4800.rkt" rackunit)

(current-print pretty-print-handler)

(define default
  (cost-model
    #:var 1
    #:if 1
    cons 1
    empty? 1
    first 1
    rest 1
    < 1))

(define thousand 1000)
(define million (* thousand thousand))

(define alternate
  (cost-model
    empty? (* 3 million)
    first (* 4 million)
    rest (* 7 million)
    cons (* 2 million)
    < 0))

;; sort : (Listof Number) -> (Listof Number)
;; Sorts the input list in ascending order.
(define/cost (sort stuff)
  (if (empty? stuff)
    stuff
    (put-into-stuff (first stuff)
      (sort (rest stuff)))))

;; put-into-stuff : Number (Listof Number)
;; Put one thing into a bunch of sorted stuff.
(define/cost (put-into-stuff thing stuff)
  (if (empty? stuff)
    (cons thing empty)
    (if (< thing (first stuff))
      (cons thing stuff)
      (cons (first stuff)
        (put-into-stuff thing (rest stuff))))))

(define (best-case i) (build-list i identity))
(define (worst-case i) (reverse (best-case i)))

(for {[i (in-range 10)]}
  (define n (expt 10 i))
  (define best (best-case n))
  (define worst (worst-case n))
  (newline)
  (printf "Size: ~a\n" n)
  (printf "Best case:  ~a\n"
    (integer->comma-separated
      (cost-of #:model default (sort best))))
  (printf "Best alt.:  ~a\n"
    (integer->comma-separated
      (cost-of #:model alternate (sort best))))
  (printf "Worst case: ~a\n"
    (integer->comma-separated
      (cost-of #:model default (sort worst))))
  (printf "Worst alt.: ~a\n"
    (integer->comma-separated
      (cost-of #:model alternate (sort worst)))))
