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

;; f(n) = cost of sorting in the worst case

(define (f n)
  (define xs (worst-case n))
  (cost-of #:model default
    (sort xs)))

(define (g n) (* n n))

(define (O? f g c n0)
  (define k (random-number))
  (printf "Running ~s trials.\n" k)
  (for/and {[trial (in-range 1 k)]}
    (let {[n (+ n0 (random-big-number))]}
      (printf "Trying n=~s\n" n)
      (<= (f n) (* c (g n))))))

(define (random-number)
  (+ 1 (random 10)))

(define (random-big-number)
  (expt 10 (random-number)))

(O?
  (lambda (n) (* n n))
  (lambda (n) (* 2000 n))
  1
  1)

#;(O? f g 18 1)

#;(for {[i (in-range 10)]}
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

;; Questions:

;; - Is there an "intuitive" approach to solving big-O inequalities?
