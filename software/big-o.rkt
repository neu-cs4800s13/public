#lang racket

(provide O?)

(require rackunit)

;; O? : (Number -> Number) (Number -> Number) Number Number
;; Checks if f(n) is in O(g(n)).
;; Specifically, tests for any (random) n > n0,
;; f(n) <= c * g(n).
(define (O? f g c n0)
  (define k (random-number))
  (for/and {[trial (in-range 1 k)]}
    (let {[n (+ n0 (random-big-number))]}
      (<= (f n) (* c (g n))))))

(define (random-number)
  (+ 1 (random 10)))

(define (random-big-number)
  (expt 10 (random-number)))

(check-equal?
  (O? (lambda (n) (expt n 2)) (lambda (n) (expt n 3)) 1 1)
  #true)

(check-equal?
  (O? (lambda (n) (expt n 3)) (lambda (n) (expt n 2)) 1 1)
  #false)
