#lang racket

(provide O? random-number random-small-number random-big-number)

;; O? : (Number -> Number) (Number -> Number) Number Number
;; Optional fifth argument: (-> Number)
;; Checks if f(n) is in O(g(n)).
;; Specifically, tests for any (random) n > n0,
;; f(n) <= c * g(n).
(define (O? f g c n0 [choose-big-number random-big-number])
  (define k (random-number))
  (for/and {[trial (in-range 1 k)]}
    (let {[n (+ n0 (choose-big-number))]}
      (<= (f n) (* c (g n))))))

(define (random-number)
  (+ 1 (random 10)))

(define (random-big-number)
  (expt 10 (random-number)))

(define (random-small-number)
  (random 10))

(module+ main
  (require rackunit)

  (check-equal?
    (O? (lambda (n) (expt n 2)) (lambda (n) (expt n 3)) 1 1)
    #true)

  (check-equal?
    (O? (lambda (n) (expt n 3)) (lambda (n) (expt n 2)) 1 1)
    #false)

  (check-equal?
    (O? (lambda (n) (expt n 3)) (lambda (n) (expt n 2)) 1 1 random-small-number)
    #false))
