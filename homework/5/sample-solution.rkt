#lang racket

(define (collect memory roots)
  (list memory roots))

(define (snapshot memory root)
  (list empty "dummy"))

(module+ test
  (require rackunit)

  (define memory1
    (vector
      ;; index 0:
      "new"
      ;; index 1:
      "york"
      ;; index 2, the definition of new-york:
      (vector (box 0) (box 1))
      ;; index 3:
      "comma"
      ;; index 4, the definition of nyc:
      (vector (box 2) (box 3) (box 2))
      ;; index 5:
      "buffalo"
      ;; index 6, the definition of up-state:
      (vector (box 5) (box 2))))

  (check-equal?
    (collect memory1 (list 2 6))
    (list
      (vector
        ;; index 0:
        "new"
        ;; index 1:
        "york"
        ;; index 2, the definition of new-york:
        (vector (box 0) (box 1))
        ;; index 3:
        "buffalo"
        ;; index 4, the definition of up-state:
        (vector (box 3) (box 2)))
      (list 2 4)))

  (define cyclic
    (vector
      (vector (box 1) (box 2))
      "Jacob's"
      (vector (box 3) (box 0))
      "ladder"))

  (check-equal?
    (snapshot cyclic 0)
    (list
      (list
        (list 'define 'v0 "Jacob's")
        (list 'define 'v1 "ladder")
        (list 'define 'v2 (list 'vector 'v0 ""))
        (list 'define 'v3 (list 'vector "ladder" 'v2))
        (list 'vector-set! 'v2 1 'v3))
      'v2)))
