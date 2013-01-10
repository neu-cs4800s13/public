#lang racket

(provide
  integer->english
  integer->comma-separated)

(define (integer->english n)
  (with-output-to-string
    (lambda {}
      (display-english-integer n))))

(define (integer->comma-separated n)
  (with-output-to-string
    (lambda {}
      (display-comma-separated-integer n))))

(define (display-comma-separated-integer n)
  (when (negative? n)
    (display "-"))
  (display-comma-separated-natural-number (abs n)))

(define (display-english-integer n)
  (when (negative? n)
    (display "-"))
  (display-english-natural-number (abs n)))

(define (display-english-natural-number n
          [suffix ""]
          [suffixes suffix-words])
  (cond
    [(empty? suffixes)
     (display-comma-separated-natural-number n)
     (display suffix)]
    [else
     (define-values {thousands ones}
       (quotient/remainder n 1000))
     (cond
       [(zero? thousands)
        (display ones)
        (display suffix)]
       [else
        (display-english-natural-number
          thousands
          (string-append " " (first suffixes))
          (rest suffixes))
        (unless (zero? ones)
          (display " ")
          (display ones)
          (display suffix))])]))

(define (display-comma-separated-natural-number n)
  (define-values {thousands ones}
    (quotient/remainder n 1000))
  (cond
    [(zero? thousands) (display n)]
    [else
     (display-comma-separated-natural-number thousands)
     (define-values {tens just-ones}
       (quotient/remainder ones 10))
     (define-values {just-hundreds just-tens}
       (quotient/remainder tens 10))
     (display ",")
     (display just-hundreds)
     (display just-tens)
     (display just-ones)]))

(define suffix-words
  '["thousand"
    "million"
    "billion"
    "trillion"
    "quadrillion"
    "quintillion"])
