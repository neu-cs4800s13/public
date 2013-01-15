#lang racket

(require "cs4800.rkt" rackunit)

(check-equal? (integer->english 0) "0")
(check-equal? (integer->english 1) "1")
(check-equal? (integer->english 999) "999")
(check-equal? (integer->english 1000) "1 thousand")
(check-equal? (integer->english 1001) "1 thousand 1")
(check-equal? (integer->english 2000) "2 thousand")
(check-equal? (integer->english 2001) "2 thousand 1")
(check-equal? (integer->english -9008007006005004003002001)
  "-9,008,007 quintillion 6 quadrillion 5 trillion 4 billion 3 million 2 thousand 1")
(check-equal? (integer->english -9000000000005000000002000)
  "-9,000,000 quintillion 5 trillion 2 thousand")

(define/cost (len xs)
  (cond
    [(empty? xs) 0]
    [else (add1 (len (rest xs)))]))

(check-equal? (len '()) 0)
(check-equal? (len '(1)) 1)
(check-equal? (len '(2 1)) 2)
(check-equal? (len '(3 2 1)) 3)

(check-equal? (cost-of (len '())) 'empty?)
(check-equal? (cost-of (len '(1))) '(+ (* 2 empty?) rest add1))
(check-equal? (cost-of (len '(2 1))) '(+ (* 3 empty?) (* 2 rest) (* 2 add1)))
(check-equal? (cost-of (len '(3 2 1))) '(+ (* 4 empty?) (* 3 rest) (* 3 add1)))

(define special
  (cost-model
    empty? 0
    first 0
    rest 0
    add1 0
    #:if #false
    #:def #false
    #:var #false
    #:call #false
    #:const #false))

(with-cost-model special
  (check-equal? (cost-of (len '()))
    '(+ (* 2 #:def) (* 2 #:const) (* 2 #:call) #:var #:if))
  (check-equal? (cost-of (len '(1)))
    '(+ (* 6 #:def) (* 2 #:const) (* 6 #:call) (* 3 #:var) (* 2 #:if)))
  (check-equal? (cost-of (len '(2 1)))
    '(+ (* 10 #:def) (* 2 #:const) (* 10 #:call) (* 5 #:var) (* 3 #:if)))
  (check-equal? (cost-of (len '(3 2 1)))
    '(+ (* 14 #:def) (* 2 #:const) (* 14 #:call) (* 7 #:var) (* 4 #:if))))

(define simple
  (cost-model
    empty? 2
    rest 3
    add1 5))

(with-cost-model simple
  (check-equal? (cost-of (len '())) 2)
  (check-equal? (cost-of (len '(1))) 12)
  (check-equal? (cost-of (len '(2 1))) 22)
  (check-equal? (cost-of (len '(3 2 1))) 32))

(define fancy
  (cost-model
    #:if 2
    empty? 3
    rest 5
    add1 (lambda (x) (+ (integer-length x) 1))))

(with-cost-model fancy
  (check-equal? (cost-of (len '())) 5)
  (check-equal? (cost-of (len '(1))) 16)
  (check-equal? (cost-of (len '(2 1))) 28)
  (check-equal? (cost-of (len '(3 2 1))) 41))

(check-equal? (cost-of #:model fancy (len '())) 5)
(check-equal? (cost-of #:model fancy (len '(1))) 16)
(check-equal? (cost-of #:model fancy (len '(2 1))) 28)
(check-equal? (cost-of #:model fancy (len '(3 2 1))) 41)

(current-cost-model simple)

(define/cost cost0 len0 (len '()))
(define/cost cost1 len1 (len '(1)))
(define/cost cost2 len2 (len '(2 1)))
(define/cost cost3 len3 (len '(3 2 1)))

(check-equal? len0 0)
(check-equal? len1 1)
(check-equal? len2 2)
(check-equal? len3 3)

(check-equal? cost0 2)
(check-equal? cost1 12)
(check-equal? cost2 22)
(check-equal? cost3 32)

(define/cost #:model fancy fancy-cost0 fancy-len0 (len '()))
(define/cost #:model fancy fancy-cost1 fancy-len1 (len '(1)))
(define/cost #:model fancy fancy-cost2 fancy-len2 (len '(2 1)))
(define/cost #:model fancy fancy-cost3 fancy-len3 (len '(3 2 1)))

(check-equal? fancy-len0 0)
(check-equal? fancy-len1 1)
(check-equal? fancy-len2 2)
(check-equal? fancy-len3 3)

(check-equal? fancy-cost0 5)
(check-equal? fancy-cost1 16)
(check-equal? fancy-cost2 28)
(check-equal? fancy-cost3 41)

(define/cost (mirror x)
  (cond
    [(pair? x)
     (define x1 (car x))
     (define x2 (cdr x))
     (define y1 (mirror x1))
     (define y2 (mirror x2))
     (define y (cons y1 y2))
     y]
    [else x]))

(check-equal?
  (cost-of #:model (cost-model pair? 1 cons 1 car 1 cdr 1)
    (mirror (cons 1 2)))
  7)

(check-equal?
  (cost-of #:model (cost-model #:if 1)
    (and 'a 'b #f 'd))
  3)

(check-equal?
  (cost-of #:model (cost-model #:if 1)
    (or #f #f 'c #f))
  3)

(check-equal?
  (cost-of #:model (cost-model #:set! 1)
    (let {[x 1]}
      (#%expression
        (begin
          (set! x 2)
          x))))
  1)

(check-equal?
  (cost-of #:model (cost-model #:set! 1)
    (let {[x 1]}
      (#%expression
        (begin0
          (set! x 2)
          x))))
  1)
