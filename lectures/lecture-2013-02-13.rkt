#lang racket

;; A GS is (gs Array Number)
(struct gs [contents start] #:mutable)
;; where start represents the amount of extra space
;; at the front of contents.

(define (empty-gs)
  (gs (make-vector 1) 1))

(define (gs-get GS i)
  (vector-ref (gs-contents GS) (+ (gs-start GS) i)))

(define (gs-set! GS i v)
  (vector-set! (gs-contents GS) (+ (gs-start GS) i) v))

(define (gs-cons v GS)
  (define k (gs-start GS))
  (cond
    [(> k 0)
     (vector-set! (gs-contents GS) (sub1 k) v)
     (set-gs-start! GS (sub1 k))]
    [else
     (define n (vector-length (gs-contents GS)))
     (define new-size (* 2 n))
     (define new-contents (make-vector new-size))
     (vector-copy-tail! (gs-contents GS) new-contents n)
     (vector-set! new-contents n v)
     (set-gs-start! GS n)
     (set-gs-contents! GS new-contents)]))

(define (gs-remove GS)
  (set-gs-start! GS
    (sub1 (gs-start GS))))

;; We didn't write vector-copy-tail! in class,
;; but it is straightforward.
(define (vector-copy-tail! old new n)
  (cond
    [(zero? n) (void)]
    [else
     (define old-i (- (vector-length old) n))
     (define new-i (- (vector-length new) n))
     (vector-set! new new-i
       (vector-ref old old-i))
     (vector-copy-tail! old new (sub1 n))]))
