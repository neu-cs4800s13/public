#lang racket

(define (collect memory roots)
  (define state (initial-state-for memory))
  (define keeper-tree (depth-first-search-vertices memory roots state))
  (define keepers (flatten keeper-tree))
  (define new->old (list->vector keepers))
  (define old->new (invert-vertex-mapping memory new->old))
  (define new-memory (update-memory memory new->old old->new))
  (define new-roots (update-roots roots old->new))
  (list new-memory new-roots))

(define (snapshot memory root)
  (list empty "dummy"))

(define (initial-state-for memory)
  (make-vector (vector-length memory) 'unseen))

(define (update-roots roots old->new)
  (cond
    [(empty? roots) empty]
    [else (cons (vector-ref old->new (first roots))
            (update-roots (rest roots) old->new))]))

(define (update-memory original new->old old->new)
  (define n (vector-length new->old))
  (define updated (make-vector n #false))
  (update-memory-locations original updated new->old old->new 0)
  updated)

(define (update-memory-locations original updated new->old old->new i)
  (cond
    [(>= i (vector-length updated)) (void)]
    [else
     (define j (vector-ref new->old i))
     (define original-value (vector-ref original j))
     (define updated-value (update-value original-value old->new))
     (vector-set! updated i updated-value)
     (update-memory-locations original updated new->old old->new (add1 i))]))

(define (update-value original-value old->new)
  (cond
    [(string? original-value) original-value]
    [(vector? original-value)
     (define n (vector-length original-value))
     (define updated-value (make-vector n #false))
     (update-pointers original-value updated-value old->new 0)
     updated-value]))

(define (update-pointers original-vector updated-vector old->new i)
  (cond
    [(>= i (vector-length updated-vector)) (void)]
    [else
     (define original-vertex (unbox (vector-ref original-vector i)))
     (define updated-vertex (vector-ref old->new original-vertex))
     (vector-set! updated-vector i (box updated-vertex))
     (update-pointers original-vector updated-vector old->new (add1 i))]))

(define (depth-first-search-vertices memory roots state)
  (cond
    [(empty? roots) empty]
    [(cons? roots)
     (cons (depth-first-search-vertex memory (first roots) state)
       (depth-first-search-vertices memory (rest roots) state))]))

(define (depth-first-search-vertex memory vertex state)
  (define status (vector-ref state vertex))
  (cond
    [(equal? status 'seen) empty]
    [(equal? status 'visiting) empty]
    [(equal? status 'unseen)
     (vector-set! state vertex 'visiting)
     (define neighbors (memory-neighbors memory vertex))
     (define finished (depth-first-search-vertices memory neighbors state))
     (vector-set! state vertex 'seen)
     (list finished vertex)]))

(define (memory-neighbors memory vertex)
  (define value (vector-ref memory vertex))
  (cond
    [(string? value) empty]
    [(vector? value) (vector-neighbors value 0)]))

(define (vector-neighbors v i)
  (cond
    [(>= i (vector-length v)) empty]
    [else (cons (unbox (vector-ref v i))
            (vector-neighbors v (add1 i)))]))

(define (flatten tree)
  (flatten/append tree empty))

(define (flatten/append tree tail)
  (cond
    [(cons? tree)
     (flatten/append (first tree)
       (flatten/append (rest tree) tail))]
    [(empty? tree) tail]
    [else (cons tree tail)]))

(define (invert-vertex-mapping v mapping)
  (define n (vector-length v))
  (define inverted (make-vector n #false))
  (invert-vertex-mappings mapping inverted 0)
  inverted)

(define (invert-vertex-mappings original inverted i)
  (cond
    [(>= i (vector-length original)) (void)]
    [else
     (define j (vector-ref original i))
     (vector-set! inverted j i)
     (invert-vertex-mappings original inverted (add1 i))]))

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
