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
  (define pass1-state (initial-state-for memory))
  (define pass1-tree (depth-first-search-vertex memory root pass1-state))
  (define finished (reverse (flatten pass1-tree)))
  (define transposed (memory-transpose memory))
  (define pass2-state (initial-state-for transposed))
  (define pass2-tree (depth-first-search-vertices memory finished pass2-state))
  (define components (flatten-each pass2-tree))
  (define order->vertex (list->vector (flatten components)))
  (define vertex->order (invert-vertex-mapping memory order->vertex))
  (define definitions (components->definitions memory components vertex->order))
  (define expression (name-in-order (vector-ref vertex->order root)))
  (list definitions expression))

(define (name-in-order order)
  (string->symbol
    (string-append "v"
      (number->string order))))

(define (components->definitions memory components vertex->order)
  (cond
    [(empty? components) empty]
    [else
     (append (component->definitions memory (first components) vertex->order)
       (components->definitions memory (rest components) vertex->order))]))

(define (component->definitions memory component vertex->order)
  (append
    (vertices->defines memory component vertex->order)
    (vertices->updates memory component vertex->order)))

(define (vertices->updates memory vertices vertex->order)
  (cond
    [(empty? vertices) empty]
    [else (append (vertex->updates memory (first vertices) vertex->order)
            (vertices->updates memory (rest vertices) vertex->order))]))

(define (vertices->defines memory vertices vertex->order)
  (cond
    [(empty? vertices) empty]
    [else (cons (vertex->define memory (first vertices) vertex->order)
            (vertices->defines memory (rest vertices) vertex->order))]))

(define (vertex->define memory i vertex->order)
  (define order (vector-ref vertex->order i))
  (define name (name-in-order order))
  (define value (vector-ref memory i))
  (list 'define name (value->expression value vertex->order order)))

(define (vertex->updates memory i vertex->order)
  (define order (vector-ref vertex->order i))
  (define value (vector-ref memory i))
  (value->updates value vertex->order order))

(define (value->expression value vertex->order order)
  (cond
    [(string? value) value]
    [(vector? value)
     (cons 'vector
       (vector->expressions value vertex->order order 0))]))

(define (value->updates value vertex->order order)
  (cond
    [(string? value) empty]
    [(vector? value)
     (vector->updates value vertex->order order 0)]))

(define (vector->expressions v vertex->order order i)
  (cond
    [(>= i (vector-length v)) empty]
    [else (cons (pointer->expression (vector-ref v i) vertex->order order)
            (vector->expressions v vertex->order order (add1 i)))]))

(define (vector->updates v vertex->order order i)
  (cond
    [(>= i (vector-length v)) empty]
    [else (append (pointer->updates (vector-ref v i) i vertex->order order)
            (vector->updates v vertex->order order (add1 i)))]))

(define (pointer->expression pointer vertex->order order)
  (define vertex (unbox pointer))
  (define pointer-order (vector-ref vertex->order vertex))
  (cond
    [(>= pointer-order order) "dummy"]
    [else (name-in-order pointer-order)]))

(define (pointer->updates pointer i vertex->order order)
  (define vertex (unbox pointer))
  (define pointer-order (vector-ref vertex->order vertex))
  (cond
    [(>= pointer-order order)
     (list
       (list 'vector-set! (name-in-order order) i
         (name-in-order pointer-order)))]
    [else empty]))

(define (memory-transpose memory)
  (define n (vector-length memory))
  (define predecessors (make-vector n empty))
  (record-memory-predecessors predecessors memory 0)
  (define transposed (make-vector n #false))
  (populate-neighbors transposed predecessors 0)
  transposed)

(define (record-memory-predecessors predecessors memory i)
  (cond
    [(>= i (vector-length memory)) (void)]
    [else
     (define neighbors (memory-neighbors memory i))
     (record-predecessors predecessors i neighbors)
     (record-memory-predecessors predecessors memory (add1 i))]))

(define (record-predecessors predecessors i neighbors)
  (cond
    [(empty? neighbors) (void)]
    [else
     (define j (first neighbors))
     (vector-set! predecessors j
       (cons i (vector-ref predecessors j)))
     (record-predecessors predecessors i (rest neighbors))]))

(define (populate-neighbors memory neighbors i)
  (cond
    [(>= i (vector-length memory)) (void)]
    [else
     (vector-set! memory i
       (neighbors->value (vector-ref neighbors i)))
     (populate-neighbors memory neighbors (add1 i))]))

(define (neighbors->value neighbors)
  (list->vector (map box neighbors)))

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
    [else (cons (depth-first-search-vertex memory (first roots) state)
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

(define (flatten-each trees)
  (map flatten trees))

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

  ;; Note: In this implementation, v2 and v3 are swapped from the
  ;; version in the assignment.
  (check-equal?
    (snapshot cyclic 0)
    (list
      (list
        (list 'define 'v0 "Jacob's")
        (list 'define 'v1 "ladder")
        (list 'define 'v2 (list 'vector 'v1 "dummy"))
        (list 'define 'v3 (list 'vector 'v0 'v2))
        (list 'vector-set! 'v2 1 'v3))
      'v3)))
