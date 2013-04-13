#lang racket

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared

(define (shared a b)
  (define m (+ 1 (string-length a)))
  (define n (+ 1 (string-length b)))
  (define max-table (make-multivector (list m n) #false))
  (define shared-table (make-multivector (list m n) #false))
  (max-shared-starting-at max-table shared-table a 0 b 0))

(define (shared/recursive a b)
  (max-shared-starting-at/recursive a 0 b 0))

(define (max-shared-starting-at mt st a i b j)
  (define entry (multivector-ref mt (list i j)))
  (cond
    [entry entry]
    [else
     (multivector-set!-and-return mt (list i j)
       (cond
         [(or (= i (string-length a)) (= j (string-length b))) 0]
         [else
          (max
            (shared-at st a i b j)
            (max-shared-starting-at mt st a (add1 i) b j)
            (max-shared-starting-at mt st a i b (add1 j)))]))]))

(define (max-shared-starting-at/recursive a i b j)
  (cond
    [(or (= i (string-length a)) (= j (string-length b))) 0]
    [else
     (max
       (shared-at/recursive a i b j)
       (max-shared-starting-at/recursive a (add1 i) b j)
       (max-shared-starting-at/recursive a i b (add1 j)))]))

(define (shared-at st a i b j)
  (define entry (multivector-ref st (list i j)))
  (cond
    [entry entry]
    [else
     (multivector-set!-and-return st (list i j)
       (cond
         [(and
            (< i (string-length a))
            (< j (string-length b))
            (char=? (string-ref a i) (string-ref b j)))
          (add1 (shared-at st a (add1 i) b (add1 j)))]
         [else 0]))]))

(define (shared-at/recursive a i b j)
  (cond
    [(and
       (< i (string-length a))
       (< j (string-length b))
       (char=? (string-ref a i) (string-ref b j)))
     (add1 (shared-at/recursive a (add1 i) b (add1 j)))]
    [else 0]))

(module+ test
  (check-equal? (shared/recursive "cat" "dog") 0)
  (check-equal? (shared/recursive "cat" "bat") 2)
  (check-equal? (shared/recursive "star" "armor") 2)
  (check-equal?
    (shared/recursive "slaughter" "laughter")
    (string-length "laughter"))
  (check-equal? (shared "cat" "dog") 0)
  (check-equal? (shared "cat" "bat") 2)
  (check-equal? (shared "star" "armor") 2)
  (check-equal?
    (shared "slaughter" "laughter")
    (string-length "laughter")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Difference

;; Operations:
;; - Copy 1 char from source to target; cost = 0
;; - Replace 1 char from source to garget; cost = 1
;; - Insert 1 char to target; cost = 1
;; - Delete 1 char from source; cost = 1
;; - Swap 2 chars from source to target; cost = 1

(define (difference a b)
  (define m (+ 1 (string-length a)))
  (define n (+ 1 (string-length b)))
  (define table (make-multivector (list m n) #false))
  (difference-between table a 0 b 0))

(define (difference-between table source i target j)
  (define result (multivector-ref table (list i j)))
  (cond
    [result result]
    [else
     (multivector-set!-and-return table (list i j)
       (cond
         [(and (= i (string-length source)) (= j (string-length target))) 0]
         [(= i (string-length source))
          (cost-to-insert table source i target j)]
         [(= j (string-length target))
          (cost-to-delete table source i target j)]
         [else
          (define base-cost
            (min
              (cost-to-replace table source i target j)
              (cost-to-insert table source i target j)
              (cost-to-delete table source i target j)))
          (define cost-with-copy
            (cond
              [(can-copy? source i target j)
               (min base-cost (cost-to-copy table source i target j))]
              [else base-cost]))
          (define cost-with-swap
            (cond
              [(can-swap? source i target j)
               (min cost-with-copy (cost-to-swap table source i target j))]
              [else cost-with-copy]))
          cost-with-swap]))]))

(define (cost-to-replace table source i target j)
  (+ SINGLE-REPLACE-COST
     (difference-between table source (+ 1 i) target (+ 1 j))))

(define (cost-to-insert table source i target j)
  (+ SINGLE-INSERT-COST
     (difference-between table source i target (+ 1 j))))

(define (cost-to-delete table source i target j)
  (+ SINGLE-DELETE-COST
     (difference-between table source (+ 1 i) target j)))

(define (cost-to-copy table source i target j)
  (+ SINGLE-COPY-COST
     (difference-between table source (+ 1 i) target (+ 1 j))))

(define (cost-to-swap table source i target j)
  (+ SINGLE-SWAP-COST
     (difference-between table source (+ 2 i) target (+ 2 j))))

(define (can-copy? source i target j)
  (char=? (string-ref source i) (string-ref target j)))

(define (can-swap? source i target j)
  (and
    (<= (+ 2 i) (string-length source))
    (<= (+ 2 j) (string-length target))
    (char=? (string-ref source i) (string-ref target (+ 1 j)))
    (char=? (string-ref source (+ 1 i)) (string-ref target j))))

(define SINGLE-COPY-COST 0)
(define SINGLE-REPLACE-COST 1)
(define SINGLE-INSERT-COST 1)
(define SINGLE-DELETE-COST 1)
(define SINGLE-SWAP-COST 1)

(module+ test
  (check-equal? (difference "cat" "cat") 0)
  (check-equal? (difference "bat" "cat") 1)
  (check-equal? (difference "at" "cat") 1)
  (check-equal? (difference "bat" "at") 1)
  (check-equal? (difference "rat" "art") 1)
  (check-equal? (difference "rats" "art") 2)
  (check-equal? (difference "cab" "abc") 2)
  (check-equal? (difference "pot" "top") 2)
  (check-equal? (difference "rats" "tart") 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multidimensional Vectors

;; A (Multi-Vectorof X) is (multi (Listof Integer) (Vectorof X))
(struct multi [dimensions vector] #:transparent)

(define (make-multivector dimensions fill)
  (multi dimensions (make-vector (product dimensions) fill)))

(define (multivector-ref mv is)
  (vector-ref (multi-vector mv)
    (index (multi-dimensions mv) is)))

(define (multivector-set!-and-return mv is x)
  (vector-set! (multi-vector mv)
    (index (multi-dimensions mv) is)
    x)
  x)

(define (product ns)
  (cond
    [(empty? ns) 1]
    [else (* (first ns) (product (rest ns)))]))

(define (index ds is)
  (cond
    [(empty? is) 0]
    [else (+ (* (first ds) (index (rest ds) (rest is))) (first is))]))
