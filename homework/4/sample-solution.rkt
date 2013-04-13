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

(define (difference a b)
  (define m (+ 1 (string-length a)))
  (define n (+ 1 (string-length b)))
  (define table (make-multivector (list m n) #false))
  (difference-between table a 0 b 0))

(define (difference/recursive a b)
  (difference-between/recursive a 0 b 0))

(define (difference-between table source i target j)
  (define result (multivector-ref table (list i j)))
  (cond
    [result result]
    [else
     (multivector-set!-and-return table (list i j)
       (cond
         [(and (= i (string-length source)) (= j (string-length target))) 0]
         [(= i (string-length source)) (cost-to-insert table source i target j)]
         [(= j (string-length target)) (cost-to-delete table source i target j)]
         [else
          (best
            (list
              (cost-to-replace table source i target j)
              (cost-to-insert table source i target j)
              (cost-to-delete table source i target j)
              (cost-to-copy table source i target j)
              (cost-to-swap table source i target j)))]))]))

(define (difference-between/recursive source i target j)
  (cond
    [(and (= i (string-length source)) (= j (string-length target))) 0]
    [(= i (string-length source)) (cost-to-insert/recursive source i target j)]
    [(= j (string-length target)) (cost-to-delete/recursive source i target j)]
    [else
     (best
       (list
         (cost-to-replace/recursive source i target j)
         (cost-to-insert/recursive source i target j)
         (cost-to-delete/recursive source i target j)
         (cost-to-swap/recursive source i target j)
         (cost-to-copy/recursive source i target j)))]))

(define (cost-to-replace table source i target j)
  (add1 (difference-between table source (+ 1 i) target (+ 1 j))))

(define (cost-to-replace/recursive source i target j)
  (add1 (difference-between/recursive source (+ 1 i) target (+ 1 j))))

(define (cost-to-insert table source i target j)
  (add1 (difference-between table source i target (+ 1 j))))

(define (cost-to-insert/recursive source i target j)
  (add1 (difference-between/recursive source i target (+ 1 j))))

(define (cost-to-delete table source i target j)
  (add1 (difference-between table source (+ 1 i) target j)))

(define (cost-to-delete/recursive source i target j)
  (add1 (difference-between/recursive source (+ 1 i) target j)))

(define (cost-to-swap table source i target j)
  (cond
    [(and
       (<= (+ 2 i) (string-length source))
       (<= (+ 2 j) (string-length target))
       (char=? (string-ref source i) (string-ref target (+ 1 j)))
       (char=? (string-ref source (+ 1 i)) (string-ref target j)))
     (add1 (difference-between table source (+ 2 i) target (+ 2 j)))]
    [else #false]))

(define (cost-to-swap/recursive source i target j)
  (cond
    [(and
       (<= (+ 2 i) (string-length source))
       (<= (+ 2 j) (string-length target))
       (char=? (string-ref source i) (string-ref target (+ 1 j)))
       (char=? (string-ref source (+ 1 i)) (string-ref target j)))
     (add1 (difference-between/recursive source (+ 2 i) target (+ 2 j)))]
    [else #false]))

(define (cost-to-copy table source i target j)
  (cond
    [(char=? (string-ref source i) (string-ref target j))
     (difference-between table source (+ 1 i) target (+ 1 j))]
    [else #false]))

(define (cost-to-copy/recursive source i target j)
  (cond
    [(char=? (string-ref source i) (string-ref target j))
     (difference-between/recursive source (+ 1 i) target (+ 1 j))]
    [else #false]))

(define (best xs)
  (cond
    [(empty? xs) #false]
    [else (better (first xs) (best (rest xs)))]))

(define (better x y)
  (cond
    [(not x) y]
    [(not y) x]
    [else (min x y)]))

(module+ test
  (check-equal? (difference/recursive "cat" "cat") 0)
  (check-equal? (difference/recursive "bat" "cat") 1)
  (check-equal? (difference/recursive "at" "cat") 1)
  (check-equal? (difference/recursive "bat" "at") 1)
  (check-equal? (difference/recursive "rat" "art") 1)
  (check-equal? (difference/recursive "rats" "art") 2)
  (check-equal? (difference/recursive "cab" "abc") 2)
  (check-equal? (difference/recursive "pot" "top") 2)
  (check-equal? (difference/recursive "rats" "tart") 3)
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
