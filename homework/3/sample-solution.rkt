#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Setup

;; To run the "test" submodule:
;; - in DrRacket, it will run automatically.
;; - at the command-line, use the command "raco test sample-solution.rkt".
(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports (if used as a library)

(provide

  fresh-assoc
  assign
  unassign
  lookup

  empty-set
  in?
  extend
  without

  new-queue
  full?
  add-leftmost
  get-leftmost
  drop-leftmost
  add-rightmost
  get-rightmost
  drop-rightmost
  concatenate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AssocMap

;; AssocMaps can be implemented using hash tables or balanced search trees.
;; Here, we use immutable AVL trees.

;; An AssocMap is an (AVL-Tree-of String)
;; See implementation of AVL trees below.

(define (fresh-assoc)
  (empty-avl-tree))

(define (assign key val assoc-map)
  (avl-tree-insert key val assoc-map))

(define (unassign key assoc-map)
  (avl-tree-delete-range key key assoc-map))

(define (lookup key assoc-map)
  (avl-tree-search key assoc-map))

(module+ test

  (define (a) (fresh-assoc))
  (check-not-exn a)
  (check-equal? (lookup 1 (a)) #false)
  (check-equal? (lookup 2 (a)) #false)

  (define (a+1=one) (assign 1 "one" (a)))
  (check-not-exn a+1=one)
  (check-equal? (lookup 1 (a+1=one)) "one")
  (check-equal? (lookup 2 (a+1=one)) #false)

  (define (a+1=one+2=two) (assign 2 "two" (a+1=one)))
  (check-not-exn a+1=one+2=two)
  (check-equal? (lookup 1 (a+1=one+2=two)) "one")
  (check-equal? (lookup 2 (a+1=one+2=two)) "two")

  (define (a+1=uno+2=two) (assign 1 "uno" (a+1=one+2=two)))
  (check-not-exn a+1=uno+2=two)
  (check-equal? (lookup 1 (a+1=uno+2=two)) "uno")
  (check-equal? (lookup 2 (a+1=uno+2=two)) "two")

  (define (a+1=uno) (unassign 2 (a+1=uno+2=two)))
  (check-not-exn a+1=uno)
  (check-equal? (lookup 1 (a+1=uno)) "uno")
  (check-equal? (lookup 2 (a+1=uno)) #false)

  (test-begin
    (for {[n (in-range 100)]}

      (define large
        (for/fold {[a (fresh-assoc)]} {[i (in-range n)]}
          (assign i (number->string i) a)))

      (for {[i (in-range n)]}
        (define msg (format "~a of ~a" i n))
        (check-equal? (lookup i large) (number->string i) msg))

      (define small
        (for/fold {[a large]} {[i (in-range n)] #:when (odd? i)}
          (unassign i a)))

      (for {[i (in-range n)]}
        (define msg (format "~a of ~a" i n))
        (cond
          [(odd? i) (check-equal? (lookup i small) #false msg)]
          [else (check-equal? (lookup i small) (number->string i) msg)]))

      (define medium
        (for/fold {[a small]} {[i (in-range n)] #:when (integer? (sqrt i))}
          (assign i "square" a)))

      (for {[i (in-range n)]}
        (define msg (format "~a of ~a" i n))
        (cond
          [(integer? (sqrt i)) (check-equal? (lookup i medium) "square" msg)]
          [(odd? i) (check-equal? (lookup i medium) #false msg)]
          [else (check-equal? (lookup i medium) (number->string i) msg)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set

;; Sets can be implemented using balanced search trees.
;; Here, we use immutable AVL trees.

;; A Set is an (AVL-Tree-of #true)
;; See implementation of AVL trees below.

(define (empty-set)
  (empty-avl-tree))

(define (in? elem set)
  (avl-tree-search elem set))

(define (extend elem set)
  (avl-tree-insert elem #true set))

(define (without elem1 elem2 set)
  (avl-tree-delete-range elem1 elem2 set))

(module+ test

  (define (s) (empty-set))
  (check-not-exn s)
  (check-equal? (in? 1 (s)) #false)
  (check-equal? (in? 2 (s)) #false)
  (check-equal? (in? 3 (s)) #false)
  (check-equal? (in? 4 (s)) #false)
  (check-equal? (in? 5 (s)) #false)
  (check-equal? (in? 6 (s)) #false)

  (define (s123) (extend 3 (extend 2 (extend 1 (s)))))
  (check-not-exn s123)
  (check-equal? (in? 1 (s123)) #true)
  (check-equal? (in? 2 (s123)) #true)
  (check-equal? (in? 3 (s123)) #true)
  (check-equal? (in? 4 (s123)) #false)
  (check-equal? (in? 5 (s123)) #false)
  (check-equal? (in? 6 (s123)) #false)

  (define (s123456) (extend 6 (extend 5 (extend 4 (s123)))))
  (check-not-exn s123456)
  (check-equal? (in? 1 (s123456)) #true)
  (check-equal? (in? 2 (s123456)) #true)
  (check-equal? (in? 3 (s123456)) #true)
  (check-equal? (in? 4 (s123456)) #true)
  (check-equal? (in? 5 (s123456)) #true)
  (check-equal? (in? 6 (s123456)) #true)

  (define (s126) (without 3 5 (s123456)))
  (check-not-exn s126)
  (check-equal? (in? 1 (s126)) #true)
  (check-equal? (in? 2 (s126)) #true)
  (check-equal? (in? 3 (s126)) #false)
  (check-equal? (in? 4 (s126)) #false)
  (check-equal? (in? 5 (s126)) #false)
  (check-equal? (in? 6 (s126)) #true)

  (test-begin
    (for {[n (in-range 100)]}
      (define n*1/5 (floor (* n 1/5)))
      (define n*2/5 (floor (* n 2/5)))
      (define n*3/5 (floor (* n 3/5)))
      (define n*4/5 (floor (* n 4/5)))

      (define big
        (for/fold {[s (empty-set)]} {[i (in-range n)]}
          (extend i s)))

      (for {[i (in-range n)]}
        (define msg (format "~a of ~a" i n))
        (check-equal? (in? i big) #true msg))

      (define small
        (without n*1/5 n*4/5 big))

      (for {[i (in-range n)]}
        (define msg (format "~a of ~a" i n))
        (cond
          [(<= n*1/5 i n*4/5) (check-equal? (in? i small) #false msg)]
          [else (check-equal? (in? i small) #true msg)]))

      (define medium
        (for/fold {[s small]} {[i (in-range n*2/5 (add1 n*3/5))]}
          (extend i s)))

      (for {[i (in-range n)]}
        (define msg (format "~a of ~a" i n))
        (cond
          [(<= n*2/5 i n*3/5) (check-equal? (in? i medium) #true msg)]
          [(<= n*1/5 i n*4/5) (check-equal? (in? i medium) #false msg)]
          [else (check-equal? (in? i medium) #true msg)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AVL Trees

;; An (AVL-Tree-of X) is either:
;; - empty
;; - (node Integer Number X (AVL-Tree-of X) (AVL-Tree-of X))
(struct node [height key value left right] #:transparent)
;; where:
;; - for any node L in left, L.key <= key
;; - for any node R in right, key <= R.key
;; - height is the height of the tree: 1 + max(left.height, right.height)
;; - left.height is at most 1 + right.height
;; - right.height is at most 1 + left.height

(define (empty-avl-tree)
  empty)

(define (avl-tree-search k t)
  (cond
    [(empty? t) #false]
    [(node? t)
     (define key (node-key t))
     (cond
       [(< k key) (avl-tree-search k (node-left t))]
       [(> k key) (avl-tree-search k (node-right t))]
       [(= k key) (node-value t)])]))

(define (avl-tree-insert k v t)
  (cond
    [(empty? t) (make-node k v empty empty)]
    [(node? t)
     (define key (node-key t))
     (cond
       [(< k key)
        (balance-node key (node-value t)
          (avl-tree-insert k v (node-left t))
          (node-right t))]
       [(> k key)
        (balance-node key (node-value t)
          (node-left t)
          (avl-tree-insert k v (node-right t)))]
       [(= k key)
        (make-node k v
          (node-left t)
          (node-right t))])]))

(define (avl-tree-delete-range k1 k2 t)
  (avl-tree-append
    (avl-tree-filter< k1 t)
    (avl-tree-filter> k2 t)))

(define (avl-tree-filter< k t)
  (cond
    [(empty? t) empty]
    [(node? t)
     (define key (node-key t))
     (cond
       [(<= k key) (avl-tree-filter< k (node-left t))]
       [(> k key)
        (define l (node-left t))
        (define v (node-value t))
        (define r (node-right t))
        (avl-tree-filter</append k r key v l)])]))

(define (avl-tree-filter</append k t left-k left-v left-t)
  (cond
    [(<< (height t) (height left-t))
     (balance-node (node-key left-t) (node-value left-t)
       (node-left left-t)
       (avl-tree-filter</append k t left-k left-v (node-right left-t)))]
    [(empty? t) (make-node left-k left-v left-t empty)]
    [(node? t)
     (define key (node-key t))
     (cond
       [(<= k key)
        (avl-tree-filter</append k (node-left t) left-k left-v left-t)]
       [(> k key)
        (define l (node-left t))
        (define v (node-value t))
        (define r (node-right t))
        (balance-node left-k left-v
          left-t
          (avl-tree-filter</append k r key v l))])]))

(define (avl-tree-filter> k t)
  (cond
    [(empty? t) empty]
    [(node? t)
     (define key (node-key t))
     (cond
       [(>= k key) (avl-tree-filter> k (node-right t))]
       [(< k key)
        (define l (node-left t))
        (define v (node-value t))
        (define r (node-right t))
        (avl-tree-filter>/append k l key v r)])]))

(define (avl-tree-filter>/append k t right-k right-v right-t)
  (cond
    [(<< (height t) (height right-t))
     (balance-node (node-key right-t) (node-value right-t)
       (avl-tree-filter>/append k t right-k right-v (node-left right-t))
       (node-right right-t))]
    [(empty? t) (make-node right-k right-v empty right-t)]
    [(node? t)
     (define key (node-key t))
     (cond
       [(>= k key)
        (avl-tree-filter>/append k (node-left t) right-k right-v right-t)]
       [(< k key)
        (define l (node-left t))
        (define v (node-value t))
        (define r (node-right t))
        (balance-node right-k right-v
          (avl-tree-filter>/append k l key v r)
          right-t)])]))

(define (avl-tree-append t1 t2)
  (cond
    [(empty? t1) t2]
    [(empty? t2) t1]
    [(< (height t1) (height t2))
     (balance-node (node-key t2) (node-value t2)
       (avl-tree-append t1 (node-left t2))
       (node-right t2))]
    [(< (height t2) (height t1))
     (balance-node (node-key t1) (node-value t1)
       (node-left t1)
       (avl-tree-append (node-right t1) t2))]
    [else
     (balance-node (node-key t1) (node-value t1)
       (node-left t1)
       (balance-node (node-key t2) (node-value t2)
         (avl-tree-append (node-right t1) (node-left t2))
         (node-right t2)))]))

(define (balance-node k v l r)
  (define delta (- (height l) (height r)))
  (cond
    [(= delta -2) (rotate-left k v l r)]
    [(<= -1 delta 1) (make-node k v l r)]
    [(= delta 2) (rotate-right k v l r)]
    [else (error 'balance-node "cannot balance ~v with ~v" l r)]))

(define (rotate-left k v l r)
  (define r.k (node-key r))
  (define r.v (node-value r))
  (define r.l (node-left r))
  (define r.r (node-right r))
  (cond
    [(> (height r.l) (height r.r))
     (rotate-left/right l k v r.l r.k r.v r.r)]
    [else
     (make-node r.k r.v (balance-node k v l r.l) r.r)]))

(define (rotate-right k v l r)
  (define l.k (node-key l))
  (define l.v (node-value l))
  (define l.l (node-left l))
  (define l.r (node-right l))
  (cond
    [(< (height l.l) (height l.r))
     (rotate-left/right l.l l.k l.v l.r k v r)]
    [else
     (make-node l.k l.v l.l (balance-node k v l.r r))]))

(define (rotate-left/right t1 k1 v1 t k2 v2 t2)
  (define k (node-key t))
  (define v (node-value t))
  (define l (node-left t))
  (define r (node-right t))
  (make-node k v
    (make-node k1 v1 t1 l)
    (make-node k2 v2 r t2)))

(define (make-node k v l r)
  (define h1 (height l))
  (define h2 (height r))
  (cond
    [(<< h1 h2) (error 'make-node "~v is too short for ~v" l r)]
    [(<< h2 h1) (error 'make-node "~v is too tall for ~v" l r)]
    [else
     (define h (add1 (max h1 h2)))
     (node h k v l r)]))

(define (height t)
  (cond
    [(empty? t) 0]
    [(node? t) (node-height t)]))

(define (<< x y)
  (< x (sub1 y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queue

;; Queues can be implemented using balanced trees or doubly-linked lists.
;; Here, we use doubly-linked lists (which can only be mutable).

;; A Queue is (queue Chain Chain).
(struct queue [leftmost rightmost] #:mutable #:transparent)
;; Either leftmost and rightmost are both empty, or they are both links.
;; If both are links, then they are part of a single chain of links:
;; * leftmost.left = empty
;; * rightmost.right = empty
;; * for any link L other than leftmost, L.left.right = L
;; * for any link L other than rightmost, L.right.left = L
;; * from any link L, leftmost is reachable by going left 0 or more times
;; * from any link L, rightmost is reachable by going right 0 or more times
;; These invariants must hold before and after any Queue operations.
;; Note that during Queue operations, in between mutations,
;; some or all of these invariants may be violated.

;; A Chain is either empty or (link Link String Link).
(struct link [left elem right] #:mutable #:transparent)

(define (new-queue)
  (queue empty empty))

(define (full? q)
  (and
    (link? (queue-leftmost q))
    (link? (queue-rightmost q))))

(define (single? q)
  (eq?
    (queue-leftmost q)
    (queue-rightmost q)))

(define (add-leftmost x q)
  (cond
    [(full? q)
     ;; If there is already a chain, update the leftmost link.
     (define old (queue-leftmost q))
     (define new (link empty x empty))
     (set-link-right! new old)
     (set-link-left! old new)
     (set-queue-leftmost! q new)]
    [else
     ;; Otherwise, start a chain from scratch.
     (define one (link empty x empty))
     (set-queue-leftmost! q one)
     (set-queue-rightmost! q one)])
  q)

(define (add-rightmost x q)
  (cond
    [(full? q)
     ;; If there is already a chain, update the rightmost link.
     (define old (queue-rightmost q))
     (define new (link empty x empty))
     (set-link-left! new old)
     (set-link-right! old new)
     (set-queue-rightmost! q new)]
    [else
     ;; Otherwise, start a chain from scrach.
     (define one (link empty x empty))
     (set-queue-rightmost! q one)
     (set-queue-leftmost! q one)])
  q)

(define (get-leftmost q)
  (link-elem (queue-leftmost q)))

(define (get-rightmost q)
  (link-elem (queue-rightmost q)))

(define (drop-leftmost q)
  (cond
    [(single? q)
     ;; If there is only one element, make the queue empty.
     (set-queue-leftmost! q empty)
     (set-queue-rightmost! q empty)]
    [else
     ;; Otherwise, just update the leftmost link.
     (define old (queue-leftmost q))
     (define new (link-right old))
     (set-link-left! new empty)
     (set-queue-leftmost! q new)])
  q)

(define (drop-rightmost q)
  (cond
    [(single? q)
     ;; If there is only one element, make the queue empty.
     (set-queue-rightmost! q empty)
     (set-queue-leftmost! q empty)]
    [else
     ;; Otherwise, just update the rightmost link.
     (define old (queue-rightmost q))
     (define new (link-left old))
     (set-link-right! new empty)
     (set-queue-rightmost! q new)])
  q)

(define (concatenate q1 q2)
  (cond
    ;; If either queue is empty, return the other.
    [(not (full? q1)) q2]
    [(not (full? q2)) q1]
    [else
     ;; Join the chains at the midpoint.
     (define r1 (queue-rightmost q1))
     (define l2 (queue-leftmost q2))
     (set-link-right! r1 l2)
     (set-link-left! l2 r1)
     ;; Update q1 to contain the combined chain.
     (define r2 (queue-rightmost q2))
     (set-queue-rightmost! q1 r2)
     (set-queue-leftmost! q2 empty)
     ;; Update q2 to be empty.
     (set-queue-rightmost! q2 empty)
     ;; Return q1.
     q1]))

(module+ test

  (define (q) (new-queue))
  (check-not-exn q)
  (check-equal? (full? (q)) #false)

  (define (qa) (add-leftmost "a" (q)))
  (check-not-exn qa)
  (check-equal? (full? (qa)) #true)
  (check-equal? (get-leftmost (qa)) "a")
  (check-equal? (get-rightmost (qa)) "a")

  (define (qab) (add-rightmost "b" (qa)))
  (check-not-exn qab)
  (check-equal? (full? (qab)) #true)
  (check-equal? (get-leftmost (qab)) "a")
  (check-equal? (get-rightmost (qab)) "b")

  (define (qcab) (add-leftmost "c" (qab)))
  (check-not-exn qcab)
  (check-equal? (full? (qcab)) #true)
  (check-equal? (get-leftmost (qcab)) "c")
  (check-equal? (get-rightmost (qcab)) "b")

  (define (qca) (drop-rightmost (qcab)))
  (check-not-exn qca)
  (check-equal? (full? (qca)) #true)
  (check-equal? (get-leftmost (qca)) "c")
  (check-equal? (get-rightmost (qca)) "a")

  (define (qc) (drop-rightmost (qca)))
  (check-not-exn qc)
  (check-equal? (full? (qc)) #true)
  (check-equal? (get-leftmost (qc)) "c")
  (check-equal? (get-rightmost (qc)) "c")
  (check-equal? (full? (drop-rightmost (qc))) #false)
  (check-equal? (full? (drop-leftmost (qc))) #false)

  (test-begin
    (for {[n (in-range 100)]}

      (define q (new-queue))

      (check-equal? (full? q) #false)

      (for {[i (in-range 100)]}
        (add-rightmost (number->string i) q)
        (check-equal? (full? q) #true)
        (check-equal? (get-rightmost q) (number->string i)))

      (for {[i (in-range 100)]}
        (check-equal? (full? q) #true)
        (check-equal? (get-leftmost q) (number->string i))
        (drop-leftmost q))

      (check-equal? (full? q) #false))))
