#lang racket

;; A Hashtable is (ht Array-of-List-of-Entry Number)
;; count is the number of entries in the hashtable
(struct hashtable [contents count] #:mutable)

;; An Entry is (entry Key Any)
(struct entry [key value] #:mutable)

;; A Key is a Number (integer)

;; load-factor-limit : Number
;; Resize hashtable if its load-factor is above this limit.
(define load-factor-limit 3/4)

;; init-size : Number
;; Initial size of hashtable array.
(define init-size 7)

;; new-hashtable : -> Hashtable
(define (new-hashtable)
  (hashtable (make-vector init-size empty) 0))

;; hash-code : Key Number -> Number
;; returns index >= 0 and < size
(define (hash-code key size)
  (modulo key size))

;; find-entry : Key Listof-Entry -> (U Entry false)
(define (find-entry key entries)
  (cond [(pair? entries)
         (define first-entry (first entries))
         (cond [(equal? key (entry-key (first entries)))
                (first entries)]
               [else (find-entry key (rest entries))])]
        [(empty? entries) false]))

;; hashtable-get : Hashtable Key Any -> Any
(define (hashtable-get ht key not-found-val)
  (define array (hashtable-contents ht))
  (define index (hash-code key (vector-length array)))
  (define entries-at-index (vector-ref array index))
  (define maybe-entry (find-entry key entries-at-index))
  (cond [(entry? maybe-entry)
         (entry-value maybe-entry)]
        [else
         not-found-val]))

;; hashtable-load-factor : Hashtable -> Number
(define (hashtable-load-factor ht)
  (/ (hashtable-count ht)
     (vector-length (hashtable-contents ht))))

;; hashtable-set! : Hashtable Key Any -> Void
(define (hashtable-set! ht key new-val)
  (define array (hashtable-contents ht))
  (define index (hash-code key (vector-length array)))
  (define entries-at-index (vector-ref array index))
  (define maybe-entry (find-entry key entries-at-index))
  (cond [(entry? maybe-entry)
         (set-entry-value! maybe-entry new-val)]
        [else
         (define new-entry (entry key new-val))
         (define new-entries-at-index (cons new-entry entries-at-index))
         (vector-set! array index new-entries-at-index)
         (set-hashtable-count! ht (+ 1 (hashtable-count ht)))
         ;; ht contains new entry, has correct count
         ;; now check if it needs to be resized
         (define new-load-factor (hashtable-load-factor ht))
         (cond [(<= new-load-factor load-factor-limit) ;; do not resize
                (void)]
               [else ;; must resize (make array larger)
                (hashtable-resize-up! ht)])]))

#|
;; hashtable-resize-up! : Hashtable -> Void
(define (hashtable-resize-up! ht)
  (define array (hashtable-contents ht))
  (define new-array (make-vector (* 2 (vector-length array)) empty))
  ;; for each index in (old) array, get list of entries
  ;;   for each entry in list:
  ;;     insert into new-array at index computed by hash-code
  ;;     with new-array's length
  ....)
|#

;; ----

;; hashtable-resize-up! : Hashtable -> Void
(define (hashtable-resize-up! ht)
  (define old-array (hashtable-contents ht))
  (define new-array (make-vector (* 2 (vector-length old-array)) empty))
  (rehash-array! old-array 0 new-array)
  (set-hashtable-contents! ht new-array))

;; rehash-array! : Array-of-List-of-Entry Number Array-of-List-of-Entry -> Void
;; Rehashes all of the entries in old-array from start-index to the end.
(define (rehash-array! old-array start-index new-array)
  (cond [(< start-index (vector-length old-array))
         (define entries (vector-ref old-array start-index))
         (rehash-entries! entries new-array)
         (rehash-array! old-array (+ 1 start-index) new-array)]
        [else (void)]))

;; rehash-entries! : List-of-Entry Array-of-List-of-Entry -> Void
(define (rehash-entries! entries new-array)
  (cond [(cons? entries)
         (rehash-entry! (first entries) new-array)
         (rehash-entries! (rest entries) new-array)]
        [(empty? entries)
         (void)]))

;; rehash-entry! : Entry Array-of-List-of-Entry -> Void
(define (rehash-entry! en new-array)
  (define index (hash-code (entry-key en) (vector-length new-array)))
  (define entries-already-rehashed (vector-ref new-array index))
  (vector-set! new-array index (cons en entries-already-rehashed)))

;; ----

;; run tests using "raco test $thisfile"
(module* test #f
  (require rackunit)

  (test-case "basic tests"
    (define ht (new-hashtable))
    (hashtable-set! ht 1 1)
    (hashtable-set! ht 2 2)
    (hashtable-set! ht 8 8)
    (check-equal? (hashtable-get ht 1 #f) 1)
    (check-equal? (hashtable-get ht 2 #f) 2)
    (check-equal? (hashtable-get ht 8 #f) 8)
    (check-equal? (hashtable-get ht 9 #f) #f)
    (check-equal? (hashtable-get ht 9 'no) 'no))

  (test-case "load-factor stays below limit"
    (define ht (new-hashtable))
    (for ([i (in-range 1000)])
      (hashtable-set! ht i i)
      (check-true (<= (hashtable-load-factor ht) load-factor-limit))))

  (test-case "agrees with association list, many random keys"
    (define ht (new-hashtable))
    (define al empty)

    (for ([i (in-range 1000)])
      (let ([key (random 1000)])
        (hashtable-set! ht key i)
        (set! al (cons (cons key i) al))))

    (for ([key (in-range 1000)])
      (let ([ht-val (hashtable-get ht key #f)]
            [al-val (cond [(assoc key al) => cdr]
                          [else #f])])
        (check-equal? ht-val al-val))))

  )
