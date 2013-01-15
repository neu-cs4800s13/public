#lang racket

(provide
  cost-of
  define/cost
  cost-model
  with-cost-model
  current-cost-model)

(require
  (for-syntax
    racket/match
    racket/syntax
    syntax/parse
    syntax/id-table
    syntax/kerncase)
  syntax/srcloc
  syntax/location)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Classes

(begin-for-syntax

  (define-splicing-syntax-class optional-cost-model
    #:attributes {value}
    (pattern
      (~optional (~seq #:model value:expr)
        #:defaults {[value #'(current-cost-model)]}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Transformers

(begin-for-syntax

  (define (transform-expr/cost stx)
    (syntax-parse stx
      #:literal-sets {kernel-literals}

      [var:id
       (define/syntax-parse key
         (match (identifier-binding #'var)
           [(? list?) #'#:def]
           ['lexical #'#:var]
           [#false #'#:top]))
       #'(values (#%cost-record key 'key var) var)]

      [(#%expression ~! e:expr)
       #'(#%expression (#%expr/cost e))]

      [(quote ~! value)
       #'(let {[x 'value]}
           (values (#%cost-record #:const '#:const x) x))]

      [(if ~! Q:expr A:expr E:expr)
       #'(let-values {[{Q.cost Q.value} (#%expr/cost Q)]}
           (#%cost-add Q.cost (#%cost-record #:if '#:if Q.value)
             (if Q.value (#%expr/cost A) (#%expr/cost E))))]

      [(letrec-values ~! {[{lhs:id ...} rhs:expr] ...} body:expr ...+)
       (define/syntax-parse {rhs.cost ...}
         (generate-temporaries (attribute rhs)))
       #'(letrec-values {[{rhs.cost lhs ...} (#%expr/cost rhs)] ...}
           (#%cost-add
             rhs.cost ...
             (#%cost-record #:let '#:let (list lhs ...) ...)
             (#%body/cost body ...)))]

      [(let-values ~! {[{lhs:id ...} rhs:expr] ...} body:expr ...+)
       (define/syntax-parse {rhs.cost ...}
         (generate-temporaries (attribute rhs)))
       #'(let-values {[{rhs.cost lhs ...} (#%expr/cost rhs)] ...}
           (#%cost-add
             rhs.cost ...
             (#%cost-record #:let '#:let (list lhs ...) ...)
             (#%body/cost body ...)))]

      [(#%plain-app ~! fun:expr arg:expr ...)
       (define/syntax-parse {arg.cost ...}
         (generate-temporaries (attribute arg)))
       (define/syntax-parse {arg.value ...}
         (generate-temporaries (attribute arg)))
       #'(let-values
             {[{fun.cost fun.value} (#%expr/cost fun)]
              [{arg.cost arg.value} (#%expr/cost arg)]
              ...}
           (#%cost-add
             fun.cost
             arg.cost ...
             (#%cost-record #:call '#:call fun.value arg.value ...)
             (if (procedure/cost? fun.value)
               (let {[proc (procedure/cost-implementation fun.value)]}
                 (proc arg.value ...))
               (call-with-values
                 (lambda {} (fun.value arg.value ...))
                 (lambda results
                   (define cost (#%cost-record fun fun.value arg.value ...))
                   (apply values cost results))))))]

      [(begin ~! e:expr ... e0:expr)
       (define/syntax-parse {e.cost ...}
         (generate-temporaries (attribute e)))
       #'(let {[e.cost (#%cost-of (#%expr/cost e))] ...}
           (#%cost-add e.cost ... (#%expr/cost e0)))]

      [(begin0 ~! e0:expr e:expr ...)
       (define/syntax-parse {e.cost ...}
         (generate-temporaries (attribute e)))
       #'(call-with-values
           (lambda {} (#%expr/cost e0))
           (lambda {e0.cost . e0.values}
             (let {[e.cost (#%cost-of (#%expr/cost e))] ...}
               (#%cost-add e.cost ...
                 (apply values e0.cost e0.values)))))]

      [(set! ~! x:id e:expr)
       #'(let-values {[{e.cost e.value} (#%expr/cost e)]}
           (#%cost-add e.cost
             (values (#%cost-record #:set! '#:set! x e.value)
               (set! x e.value))))]

      [e (wrong-syntax #'e "cannot compute a cost for this expression")]))

  (define (transform-result/cost stx)
    (syntax-parse stx 
      #:literal-sets {kernel-literals}
      [(begin ~! defn:expr ... result:expr)
       #'(#%body/cost defn ... result)]
      [e #'(#%expr/cost e)]))

  (define (transform-defn/cost id-stx stx)
    (define/syntax-parse cost id-stx)
    (syntax-parse stx
      #:literal-sets {kernel-literals}
      [(begin ~! body:expr ...)
       (define/syntax-parse [temp ...]
         (generate-temporaries (attribute body)))
       #'(begin (#%defn/cost temp body) ...
           (define cost (cost-add temp ...)))]
      [(define-values ~! {name:id ...} body:expr)
       #'(define-values {cost name ...} (#%expr/cost body))]
      [(define-syntaxes ~! {name:id ...} body:expr)
       #'(begin
           (define cost 0)
           (define-syntaxes {name ...} body))]
      [e #'(define cost (#%cost-of (#%expr/cost e)))]))

  (define (wrap-expr/cost src stx)
    #`(call-with-values
        (lambda {} #,stx)
        (lambda {cost . results}
          (unless (proper-cost? cost)
            (error 'cs4800
              "~aInternal error: cost recorded as nonsense value: ~v"
              (source-location->prefix
                (quote-srcloc #,src))))
          (apply values cost results)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros

(define-syntax cost-of
  (syntax-parser
    [(_ model:optional-cost-model e:expr)
     #'(parameterize {[current-cost-model model.value]}
         (#%cost-of (#%expr/cost e)))]))

(define-syntax with-cost-model
  (syntax-parser
    [(_ model:expr body:expr ...+)
     #'(parameterize {[current-cost-model model]}
         body ...)]))

(define-syntax define/cost
  (syntax-parser
    [(_ model:optional-cost-model cost:id name:id body:expr)
     #'(define-values {cost name}
         (parameterize {[current-cost-model model.value]}
           (#%expr/cost body)))]
    [(_ (head . args) body:expr ...+)
     #'(define head
         (lambda/cost args body ...))]))

(define-syntax lambda/cost
  (syntax-parser
    [(_ args body:expr ...+)
     #'(procedure/cost
         (lambda args
           (#%body/cost body ...)))]))

(define-syntax #%expr/cost
  (syntax-parser
    [(_ e:expr)
     (wrap-expr/cost #'e
       (transform-expr/cost
         (local-expand #'e 'expression (list #'#%top))))]))

(define-syntax #%body/cost
  (syntax-parser
    [(_ defn:expr ... result:expr)
     (define/syntax-parse {cost ...}
       (generate-temporaries (attribute defn)))
     #'(begin
         (#%defn/cost cost defn) ...
         (#%cost-add cost ... (#%result/cost result)))]))

(define-syntax #%result/cost
  (syntax-parser
    [(_ result:expr)
     (transform-result/cost
       (local-expand #'result (syntax-local-context) #false))]))

(define-syntax #%defn/cost
  (syntax-parser
    [(_ cost:id defn:expr)
     (transform-defn/cost #'cost
       (local-expand #'defn (syntax-local-context) #false))]))

(define-syntax #%cost-add
  (syntax-parser
    [(_ cost:expr ... body:expr)
     (define/syntax-parse {x ...}
       (generate-temporaries (attribute cost)))
     #'(let {[x cost] ...}
         (call-with-values
           (lambda {} body)
           (lambda {y . results}
             (apply values (cost-add x ... y) results))))]))

(define-syntax #%cost-of
  (syntax-parser
    [(_ e:expr)
     #'(call-with-values
         (lambda {} (#%expression e))
         (lambda {cost . results} cost))]))

(define-syntax #%values-of
  (syntax-parser
    [(_ e:expr)
     #'(call-with-values
         (lambda {} (#%expression e))
         (lambda {cost . results} (apply values results)))]))

(define-syntax #%cost-record
  (syntax-parser
    [(_ name key:expr arg:expr ...)
     #'(cost-record 'name key arg ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Types

(struct procedure/cost [implementation]
  #:property prop:procedure
  (make-keyword-procedure
    (lambda {keys vals proc/cost . args}
      (define proc (procedure/cost-implementation proc/cost))
      (#%values-of (keyword-apply proc keys vals args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(define current-cost-model (make-parameter (hasheq)))

(define cost-model
  (make-keyword-procedure
    (lambda (keys key-vals . args)
      (define-values {funs fun-vals}
        (split-arguments 'cost-model args))
      (for {[fun (in-list funs)]
            [index (in-range 0 +inf.0 2)]}
        (unless (procedure? fun)
          (error 'cost-model
            "expected a procedure as (optional) argument ~a, but got ~s in: ~s"
            index
            fun
            args)))
      (foldl
        (lambda (fun/key val model)
          (dict-set model fun/key val))
        (hasheq)
        (append funs keys)
        (append fun-vals key-vals)))))

(define (split-arguments name xs0)
  (let loop {[xs xs0]}
    (match xs
      ['() (values '() '())]
      [(list* x y zs)
       (define-values {xs ys} (loop zs))
       (values (cons x xs) (cons y ys))]
      [_ (error name
           "expected an even number of arguments, but got ~a arguments: ~s"
           (length xs0)
           xs0)])))

(define (cost-record name key . args)
  (define default
    (cond
      [(procedure? key) #false]
      [(keyword? key) 0]))
  (define cost
    (dict-ref (current-cost-model) key default))
  (cond
    [(false? cost) name]
    [(number? cost) cost]
    [(procedure? cost) (apply cost args)]))

(define cost-add
  (case-lambda
    [{} 0]
    [{cost} cost]
    [{one two} (cost-add2 one two)]
    [{one . more}
     (for/fold {[one one]} {[two (in-list more)]}
       (cost-add2 one two))]))

(define (cost-add2 one two)
  (build-cost
    (merge-addends
      (abstract-cost one)
      (abstract-cost two))
    (+ (concrete-cost one)
       (concrete-cost two))))

(define (build-cost abstract concrete)
  (cond
    [(empty? abstract) concrete]
    [(zero? concrete)
     (cond
       [(singleton? abstract) (first abstract)]
       [else (append (list '+) abstract)])]
    [else (append (list '+) abstract (list concrete))]))

(define (concrete-cost cost)
  (cond
    [(number? cost) cost]
    [(name? cost) 0]
    [(product? cost) 0]
    [(sum? cost) (concrete-addend (sum-addends cost))]))

(define (sum-addends sum) (rest sum))

(define (concrete-addend adds)
  (define add (last adds))
  (if (number? add) add 0))

(define (abstract-cost cost)
  (cond
    [(number? cost) empty]
    [(name? cost) (list cost)]
    [(product? cost) (list cost)]
    [(sum? cost) (abstract-addends (sum-addends cost))]))

(define (abstract-addends adds)
  (cond
    [(number? (last adds)) (drop-right adds 1)]
    [else adds]))

(define (merge-addends ones twos)
  (cond
    [(empty? ones) twos]
    [(empty? twos) ones]
    [else
     (define one (first ones))
     (define other-ones (rest ones))
     (define name (addend-name one))
     (define factor-one (addend-factor one))
     (define-values {factor-two other-twos}
       (extract-addend name twos))
     (cons (addend (+ factor-one factor-two) name)
       (merge-addends other-ones other-twos))]))

(define (extract-addend name0 adds0)
  (let/ec return
    (let loop {[adds adds0]}
      (cond
        [(empty? adds) (return 0 adds0)]
        [else
         (define add (first adds))
         (define others (rest adds))
         (define name (addend-name add))
         (define factor (addend-factor add))
         (cond
           [(eq? name name0) (values factor others)]
           [else
            (define-values {factor remainder} (loop others))
            (values factor (cons add remainder))])]))))

(define (addend-name add)
  (cond
    [(product? add) (third add)]
    [else add]))

(define (addend-factor add)
  (cond
    [(product? add) (second add)]
    [else 1]))

(define (addend factor name)
  (cond
    [(= factor 1) name]
    [else (list '* factor name)]))

(define (product? x)
  (and (list? x)
    (eq? (first x) '*)))

(define (sum? x)
  (and (list? x)
    (eq? (first x) '+)))

(define (name? x)
  (or (symbol? x)
    (keyword? x)))

(define (singleton? x)
  (and (cons? x) (empty? (rest x))))

(define (proper-cost? x)
  (or (number? x)
    (proper-sum? x)
    (proper-product? x)
    (proper-name? x)))

(define (proper-sum? x)
  (match x
    [(list '+ adds ...)
     (proper-addends? adds)]
    [_ #false]))

(define (proper-addends? x)
  (cond
    [(empty? x) #true]
    [(singleton? x)
     (or (number? (first x))
       (proper-addend? (first x)))]
    [(cons? x)
     (and (proper-addend? (first x))
       (proper-addends? (rest x)))]))

(define (proper-addend? x)
  (or (proper-product? x)
    (proper-name? x)))

(define (proper-product? x)
  (match x
    [(list '* factor name)
     (and (exact-positive-integer? factor)
       (proper-name? name))]
    [_ #false]))

(define (proper-name? x) (name? x))
