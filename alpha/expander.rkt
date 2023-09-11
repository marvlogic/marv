#lang racket/base

(require racket/hash)
(require racket/string)
(require (for-syntax racket/base syntax/parse))
(require marv/utils/hash)

(require racket/pretty)

(define VARS (make-parameter (hash)))

(define (error:excn msg)
  (raise (format "ERROR: ~a\n at ~a:~a" msg 1 2))) ;(syntax-source stx) (syntax-line stx)))

(define (set-var id v)
  (when (hash-has-key? (VARS) id) (error:excn (format "~a is already defined" id)))
  (VARS (hash-set (VARS) id v)))

; todo - out
(define (get-var id) (hash-ref (VARS) id))

(define (def-res id drv attr v)
  (define r (hash-set* v '$driver drv '$type (string-join (map symbol->string attr) ".")))
  (set-var id r)
  r)

(define (config-overlay left right) (hash-union left right #:combine (lambda (v0 _) v0)))
; (define (config-take cfg attrs) (hash-take cfg attrs))

(define (hash-nref hs ks)
  (for/fold ([h hs])
            ([k (in-list ks)])
    (hash-ref h k)))

(define (handle-ref id tgt . ks)
  (define ksx (map syntax-e ks))
  (define p (string->symbol (string-join (map symbol->string (cons id ksx)) ".")))
  (cond [(hash-has-key? tgt '$driver) (ref p)]
        [else (hash-nref tgt ksx)]))

; (define (handle-deref r) #`(hash-nref #,(car r) #,(cdr r)))

(define (loop-res-name name loop-ident)
  (format "~a.~a" name (get-var loop-ident)))

(require marv/alpha/support)
(require marv/core/values)

(define (resource-var? id)
  (define v (hash-ref (VARS) id))
  (and (hash? v) (hash-has-key? v '$driver)))

; TODO - swap prefix usage to m- on the provided

(begin-for-syntax
  ; (define (m-marv-spec stx)
  ;   (syntax->datum (m-marv-spec2 stx)))

  (define (m-marv-spec stx)
    (syntax-parse stx
      [(_ STMT ...)
       #'(begin
           (require marv/alpha/support)
           (require racket/hash)
           (require racket/pretty)
           STMT ...
           (define resources (gen-resources (VARS)))
           (define drivers (gen-drivers (VARS)))
           (provide resources drivers)
           ;  (pretty-print (VARS))
           )]
      [else (raise "nowt")]))

  (define (m-statement stx)
    (syntax-parse stx
      [(_ STMT) (syntax/loc stx STMT)]
      [else (raise "nowt-stmt")]))

  (define (m-decl stx)
    (syntax-parse stx
      [(_ DECL) (syntax/loc stx DECL)]
      [else (raise "nowt-decl")]))

  (define (m-var-decl stx)
    (syntax-parse stx
      [(_ id:expr EXPR) (syntax/loc stx (define id EXPR))]
      [else (raise "nowt-var-decl")]))

  (define (m-config-func-decl stx)
    (syntax-parse stx
      [(_ id:expr param ... CONF-OBJ)
       (syntax/loc stx
         (define (id param ...) CONF-OBJ))]
      [else (raise "config-func-decl")]))

  (define (m-config-func-call stx)
    (syntax-parse stx
      [(_ func:id param ...)
       (syntax/loc stx (func param ...))]
      [else (raise "config-func-call")]))

  ; (define (m-config-func-param stx)
  ;   (syntax-parse stx
  ;     [(_ id:expr)
  ;      (syntax/loc stx
  ;        (define id EXPR))]
  ;     [else (raise "nowt-var-decl")]))


  (define (m-built-in stx)
    (syntax-parse stx
      [(_ BUILTIN) (syntax/loc stx BUILTIN)]
      [else (raise "nowt-builtin")]))

  (define (m-env-read stx)
    (syntax-parse stx
      [(_ env-var:string) (syntax/loc stx (getenv-or-raise env-var))]
      [else (raise "m-env-read")]))

  (define (m-strf stx)
    (syntax-parse stx
      [(_ str:string expr ... ) (syntax/loc stx (format str expr ...))]
      [else (raise "m-strf")]))

  (define (m-pprint stx)
    (syntax-parse stx
      [(_ ident:expr) (syntax/loc stx (pretty-print ident))]))

  (define (m-expression stx)
    (syntax-parse stx
      [(_ val:integer) (syntax/loc stx val)]
      [(_ val:string) (syntax/loc stx val)]
      [(_ other) (syntax/loc stx other)]
      [else (raise (format "expression didn't match: ~a" stx))]
      ))

  (define (m-boolean stx)
    (syntax-parse stx
      [(_ "true") (syntax/loc stx val) #'#t]
      [(_ "false") (syntax/loc stx val) #'#f]))

  (define (m-config-object stx)
    (syntax-parse stx
      [(_ ATTR ...)
       (syntax/loc stx (make-immutable-hasheq (list ATTR ...)))]
      [else (raise "m-config-object")]))

  (define (m-alist stx)
    (syntax-parse stx
      [(_ EXPR ...)
       (syntax/loc stx (list EXPR ...))]
      [else (raise "m-alist")]))

  (define (m-list-attr stx)
    (syntax-parse stx
      [(_ ATTR ...)
       (syntax/loc stx (list 'ATTR ...))]
      [else (raise "m-list-attr")]))

  (define (m-config-expr stx)
    (syntax-parse stx
      [(_ CFEXPR) #'CFEXPR]
      [else (raise "m-config-expr")]))

  (define (m-config-merge stx)
    (syntax-parse stx
      [(_ LEFT "->" RIGHT) (syntax/loc stx (config-overlay LEFT RIGHT))]
      [(_ LEFT "<-" RIGHT) (syntax/loc stx (config-overlay RIGHT LEFT))]
      [else (raise "m-config-merge")]))

  (define (m-config-take stx)
    (syntax-parse stx
      [(_ CFEXPR ATTRLIST) (syntax/loc stx (hash-take CFEXPR ATTRLIST))]
      ; [(_ LEFT "<-" RIGHT) (syntax/loc stx (config-overlay RIGHT LEFT))]
      [else (raise "m-config-take")]))


  (define (m-config-ident stx)
    (syntax-parse stx
      [(_ CFIDENT) (syntax/loc stx CFIDENT)]
      [else (raise "m-config-ident")]))

  (define (m-attr-decl stx)
    (syntax-parse stx
      [(_ att-name:expr ((~literal expression) EXPR))
       (syntax/loc stx `(att-name . ,(expression EXPR)))]
      [(_ att-name:expr ((~literal reference) REF))
       (syntax/loc stx `(att-name . ,(reference REF)))]
      [(_ att-name:expr ident:id)
       (syntax/loc stx `(att-name . ,ident))]
      [(_ att-name:expr IDENT)
       (syntax/loc stx `(att-name . ,(get-var IDENT)))]

      ; TODO - immutable stuff in the syntax is just temporary until moved to the driver
      [(_ att-name:expr "imm:" ((~literal expression) EXPR))
       (syntax/loc stx `(att-name . ,(ival (expression EXPR))))]
      [(_ att-name:expr "imm:" ((~literal reference) REF))
       (syntax/loc stx `(att-name . ,(ival (reference REF))))]
      [(_ att-name:expr "imm:" ident:id)
       (syntax/loc stx `(att-name . ,(ival ident)))]
      [(_ att-name:expr "imm:" IDENT)
       (syntax/loc stx `(att-name . ,(ival (get-var IDENT))))]
      [else (raise "m-attr-decl")]))

  (define (m-reference stx)
    (syntax-parse stx
      [(_ (tgt:id key ...)) (syntax/loc stx (handle-ref 'tgt tgt #'key ...))]
      ; [(_ (tgt:id key)) (syntax/loc stx (handle-ref tgt (syntax-e #'key)))]
      ; [(_ (tgt:id key)) (syntax/loc stx (hash-ref tgt (syntax-e #'key)))]
      ; [(_ (tgt:id key)) #'(hash-ref tgt (syntax-e #'key))]
      ))

  (define (m-res-decl stx)
    (syntax-parse stx
      [(_ name:expr
          ((~literal driver-id) did:expr)
          ((~literal driver-attr) dad:expr) cfg)
       (syntax/loc stx (define name (def-res 'name 'did 'dad cfg)))]

      [(_ name:string
          ((~literal loop-ident) lid:string)
          ((~literal driver-id) did:string)
          ((~literal driver-attr) dad:string) cfg)
       (syntax/loc stx (set-res (loop-res-name name lid) did dad cfg))]
      [else (raise (format "res-decl didn't match: ~a" stx))]))

  (define (m-for-list stx)
    (syntax-parse stx
      [(_ LOOPVAR "{" STMT ... "}")
       (syntax/loc stx `(for/list LOOPVAR (STMT ...)))]
      [else (raise (format "for-list didn't match: ~a" stx))])
    )

  (define (m-loop-var stx)
    (syntax-parse stx
      [(_ IDENT:string LIST) (syntax/loc stx `[ ,(string->symbol IDENT) ,LIST])]
      [else (raise (format "loop-var didn't match: ~a" stx))]))

  )

(define-syntax marv-spec m-marv-spec)
(define-syntax statement m-statement)
(define-syntax decl m-decl)
(define-syntax var-decl m-var-decl)
(define-syntax config-func-decl m-config-func-decl)
(define-syntax res-decl m-res-decl)
(define-syntax config-func-call m-config-func-call)
(define-syntax expression m-expression)
(define-syntax reference m-reference)
(define-syntax config-object m-config-object)
(define-syntax alist m-alist)
(define-syntax list-attr m-list-attr)
(define-syntax attr-decl m-attr-decl)
(define-syntax built-in m-built-in)
(define-syntax env-read m-env-read)
(define-syntax strf m-strf)
(define-syntax pprint m-pprint)
(define-syntax boolean m-boolean)
(define-syntax config-expr m-config-expr)
(define-syntax config-merge m-config-merge)
(define-syntax config-take m-config-take)
(define-syntax config-ident m-config-ident)
(define-syntax for-list m-for-list)
(define-syntax loop-var m-loop-var)

(provide marv-spec decl var-decl res-decl config-func-call config-func-decl
         expression reference statement config-object alist list-attr
         config-expr config-merge config-ident config-take
         for-list loop-var
         attr-decl built-in env-read pprint strf
         boolean)
