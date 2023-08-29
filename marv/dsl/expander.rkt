#lang racket/base

(require (for-syntax racket/base syntax/parse))

(define VARS (make-parameter (hash)))

(define (set-var id v) (VARS (hash-set (VARS) id v)))
(define (get-var id) (hash-ref (VARS) id))
(define (set-res id drv attr v) (set-var id (hash-set* v '$driver drv '$type attr)))

(require marv/dsl/support)
(require marv/utils/hash)
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
           (require marv/dsl/support)
           (require marv/utils/hash)
           (require racket/hash)
           STMT ...
           (define resources (gen-resources (VARS)))
           (define drivers (gen-drivers (VARS)))
           (provide resources drivers)
           (require racket/pretty)
           (pretty-print (VARS)))]
      [else (raise "nowt")]))

  (define (m-statement stx)
    (syntax-parse stx
      [(_ STMT) #'STMT]
      [else (raise "nowt-stmt")]))

  (define (m-decl stx)
    (syntax-parse stx
      [(_ DECL) #'DECL]
      [else (raise "nowt-decl")]))

  (define (m-var-decl stx)
    (syntax-parse stx
      [(_ id EXPR) (syntax/loc stx (set-var id EXPR))]
      [else (raise "nowt-var-decl")]))

  (define (m-built-in stx)
    (syntax-parse stx
      [(_ BUILTIN) #'BUILTIN]
      [else (raise "nowt-builtin")]))

  (define (m-env-read stx)
    (syntax-parse stx
      [(_ env-var:string) (syntax/loc stx (getenv-or-raise env-var))]
      [else (raise "m-env-read")]))

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

  (define (m-config-expr stx)
    (syntax-parse stx
      [(_ CFEXPR) #'CFEXPR]
      [else (raise "m-config-expr")]))

  (define (m-config-merge stx)
    (syntax-parse stx
      [(_ CFLEFT CFRIGHT) (syntax/loc stx (hash-merge CFLEFT CFRIGHT))]
      [else (raise "m-config-merge")]))

  (define (m-config-ident stx)
    (syntax-parse stx
      [(_ CFIDENT) (syntax/loc stx (get-var CFIDENT))]
      [else (raise "m-config-ident")]))

  (define (m-attr-decl stx)
    (syntax-parse stx
      [(_ att-name:string ((~literal expression) EXPR))
       (syntax/loc stx `(,(string->symbol att-name) . ,(expression EXPR)))]
      [(_ att-name:string ((~literal reference) REF))
       (syntax/loc stx `(,(string->symbol att-name) . ,(ref (string->symbol REF))))]
      [(_ att-name:string IDENT)
       (syntax/loc stx `(,(string->symbol att-name) . ,(get-var IDENT)))]

      ; TODO - immutable stuff in the syntax is just temporary until moved to the driver
      [(_ att-name:string "imm:" ((~literal expression) EXPR))
       (syntax/loc stx `(,(string->symbol att-name) . ,(ival (expression EXPR))))]
      [(_ att-name:string "imm:" ((~literal reference) REF))
       (syntax/loc stx `(,(string->symbol att-name) . ,(iref (string->symbol REF))))]
      [(_ att-name:string "imm:" IDENT)
       (syntax/loc stx `(,(string->symbol att-name) . ,(ival (get-var IDENT))))]
      [else (raise "m-attr-decl")]))

  (define (m-res-decl stx)
    (syntax-parse stx
      [(_ name:string ((~literal driver-id) did:string)
          ((~literal driver-attr) dad:string) cfg)
       (syntax/loc stx (set-res name did dad cfg))]
      [else (raise (format "res-decl didn't match: ~a" stx))]))

  )

(define-syntax marv-spec m-marv-spec)
(define-syntax statement m-statement)
(define-syntax decl m-decl)
(define-syntax var-decl m-var-decl)
(define-syntax res-decl m-res-decl)
(define-syntax expression m-expression)
(define-syntax config-object m-config-object)
(define-syntax alist m-alist)
(define-syntax attr-decl m-attr-decl)
(define-syntax built-in m-built-in)
(define-syntax env-read m-env-read)
(define-syntax boolean m-boolean)
(define-syntax config-expr m-config-expr)
(define-syntax config-merge m-config-merge)
(define-syntax config-ident m-config-ident)

(provide marv-spec decl var-decl res-decl
         expression statement config-object alist
         config-expr config-merge config-ident
         attr-decl built-in env-read boolean)
