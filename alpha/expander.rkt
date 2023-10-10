#lang racket/base

(require racket/string)
(require racket/syntax)
(require (for-syntax racket/base racket/syntax syntax/parse racket/pretty))

(require racket/pretty)

(require marv/alpha/support)
(require marv/core/values)
(require marv/log)

; TODO - swap prefix usage to m- on the provided

(begin-for-syntax
  ; (define (m-marv-spec stx)
  ;   (syntax->datum (m-marv-spec2 stx)))

  (define (m-marv-spec stx)
    (syntax-parse stx
      [(_ MODULE ...)
       #'(begin
           (require marv/alpha/support)
           (require racket/hash)
           (require racket/pretty)
           MODULE ...
           (define drivers (gen-drivers (hash)))
           (provide drivers)
           )]
      [else (raise "nowt")]))

  (define (m-marv-module stx)
    (syntax-parse stx
      ; TODO - handle no-params case
      [(_ mod-id:expr "(" PARAMS ... ")" STMT ...
          (~optional (~seq "return" RETURN) #:defaults ([RETURN #'void])))
       (syntax/loc stx
         (begin
           (define (mod-id resid-prefix mkres params)
             (log-marv-debug "** Generating module: ~a=~a(~a)" resid-prefix 'mod-id params)
             (define rs
               (with-module-ctx resid-prefix params
                 (lambda ()
                   PARAMS ...
                   STMT ...
                   (define rs (gen-resources mkres))
                   RETURN
                   rs
                   )))
             (log-marv-debug "** generation completed for ~a.~a" resid-prefix 'mod-id)
             rs)
           (provide mod-id)))]
      [else (raise "invalid module spec m-marv-module")]))

  (define (m-module-parameter stx)
    (syntax-parse stx
      [(_ PARAMETER) (syntax/loc stx (define PARAMETER (get-param 'PARAMETER)))]))

  (define (m-module-return stx)
    (syntax-parse stx
      [(_ "return" RETURNS ...) (syntax/loc stx (set-return (make-immutable-hasheq (list RETURNS ...))))]
      [else (raise "m-module-return f*")]))

  (define (m-module-import stx)
    (syntax-parse stx
      [(_ FILENAME) (syntax/loc stx (require FILENAME)) ]
      [(_ FILENAME "as" ALIAS) #`(require (prefix-in #,(format-id #f "~a/" #`ALIAS) FILENAME)) ]
      [else (raise "m-import")]))

  (define (m-return-parameter stx)
    (syntax-parse stx
      [(_ NAME VALUE) (syntax/loc stx (cons 'NAME VALUE))]))

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

  (define (m-type-decl stx)
    (syntax-parse stx
      [(_ ((~literal driver-id) did:expr)
          ((~literal driver-attr) datr:expr) body)
       (syntax/loc stx (register-type 'did 'datr (make-immutable-hash body)))]))

  (define (m-type-body stx)
    (syntax-parse stx
      [(_ crud-decl ...) (syntax/loc stx (list crud-decl ...))]))

  (define (m-type-crud-decl stx)
    (syntax-parse stx
      [(_ "create" SPEC) (syntax/loc stx (cons 'create SPEC))]
      [(_ "delete" SPEC) (syntax/loc stx (cons 'delete SPEC))]))

  (define (m-type-api-spec stx)
    (syntax-parse stx
      [(_ ((~literal driver-attr) datr:expr) xform-id:identifier)
       (syntax/loc stx `(datr ,xform-id))]
      [else (raise "api-spec")]))

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
      [(_ passthru) (syntax/loc stx passthru)]
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
      ; TODO - why not support fn?
      [(_ CFEXPR ATTRLIST) (syntax/loc stx (hash-take CFEXPR ATTRLIST))]
      [else (raise "m-config-take")]))

  (define (m-config-ident stx)
    (syntax-parse stx
      [(_ CFIDENT) (syntax/loc stx CFIDENT)]
      [else (raise "m-config-ident")]))

  (define (m-keyword stx)
    (syntax-parse stx
      [(_ keyword) (syntax/loc stx keyword)]))

  (define (m-attr-decl stx)
    (syntax-parse stx
      [(_ att-name:string ((~literal expression) EXPR))
       (syntax/loc stx `(,(string->symbol att-name) . ,(expression EXPR)))]
      [(_ att-name:expr ((~literal expression) EXPR))
       (syntax/loc stx `(att-name . ,(expression EXPR)))]

      ; TODO - immutable stuff in the syntax is just temporary until moved to the driver
      [(_ att-name:string ((~literal expression) EXPR))
       (syntax/loc stx `(,(string->symbol att-name) . ,(ival (expression EXPR))))]
      [(_ att-name:expr "imm:" ((~literal expression) EXPR))
       (syntax/loc stx `(att-name . ,(ival (expression EXPR))))]
      [else (raise "m-attr-decl")]))

  (define (m-reference stx)
    (syntax-parse stx
      [(_ (tgt:id key ...)) (syntax/loc stx (handle-ref tgt 'tgt #'key ...))]
      ))

  (define (m-res-decl stx)
    (syntax-parse stx
      [(_ name:expr
          ((~literal driver-id) did:expr)
          ((~literal driver-attr) dad:expr) cfg)
       (syntax/loc stx (define name (def-res 'name 'did 'dad cfg)))]

      ; [(_ name:string
      ;     ((~literal loop-ident) lid:string)
      ;     ((~literal driver-id) did:string)
      ;     ((~literal driver-attr) dad:string) cfg)
      ;  (syntax/loc stx (set-res (loop-res-name name lid) did dad cfg))]
      [else (raise (format "res-decl didn't match: ~a" stx))]))

  (define (m-module-invoke stx)
    (syntax-parse stx
      [(_ var-id:expr mod-id:expr PARAMS ...)
       (syntax/loc stx
         (define var-id (module-call 'var-id mod-id (make-immutable-hasheq (list PARAMS ...)) )))]))

  (define (m-named-parameter stx)
    (syntax-parse stx
      ; NB string has to be first!
      [(_ param-name:string EXPR) (syntax/loc stx (cons (string->symbol param-name) EXPR))]
      [(_ param-name:expr EXPR) (syntax/loc stx (cons 'param-name EXPR))]
      ))
  )

(define-syntax marv-spec m-marv-spec)
(define-syntax marv-module m-marv-module)
(define-syntax module-parameter m-module-parameter)
(define-syntax module-return m-module-return)
(define-syntax return-parameter m-return-parameter)
(define-syntax module-import m-module-import)
(define-syntax statement m-statement)
(define-syntax decl m-decl)
(define-syntax var-decl m-var-decl)
(define-syntax config-func-decl m-config-func-decl)

(define-syntax type-decl m-type-decl)
(define-syntax type-body m-type-body)
(define-syntax type-crud-decl m-type-crud-decl)
(define-syntax type-api-spec m-type-api-spec)

(define-syntax res-decl m-res-decl)
(define-syntax module-invoke m-module-invoke)
(define-syntax named-parameter m-named-parameter)
(define-syntax config-func-call m-config-func-call)
(define-syntax expression m-expression)
(define-syntax reference m-reference)
(define-syntax config-object m-config-object)
(define-syntax alist m-alist)
(define-syntax list-attr m-list-attr)
(define-syntax attr-decl m-attr-decl)
(define-syntax keyword m-keyword)
(define-syntax built-in m-built-in)
(define-syntax env-read m-env-read)
(define-syntax strf m-strf)
(define-syntax pprint m-pprint)
(define-syntax boolean m-boolean)
(define-syntax config-expr m-config-expr)
(define-syntax config-merge m-config-merge)
(define-syntax config-take m-config-take)
(define-syntax config-ident m-config-ident)

(provide marv-spec marv-module module-parameter decl var-decl res-decl
         module-invoke named-parameter module-return return-parameter
         module-import
         config-func-call config-func-decl
         type-decl type-body type-crud-decl type-api-spec
         expression reference statement config-object alist list-attr
         config-expr config-merge config-ident config-take
         attr-decl keyword built-in env-read pprint strf
         boolean)
