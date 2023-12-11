#lang racket/base

(require racket/string)
(require racket/syntax)
(require (for-syntax racket/base racket/syntax syntax/parse racket/pretty marv/core/globals))

(require racket/pretty)

(require marv/alpha/support)
(require marv/core/values)
(require marv/core/globals)
(require marv/log)

; (require (for-syntax marv/core/values))

; TODO - swap prefix usage to m- on the provided (define a macro?)

(define-for-syntax (src-location s) (format "~a:~a" (syntax-source s) (syntax-line s)))

(begin-for-syntax

  (define (m-marv-spec stx)
    (syntax-parse stx
      [(_ MODULE ...)
       #'(begin
           (require marv/alpha/support)
           (require racket/hash)
           (require racket/pretty)
           MODULE ...
           )]
      [else (raise "nowt")]))

  (define (m-outer-decl stx)
    (syntax-parse stx
      [(_ OUTER) (syntax/loc stx OUTER)]
      [else (raise "nowt-outer-decl")]))

  (define (m-module-export stx)
    (syntax-parse stx
      [(_ IDS ...) (syntax/loc stx (provide IDS ...))]
      [else (raise "nowt-module-export")]))

  (define (m-marv-module stx)
    (syntax-parse stx
      [(_ (~optional (~and private? "private"))
          mod-id:expr (~optional (~seq "(" PARAMS ... ")")
                                 #:defaults ([(PARAMS 1) null]))
          STMT ...
          (~optional (~seq "return" RETURN)
                     #:defaults ([RETURN #'void])))
       #:with MAYBE-PRIVATE (if (attribute private?) #'(void) #'(provide mod-id))
       (syntax/loc stx
         (begin
           (define (mod-id resid-prefix params)
             (log-marv-debug "** Generating module: ~a=~a(~a)" resid-prefix 'mod-id params)
             (define rs
               (with-module-ctx resid-prefix params
                 (lambda ()
                   PARAMS ...
                   STMT ...
                   (define rs (gen-resources))
                   RETURN
                   rs
                   )))
             (log-marv-debug "** generation completed for ~a.~a" resid-prefix 'mod-id)
             rs)
           MAYBE-PRIVATE))]
      [else (raise "invalid module spec m-marv-module")]))

  (define (m-module-parameter stx)
    (syntax-parse stx
      [(_ PARAMETER "=" EXPR) (syntax/loc stx (define PARAMETER (get-param 'PARAMETER EXPR)))]
      [(_ PARAMETER) (syntax/loc stx (define PARAMETER (get-param 'PARAMETER)))]))

  (define (m-module-return stx)
    (syntax-parse stx
      [(_ "return" RETURNS ...) (syntax/loc stx (set-return (make-immutable-hasheq (list RETURNS ...))))]
      [else (raise "m-module-return f*")]))

  (define (m-module-import stx)
    (syntax-parse stx
      ; this fix comes courtesy of replies to this:
      ; https://stackoverflow.com/questions/77621776/hard-coded-require-from-a-racket-macro-doesnt-bind-provided-identifiers
      [(_ MOD-ID:id)
       (define mpath (format "marv/~a.mrv" (syntax-e #'MOD-ID)))
       (datum->syntax stx `(require (lib ,mpath)))]

      [(_ MOD-ID:id "as" ALIAS)
       (define mpath (format "marv/~a.mrv" (syntax-e #'MOD-ID)))
       (define alias (format-id #f "~a:" (syntax-e #'ALIAS)))
       (datum->syntax stx `(require (prefix-in ,alias (lib ,mpath)))) ]

      [(_ FILENAME:string) (syntax/loc stx (require FILENAME)) ]
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

    (define-splicing-syntax-class type-body
      #:description "body declaration"
      #:literals (config-expr)
      (pattern (~seq vid:id cex)))

    (syntax-parse stx
      [(_ ((~literal type-id) type-id:expr)
          ((~literal driver-id) did:expr)
          body:type-body ...)
       (syntax/loc stx
         (begin
           (define (type-id verb config)
             (log-marv-debug "type-fn ~a.~a called with config ~a" 'type-id verb config)
             ; TODO - case vs hash?
             (case verb
               ['origin (hash 'driver 'did 'type 'type-id)]
               ['driver 'did]
               ['type 'type-id]
               ['body.vid body.cex] ...
               ))
           ))]))

  (define (m-type-api-spec stx)
    (syntax-parse stx
      #:literals (api-id transformer-id)
      [(_ (api-id aid:expr) (transformer-id req-xform-id:identifier) (transformer-id resp-xform-id:identifier))
       (syntax/loc stx `(aid ,req-xform-id ,resp-xform-id))]))

  (define (m-generic-placeholder stx)stx)

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

  (define (m-base64encode stx)
    (syntax-parse stx
      [(_ ex:expr) (syntax/loc stx (b64enc ex))]
      [else (raise "m-base64encode")]))

  (define (m-base64decode stx)
    (syntax-parse stx
      [(_ ex:expr) (syntax/loc stx (b64dec ex))]
      [else (raise "m-base64decode")]))

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

    (define (this-name stx) (format-id #f "this_~a" (syntax-e stx)))

    (define-splicing-syntax-class attr-decl
      #:description "attribute declaration"
      #:literals (expression)
      #:attributes (name tname expr raw-expr)
      (pattern (~seq name:id (expression e))
        #:attr tname (this-name #'name)
        #:attr expr #'e
        #:attr raw-expr #'e)
      (pattern (~seq aname:string (expression expr))
        #:attr name (format-id #f "~a" (syntax-e #'aname))
        #:attr tname (this-name #'aname)
        #:attr raw-expr #'expr)
      (pattern (~seq name:id "imm:" (expression e))
        #:attr tname (this-name #'name)
        #:attr expr #'(ival e)
        #:attr raw-expr #'e))

    (syntax-parse stx
      [(_ attr:attr-decl ...)
       ; TODO - this_* declarations don't work when referenced
       ;  #'(let* ([attr.tname attr.raw-expr] ... )
       ;      (make-immutable-hasheq (list (cons 'attr.name attr.expr) ...))) ]
       #'(make-immutable-hasheq (list (cons 'attr.name attr.expr) ...)) ]
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
      [(_ CFEXPR ATTRLIST) (syntax/loc stx (config-reduce CFEXPR ATTRLIST))]
      [else (raise "m-config-take")]))

  (define (m-config-ident stx)
    (syntax-parse stx
      [(_ CFIDENT) (syntax/loc stx CFIDENT)]
      [else (raise "m-config-ident")]))

  (define (m-keyword stx)
    (syntax-parse stx
      [(_ keyword) (syntax/loc stx keyword)]))

  (define (m-reference stx)
    (syntax-parse stx
      [(_ ref:id)
       (define splitref (split-symbol (syntax-e #'ref)))
       (define root (format-id stx "~a" (car splitref)))
       (define rst (datum->syntax stx (cdr splitref)))
       ; RAW version:  #`(handle-ref #,r0 'r '#,rs)
       (with-syntax
           ([root root]
            [tail rst])
         (syntax/loc stx (handle-ref root 'root 'tail)))]))

  (define (m-res-decl stx)
    (syntax-parse stx
      [(_ name:expr ((~literal type-id) tid:id) cfg)
       #`(define name
           (with-src-handlers #,(src-location stx)  "valid type" 'tid
             (lambda()(def-res tid 'name cfg))))]
      ))

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
(define-syntax outer-decl m-outer-decl)
(define-syntax marv-module m-marv-module)
(define-syntax module-parameter m-module-parameter)
(define-syntax module-return m-module-return)
(define-syntax return-parameter m-return-parameter)
(define-syntax module-import m-module-import)
(define-syntax module-export m-module-export)
(define-syntax statement m-statement)
(define-syntax decl m-decl)
(define-syntax var-decl m-var-decl)
(define-syntax config-func-decl m-config-func-decl)

(define-syntax type-decl m-type-decl)
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
(define-syntax keyword m-keyword)
(define-syntax built-in m-built-in)
(define-syntax env-read m-env-read)
(define-syntax strf m-strf)
(define-syntax base64encode m-base64encode)
(define-syntax base64decode m-base64decode)
(define-syntax pprint m-pprint)
(define-syntax boolean m-boolean)
(define-syntax config-expr m-config-expr)
(define-syntax config-merge m-config-merge)
(define-syntax config-take m-config-take)
(define-syntax config-ident m-config-ident)

(define-syntax api-id m-generic-placeholder)
(define-syntax transformer-id m-generic-placeholder)
(define-syntax driver-id m-generic-placeholder)
(define-syntax type-id m-generic-placeholder)
(define-syntax verb m-generic-placeholder)

(provide marv-spec outer-decl marv-module module-parameter decl var-decl res-decl
         module-invoke named-parameter module-return return-parameter
         module-import
         module-export
         api-id transformer-id driver-id type-id
         config-func-call config-func-decl
         type-decl type-api-spec
         expression reference statement config-object alist list-attr
         config-expr config-merge config-ident config-take
         keyword built-in env-read pprint strf base64encode base64decode
         boolean)
