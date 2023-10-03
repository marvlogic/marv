#lang racket/base

(require racket/hash)
(require racket/string)
(require marv/log)
(require marv/utils/hash)
(require marv/core/values)

(provide gen-resources gen-drivers getenv-or-raise
         hash-union
         resources-stub
         set-var
         get-var
         def-res resource-var? config-overlay hash-nref handle-ref
         set-params get-param
         register-type)

(define (error:excn msg)
  (raise (format "ERROR: ~a\n at ~a:~a" msg 1 2))) ;(syntax-source stx) (syntax-line stx)))

(define PARAMS (make-parameter (hash)))
(define set-params PARAMS)
(define (get-param p) (hash-ref (PARAMS) p))

(define VARS (make-parameter (hash)))
(define (set-var id v)
  (when (hash-has-key? (VARS) id) (error:excn (format "~a is already defined" id)))
  (VARS (hash-set (VARS) id v)))
(define (get-var id) (hash-ref (VARS) id))

(define (def-res id drv attr v)
  (define r (hash-set* v '$driver drv '$type (string->symbol (string-join (map symbol->string attr) "."))))
  (set-var id r)
  r)

(define (resource-var? id)
  (define v (hash-ref (VARS) id))
  (and (hash? v) (hash-has-key? v '$driver)))

(define (config-overlay left right) (hash-union left right #:combine (lambda (v0 _) v0)))
; (define (config-take cfg attrs) (hash-take cfg attrs))

(define (hash-nref hs ks)
  (for/fold ([h hs])
            ([k (in-list ks)])
    (hash-ref h k)))

(define (handle-ref id tgt . ks)
  (define ksx (map syntax-e ks))
  (define full-ref (string->symbol (string-join (map symbol->string (cons id ksx)) ".")))
  (cond [(hash-has-key? tgt '$driver) (ref full-ref)]
        [else (hash-nref tgt ksx)]))

; (define (handle-deref r) #`(hash-nref #,(car r) #,(cdr r)))

(define (loop-res-name name loop-ident)
  (format "~a.~a" name (get-var loop-ident)))

(define (resources-stub module-id mkres params)
  (parameterize ([VARS (hash)]
                 [PARAMS params])
    (print (list mkres params))))

(define (gen-resources)
  (define rs
    (filter (lambda (kv)
              (and (hash? (cdr kv)) (hash-has-key? (cdr kv) '$driver)))
            (hash->list (VARS))))
  (lambda (mkres params) (xform-resources mkres rs)))

(define (xform-resources mkres resources)
  (define (xform kv)
    (define id (car kv))
    (define res (cdr kv))
    (cons id (mkres (hash-ref res '$driver) (hash-remove res '$driver))))
  (map xform resources))

(define (gen-drivers decls-hs) tmp-drivers)

(define (getenv-or-raise e)
  (or (getenv e) (raise (format "ERROR: ~a must be defined in environment" e))))

(require marv/drivers/driver)
(require marv/drivers/dev)
(require marv/drivers/gcp/api)
(require marv/drivers/gcp/transformers)

(define (register-type driver-id type-id api-specs)

  (define type (string->symbol(string-join (map symbol->string type-id) ".")))
  (define (check-for m)
    (unless (hash-has-key? api-specs m)
      (raise (format "~a:~a does not have the required '~a' clause" driver-id type m))))
  (check-for 'create)
  (check-for 'delete)

  (log-marv-info "Registering: ~a:~a ~a" driver-id type api-specs)

  (define (type-transform spec)
    (cond [(null? spec) (transformer null null)]
          [else (transformer (string->symbol(string-join (map symbol->string (car spec)) ".")) (cadr spec))]))

  (define (register-type-msg type transforms) (hash '$type type 'transforms transforms))

  (define driver-head (make-driver-for-set (tmp-drivers)))
  (driver-head driver-id 'register-type
               (register-type-msg
                type
                (for/list ([op '(create read update delete)])
                  (type-transform (hash-ref api-specs op null)))))
  ; NB return void or the empty hash gets printed!
  (void))

(define defaults
  (hash 'project (getenv-or-raise "MARV_GCP_PROJECT")
        'region  (getenv-or-raise "MARV_GCP_REGION")))

(define (tmp-drivers)
  (hash
   'dev (init-dev-driver 'dev)
   'gcp (init-gcp 'gcp (gcp-http-transport (getenv-or-raise "GCP_ACCESS_TOKEN"))
                  )))
