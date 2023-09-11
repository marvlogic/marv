#lang racket/base

(require marv/log)
(require racket/string)
(provide gen-resources gen-drivers getenv-or-raise
         register-type)

(define (gen-resources decls-hs)
  (define rs
    (filter (lambda (kv)
              (and (hash? (cdr kv)) (hash-has-key? (cdr kv) '$driver)))
            (hash->list decls-hs)))
  ; (displayln rs)
  (lambda (mkres) (xform-resources mkres rs)))

(define (xform-resources mkres resources)
  (define (xform kv)
    (define id (car kv))
    (define res (cdr kv))
    (cons id (mkres (hash-ref res '$driver) (hash-remove res '$driver))))
  (map xform resources))

(define (gen-drivers decls-hs) tmp-drivers)

(define (getenv-or-raise e)
  (or (getenv e) (raise (format "ERROR: ~a must be defined in environment" e))))

(require marv/drivers/dev)
(require marv/drivers/gcp/api)

(define (register-type driver-id type-list stuff)
  (define type (string->symbol(string-join (map symbol->string type-list) ".")))
  (log-marv-info "Registering: ~a:~a ~a" driver-id type stuff)

  (define (type-transform spec)
    (values (string->symbol(string-join (map symbol->string (car spec)) ".")) (cadr spec)))

  (for ([op (hash-keys stuff)])
    (define-values (typeapi xform) (type-transform (hash-ref stuff op)))
    (log-marv-info "Sub-reg ~a:~a" typeapi xform)
    (gcp-register-type type typeapi xform)))

(define defaults
  (hash 'project (getenv-or-raise "MARV_GCP_PROJECT")
        'region  (getenv-or-raise "MARV_GCP_REGION")))
(define (tmp-drivers)
  (hash
   'dev (init-dev-driver 'dev)
   'gcp (init-gcp 'gcp (gcp-http-transport (getenv-or-raise "GCP_ACCESS_TOKEN"))
                  #:project (hash-ref defaults 'project)
                  #:region (hash-ref defaults 'region)
                  )))
