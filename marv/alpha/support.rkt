#lang racket/base

(provide gen-resources gen-drivers getenv-or-raise)

(define (gen-resources decls-hs)
  (define rs
    (filter (lambda (kv)
              (and (hash? (cdr kv)) (hash-has-key? (cdr kv) '$driver)))
            (hash->list decls-hs)))
  (displayln rs)
  (lambda (mkres) (xform-resources mkres rs)))

(define (xform-resources mkres resources)
  (define (xform kv)
    (define id (car kv))
    (define res (cdr kv))
    (cons (string->symbol id) (mkres (string->symbol(hash-ref res '$driver)) res)))
  (map xform resources))

(define (gen-drivers decls-hs) tmp-drivers)

(define (getenv-or-raise e)
  (or (getenv e) (raise (format "ERROR: ~a must be defined in environment" e))))

(require marv/drivers/dev)
(require marv/drivers/gcp/api)
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
