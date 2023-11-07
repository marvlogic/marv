#lang racket/base

(require racket/contract)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/crud)
(require marv/drivers/gcp/generic-api-handler)
(require marv/drivers/gcp/operation-handler)
(require marv/drivers/gcp/transformers)
(require marv/drivers/utils)
(require marv/core/config)
(require marv/log)
(require marv/drivers/gcp/api/secret-manager/types)
(require marv/drivers/gcp/api/secret-manager/patches)
(require marv/drivers/gcp/api/secret-manager/transformers)

(provide init-api handle-register-type)

(define DISCOVERY (make-parameter #f))

(define (init-api interface-id api-id http)
  (DISCOVERY (load-discovery (symbol->string interface-id) api-id patches))
  (define (genrq cf)
    (lambda(res) ((mk-request-handler (DISCOVERY) secret-type-map iam-api-operation-handler) cf res http)))

  ; (request-transformers)
  (define crudfn
    (make-driver-crud-fn
     validate
     (genrq crud-create) (genrq crud-read) (genrq crud-update) (genrq crud-delete)
     aux-handler))
  crudfn)

(define (aux-handler op msg)
  (case op
    ['register-type handle-register-type]
    [else (raise "Unsupported op/message in secret-manager-api")]))

; TODO - gcp-common module
(define (gcp-type r) (hash-ref r '$type))

(define/contract (validate cfg)
  (config/c . -> . config/c)

  (define type (gcp-type cfg))
  (define api (api-for-type-op (DISCOVERY) (crud-create(secret-type-map type))))

  (define (has-required-api-parameters?)
    (define req-params (api-required-params api))
    (for/first ([p req-params] #:unless (hash-has-key? cfg p))
      (raise (format "Config does not have required field(s) (~a) ~a" p req-params))))
  ; (has-required-api-parameters?)
  cfg)

; TODO23 - must be common?
(define (handle-register-type msg)
  (define-values (type transformers) (values (hash-ref msg '$type) (hash-ref msg 'transforms)))
  (log-marv-info "secret-manager-register-type: ~a:~a" type transformers)
  (define apis (map transformer-api-id transformers))
  (define-values (create-api read-api update-api delete-api) (apply values apis))
  (register-type type (crud create-api read-api update-api delete-api))

  ; TODO23 - tidy this
  (define tfns (map transformer-req-fn transformers))
  (define rfns (map transformer-resp-fn transformers))
  (for ([a apis]
        [req tfns]
        [resp rfns]
        #:when (procedure? req))
    (register-transformers (transformer a req resp)))
  (hash))