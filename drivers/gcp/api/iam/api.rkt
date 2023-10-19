#lang racket/base

(require racket/string)
(require racket/contract)

(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/crud)
(require marv/drivers/gcp/generic-api-handler)
(require marv/drivers/gcp/operation-handler)
(require marv/drivers/driver)
(require marv/core/config)

(require marv/drivers/gcp/api/iam/types)

(provide init-api)

(define DISCOVERY (make-parameter #f))

(define (init-api interface-id api-id http)
  (DISCOVERY (load-discovery (symbol->string interface-id) api-id))
  (define (genrq cf)
    (lambda(res) ((mk-request-handler (DISCOVERY) iam-type-map iam-api-operation-handler) cf res http)))

  (define crudfn
    (make-driver-crud-fn
     validate
     (genrq crud-create) (genrq crud-read) (genrq crud-update) (genrq crud-delete)
     aux-handler))
  crudfn)

(define (aux-handler op msg)
  (case op
    ; ['register-type register-type]
    [else (raise "Unsupported op/message in storage-api")]))

; TODO - gcp-common module
(define (gcp-type r) (hash-ref r '$type))

(define/contract (validate cfg)
  (config/c . -> . config/c)

  (define type (gcp-type cfg))
  (define api (api-for-type-op (DISCOVERY) (crud-create(iam-type-map type))))

  (define (has-required-api-parameters?)
    (define req-params (api-required-params api))
    (for/first ([p req-params] #:unless (hash-has-key? cfg p))
      (raise (format "Config does not have required field(s) (~a) ~a" p req-params))))
  (has-required-api-parameters?)
  cfg)

