#lang racket/base

(require racket/contract)

(require marv/log)
(require marv/core/config)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/crud)
(require marv/drivers/gcp/generic-api-handler)
(require marv/drivers/gcp/operation-handler)
(require marv/drivers/gcp/transformers)
(require marv/drivers/utils)

(require marv/drivers/gcp/api/compute/types)

(provide init-api)

(define DISCOVERY (make-parameter #f))

(define (init-api interface-id api-id http)
  (DISCOVERY (load-discovery (symbol->string interface-id) api-id))
  (define (genrq cf) (lambda(res) ((mk-request-handler (DISCOVERY) compute-type-map compute-api-operation-handler) cf res http)))

  (define crudfn
    (make-driver-crud-fn
     validate
     (genrq crud-create) (genrq crud-read) (genrq crud-update) (genrq crud-delete)
     aux-handler))
  crudfn)

(define (aux-handler op msg)
  (case op
    ; ['register-type register-type]
    [else (raise "Unsupported op/message in compute-api")]))

; TODO - gcp-common module
(define (gcp-type r) (hash-ref r '$type))

(define/contract (validate cfg)
  (config/c . -> . config/c)

  (define type (gcp-type cfg))
  (define api (api-for-type-op (DISCOVERY) (crud-create(compute-type-map type))))

  (define (has-required-api-parameters?)
    (define req-params (api-required-params api))
    (for/first ([p req-params] #:unless (hash-has-key? cfg p))
      (raise (format "Config does not have required field(s) (~a) ~a" p req-params))))
  (has-required-api-parameters?)
  cfg)

; TODO23
; (define (register-type msg)
;   (define-values (type transformers) (values (hash-ref msg '$type) (hash-ref msg 'transforms)))
;   (log-marv-info "compute-register-type: ~a:~a" type transformers)
;   (define apis (map transformer-api-id transformers))
;   (define-values (create-api read-api update-api delete-api) (apply values apis))
;   (ct-register-type type (crud create-api read-api update-api delete-api))

;   (define tfns (map transformer-fn transformers))
;   (for ([a apis]
;         [t tfns]
;         #:when (procedure? t))
;     (register-request-transformer (transformer a t)))
;   (hash))