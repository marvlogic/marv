#lang racket/base

(require racket/string)
(require racket/pretty)
(require racket/contract)

(require marv/log)
(require marv/core/config)
(require marv/utils/hash)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/crud)
(require marv/drivers/gcp/generic-api-handler)
(require marv/drivers/gcp/operation-handler)
(require marv/drivers/gcp/transformers)
(require marv/drivers/driver)

(require marv/drivers/gcp/compute-types)

(provide (prefix-out compute. init-api)
         (prefix-out compute. register-type))

(define DISCOVERY (make-parameter #f))

(define (init-api interface-id api-id http)
  (DISCOVERY (load-discovery (symbol->string interface-id) api-id))
  (define (genrq cf) (lambda(res) (generic-request cf res http)))

  (define crudfn
    (make-driver-crud-fn
     validate
     (genrq crud-create) (genrq crud-read) (genrq crud-update) (genrq crud-delete)
     aux-handler))
  crudfn)

(define (aux-handler op msg)
  (case op
    ['register-type register-type]
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

(define/contract (generic-request crud-fn config http)
  (procedure? config/c any/c . -> . config/c)
  (define type-op (crud-fn (compute-type-map (gcp-type config))))
  (log-marv-debug "gen/req: type-op=~a ~a" type-op config)
  (define xfd-resource (apply-request-transformer type-op config) )
  (log-marv-debug "xformed: ~a" xfd-resource)
  (cond
    ; TODO - hacked
    [(null? type-op) xfd-resource]
    [(symbol? type-op)
     (define api (api-for-type-op (DISCOVERY) type-op))
     (define is-delete? (eq? crud-delete crud-fn))
     (define response (generic-api-req api xfd-resource http is-delete? compute-api-operation-handler))
     (log-marv-debug "response: ~a" response)
     (define resp (api-resource api response))
     (hash-merge resp config)]
    [else raise (format "type has no usable CRUD for ~a : ~a" crud-fn type-op )]
    ))

(define (register-type msg)
  (define-values (type transformers) (values (hash-ref msg '$type) (hash-ref msg 'transforms)))
  (log-marv-info "compute-register-type: ~a:~a" type transformers)
  (define apis (map transformer-api-id transformers))
  (define-values (create-api read-api update-api delete-api) (apply values apis))
  (ct-register-type type (crud create-api read-api update-api delete-api))

  (define tfns (map transformer-fn transformers))
  (for ([a apis]
        [t tfns]
        #:when (procedure? t))
    (register-request-transformer (transformer a t)))
  (hash))