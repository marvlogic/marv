#lang racket/base

(require racket/string)
(require racket/contract)

(require marv/log)
(require marv/core/config)
(require marv/utils/hash)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/crud)
(require marv/drivers/gcp/transformers)
(require marv/drivers/driver)

(require marv/drivers/gcp/storage-types)

(provide (prefix-out storage. init-api))

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
    ; ['register-type register-type]
    [else (raise "Unsupported op/message in storage-api")]))

(define (validate cfg) cfg)

; TODO - gcp-common module
(define (gcp-type r) (hash-ref r '$type))

(define/contract (generic-request crud-fn config http)
  (procedure? config/c any/c . -> . config/c)
  (define type-op (crud-fn (storage-type-map (gcp-type config))))
  (log-marv-debug "gen/req: type-op=~a ~a" type-op config)
  (define xfd-resource (apply-request-transformer type-op config) )
  (log-marv-debug "xformed: ~a" xfd-resource)
  (cond
    [(null? type-op) xfd-resource]
    [(symbol? type-op)
     (define api (api-for-type-op (DISCOVERY) type-op))
     (define response
       (http (api-http-method api)
             (api-resource-url api xfd-resource)
             (api-resource api xfd-resource)))
     (hash-merge
      config
      (handle-delete crud-fn response))]
    [else raise (format "type has no usable CRUD for ~a : ~a" crud-fn type-op )]
    ))

(define (handle-delete crud-fn response) (if (eq? crud-delete crud-fn) (hash) response))