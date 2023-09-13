#lang racket/base

(require racket/string)
(require racket/pretty)

(require marv/log)
(require marv/core/resources)
(require marv/utils/hash)
(require marv/drivers/gcp/compute-types)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/crud)
(require marv/drivers/gcp/transformers)
(require marv/drivers/driver)


(provide (prefix-out compute. init-api)
         (prefix-out compute. register-type))

(define DISCOVERY (make-parameter #f))

(define (init-api interface-id api-id http)
  (DISCOVERY (load-discovery (symbol->string interface-id) api-id))
  (define (mk-res config)
    (define res (validate-res config))
    (define (create) (generic-request crud-create res http))
    (define (readr) (generic-request crud-read res http))
    (define (update) (generic-request crud-update res http))
    (define (delete) (generic-request crud-delete res http))
    (resource res (make-driver-crud-fn create readr update delete)))
  mk-res)

(define (register-type type transformers)
  (log-marv-info "compute-register-type: ~a:~a" type transformers)
  (define apis (map transformer-api-id transformers))
  (define-values (create-api read-api update-api delete-api) (apply values apis))
  (ct-register-type type (crud create-api read-api update-api delete-api))

  (define tfns (map transformer-fn transformers))
  (for ([a apis]
        [t tfns]
        #:when (procedure? t))
    (register-request-transformer (transformer a t))))

; TODO - gcp-common module
(define (gcp-type r) (string->symbol(hash-ref r '$type)))

(define (validate-res resource)

  (define type (gcp-type resource))
  (define api (api-for-type-op (DISCOVERY) (crud-create(compute-type-map type))))

  (define (has-required-api-parameters?)
    (define req-params (api-required-params api))
    (for/first ([p req-params] #:unless (hash-has-key? resource p))
      (raise (format "Resource does not have required field(s) (~a) ~a" p req-params))))

  (has-required-api-parameters?)
  resource)

(define (generic-request crud-fn resource http)
  (define type-op (crud-fn (compute-type-map (gcp-type resource))))
  (log-marv-debug "gen/req: type-op=~a ~a" type-op resource)
  (define xfd-resource (apply-request-transformer type-op resource) )
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
      resource
      (if (expect-operation-response? api)
          (handle-operation-response crud-fn response http)
          (handle-delete crud-fn response)))]
    [else raise (format "type has no usable CRUD for ~a : ~a" crud-fn type-op )]
    ))

(define (handle-delete crud-fn response) (if (eq? crud-delete crud-fn) (hash) response))

(define (expect-operation-response? api) (equal? "Operation" (api-response-type api)))

(define (handle-operation-response crud-fn op-response http)
  (cond [(not (hash? op-response)) #f]
        [(equal? "compute#operation" (hash-ref op-response 'kind))
         (define completed-op (wait-completed op-response http))
         (cond [(eq? crud-delete crud-fn) (hash)]
               [else (http 'GET (hash-ref completed-op 'targetLink) '())])]
        [else op-response]
        ))

(define (wait-completed op-response http #:wait1 (wait1 1) #:wait2 (wait2 2))
  (printf "\033[s..IN PROGRESS..~a\033[u" wait1)
  (flush-output)
  (define sleeptime(+ wait1 wait2))
  (sleep sleeptime)
  (define new-op (http 'GET (hash-ref op-response 'selfLink) '()))
  (cond [(equal? "DONE" (hash-ref new-op 'status))
         (displayln "..DONE\033[K")
         (flush-output)
         new-op]
        [else (wait-completed new-op http #:wait1 wait2 #:wait2 sleeptime)]))
