#lang racket/base

(require racket/string)
(require racket/pretty)

(require marv/utils/hash)
(require marv/drivers/gcp/compute-types)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/crud)

(provide (prefix-out compute. init-api))

(define DISCOVERY (make-parameter #f))

(define (init-api interface-id api-id)
  (DISCOVERY (load-discovery (symbol->string interface-id) api-id))

  (define (create-request resource http) (generic-request compute-type-map 'insert resource http))
  (define (read-request resource http) (generic-request compute-type-map 'get resource http))
  ; (define (read-request resource http) (http 'GET (hash-ref resource 'selfLink) '()))
  (define (update-request resource http) (generic-request compute-type-map 'patch resource http))
  (define (delete-request resource http) (generic-request compute-type-map 'delete resource http))
  (hash 'validate validate-res
        'create create-request
        'read read-request
        'update update-request
        'delete delete-request))

; TODO - gcp-common module
(define (gcp-type r) (hash-ref r '$type))

(define (validate-res resource)
  (define type (string->symbol(cadr (string-split (gcp-type resource) "."))))
  (define api (api-for-type-op (DISCOVERY) (compute-type-map type crud-create)))
  (validate-parameters-for-api (api-for-type-op (DISCOVERY) (compute-type-map type crud-create)) resource)
  #t)

(define (validate-parameters-for-api api resource)
  (displayln (api-required-params api)))

(define (generic-request type-map api-method resource http)
  (define type (string->symbol(cadr (string-split (gcp-type resource) "."))))
  (define api (api-for-type (DISCOVERY) (hash-ref type-map type) api-method))
  (define response
    (http (api-http-method api)
          (api-resource-url api type resource)
          (api-resource api resource)))
  (hash-merge
   resource
   (if (expect-operation-response? api)
       (handle-operation-response api-method response http)
       (handle-delete api-method response))))

(define (handle-delete api-method response) (if (eq? 'delete api-method) (hash) response))

(define (expect-operation-response? api) (equal? "Operation" (api-response-type api)))

(define (handle-operation-response api-method op-response http)
  (cond [(not (hash? op-response)) #f]
        [(equal? "compute#operation" (hash-ref op-response 'kind))
         (define completed-op (wait-completed op-response http))
         (cond [(eq? 'delete api-method) (hash)]
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
