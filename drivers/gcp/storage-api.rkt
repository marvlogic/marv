#lang racket/base

(require racket/string)
(require racket/pretty)

(require marv/utils/hash)
(require marv/drivers/gcp/storage-types)
(require marv/drivers/gcp/discovery)

(provide (prefix-out storage. init-api))

(define DISCOVERY (make-parameter #f))

(define (init-api interface-id api-id)
  (DISCOVERY (load-discovery (symbol->string interface-id) api-id))
  (define (create-request resource http) (generic-request storage-type-map 'insert resource http))
  (define (read-request resource http) (generic-request storage-type-map 'get resource http))
  ; (define (read-request resource http) (http 'GET (hash-ref resource 'selfLink) '()))
  (define (update-request resource http) (generic-request storage-type-map 'patch resource http))
  (define (delete-request resource http) (generic-request storage-type-map 'delete resource http))
  (hash 'create create-request
        'read read-request
        'update update-request
        'delete delete-request))

; TODO - gcp-common module
(define (gcp-type r) (hash-ref r '$type))

(define (generic-request type-map api-method resource http)
  (define type (string->symbol(cadr (string-split (gcp-type resource) "."))))
  (define api (api-for-type (DISCOVERY) (hash-ref type-map type) api-method))
  (define response
    (http (api-http-method api)
          (api-resource-url api type resource)
          (api-resource api resource)))
  (hash-merge resource (handle-delete api-method response)))

(define (handle-delete api-method response) (if (eq? 'delete api-method) (hash) response))