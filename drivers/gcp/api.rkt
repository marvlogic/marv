#lang racket/base
(require racket/hash)
(require racket/string)
(require net/http-easy)
(require racket/contract)

(require marv/log)
(require marv/utils/hash)
(require marv/core/resources)
(require marv/drivers/driver)

(require marv/drivers/gcp/compute-api)
(require marv/drivers/gcp/storage-api)


; TODO - common module
(define (gcp-type r) (hash-ref r '$type))

(provide init-gcp
         gcp-http-transport )

(define (init-gcp interface-id
                  http-transport
                  #:project project-id
                  #:region region)

  (define driver-conf (hash 'project project-id
                            'region region))

  ; TODO - contract for supported types

  (define/contract (mk-resource res)
    (hash? . -> . config/c)
    ((google-api-fn res 'validate) res))

  (define APIS
    (hash "compute" (compute.init-api interface-id "compute:beta")
          "storage" (storage.init-api interface-id "storage:v1")))

  (define (google-api-fn resource op)
    (define sub-api (car (string-split (gcp-type resource) ".")))
    (hash-ref (hash-ref APIS sub-api) op))

  (define/contract (create resource)
    (config/c . -> . config/c)
    ((google-api-fn resource 'create) resource http-transport))

  (define/contract (readr resource)
    (config/c . -> . config/c)
    ((google-api-fn resource 'read)  resource http-transport))

  (define/contract (update resource)
    (config/c . -> . config/c)
    ((google-api-fn resource 'update) resource http-transport))

  (define/contract (delete resource)
    (config/c . -> . config/c)
    ((google-api-fn resource 'delete) resource http-transport))

  (driver mk-resource create readr update delete driver-conf))

(define (gcp-http-transport access-token)

  (define auth-token (bearer-auth access-token))

  (define (expect-2xx resp #:expect-status (expect '(200 204)))
    (log-marv-debug "~a" (response-json resp))
    (cond [(member (response-status-code resp) expect )(response-json resp)]
          [else (raise (format "unexpected response: ~a:~a"
                               (response-status-code resp)
                               (response-body resp)))]))

  (define methods
    (hash
     'POST (lambda(url body) (expect-2xx (post url #:auth auth-token #:json body)))
     'GET (lambda(url _) (expect-2xx (get url #:auth auth-token)))
     'PUT (lambda(url body) (expect-2xx (put url #:auth auth-token #:json body)))
     'PATCH (lambda(url body) (expect-2xx (patch url #:auth auth-token #:json body)))
     'DELETE (lambda(url _) (expect-2xx (delete url #:auth auth-token)))
     ))
  (lambda (method url body) ((hash-ref methods method) url body)))

; (define (exec api-method meta resource http)
;   (api->state ((api-method (google-api (meta-type meta))) meta resource http)))

; TODO - maybe a standard (e.g. a prefix) to identify keys to remove?
; TODO - region -assumption?
(define (state->api s) (hash-remove-multi s 'project 'region))

; TODO - may find etag is not avoidable in this way, so might need a soft-diff
(define (api->state res-state api-resp)
  (hash-union res-state (hash-remove-multi api-resp 'etag 'timeCreated 'metageneration 'updated)
              #:combine (lambda (a b)b)))

(define (resource-self-link res-state) (hash-ref res-state 'selfLink))
