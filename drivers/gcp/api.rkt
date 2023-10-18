#lang racket/base
(require racket/hash)
(require racket/string)
(require net/http-easy)
(require racket/contract)

(require marv/log)
(require marv/utils/hash)
(require marv/core/config)
(require marv/drivers/types)

(require marv/drivers/gcp/compute-api)
(require marv/drivers/gcp/storage-api)
(require marv/drivers/gcp/transformers)

; TODO - common module, and abstract setting it too
(define (gcp-type r) (hash-ref r '$type))

(provide init-gcp gcp-http-transport)

(define (init-gcp interface-id http-transport)
  (define apis
    (hash 'compute (compute.init-api interface-id "compute:beta" http-transport)
          'storage (storage.init-api interface-id "storage:v1" http-transport)
          ))

  (define/contract (routing op config)
    (msg-id/c config/c . -> . config/c)
    (define subtype (string->symbol (car (string-split (symbol->string (gcp-type config)) "."))))
    (define crudfn (hash-ref apis subtype))
    (crudfn op config))

  routing)

(define (gcp-http-transport access-token)

  (define auth-token (bearer-auth access-token))

  (define (expect-2xx resp #:expect-status (expect '(200 204)))
    (cond [(member (response-status-code resp) expect )
           (log-marv-debug "~v" (response-json resp))
           (response-json resp)]
          [else (log-marv-warn "Headers: ~v" (response-headers resp))
                (raise (format "unexpected response: ~a:~a headers:~a"
                               (response-status-code resp)
                               (response-body resp)
                               (response-headers resp)))]))

  (define methods
    (hash
     'POST (lambda(url body) (expect-2xx (post url #:auth auth-token #:json body)))
     'GET (lambda(url _) (expect-2xx (get url #:auth auth-token)))
     'PUT (lambda(url body) (expect-2xx (put url #:auth auth-token #:json body)))
     'PATCH (lambda(url body) (expect-2xx (patch url #:auth auth-token #:json body)))
     'DELETE (lambda(url _) (expect-2xx (delete url #:auth auth-token)))
     ))
  (define (handle method url body)
    (log-marv-info "gcp-http-transport: ~a ~a" method url)
    ((hash-ref methods method) url body))
  handle)

(define (resource-self-link res-state) (hash-ref res-state 'selfLink))