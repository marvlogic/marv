#lang racket/base
(require net/http-easy)
(require racket/contract)
(require racket/string)

(require marv/log)
(require marv/core/config)
(require marv/drivers/types)

(require marv/drivers/gcp/operation-handler)
(require (prefix-in generic: marv/drivers/gcp/generic-api))
(require (prefix-in sm: marv/drivers/gcp/patches/secret-manager))
(require (prefix-in sql: marv/drivers/gcp/patches/sql))

(provide init-gcp gcp-http-transport)

(define (init-gcp http-transport)
  (define apis
    (hash
     "compute" (generic:init-api http-transport compute-api-operation-handler)
     "iam" (generic:init-api  http-transport iam-api-operation-handler)
     "storage" (generic:init-api  http-transport compute-api-operation-handler)
     "secretmanager" (generic:init-api  http-transport iam-api-operation-handler)
     "sql" (generic:init-api http-transport compute-api-operation-handler)
     ))

  (define/contract (routing driver-cmd)
    (driver-cmd/c . -> . driver-resp/c)
    (log-marv-debug " GCP routing called: ~a" driver-cmd)
    (define gcp-api (hash-ref (driver-spec-api driver-cmd) 'api-id))
    (define api (hash-ref apis gcp-api))
    (api driver-cmd http-transport))
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
