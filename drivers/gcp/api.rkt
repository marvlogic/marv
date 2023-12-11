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

;TODO41- need driver-id anymore? only used for with-workspace calls...
(define (init-gcp driver-id http-transport)
  (define apis
    (hash
     'compute (generic:init-api driver-id "compute:beta" http-transport compute-api-operation-handler)
     'iam (generic:init-api driver-id "iam:v1" http-transport iam-api-operation-handler)
     'storage (generic:init-api driver-id "storage:v1" http-transport compute-api-operation-handler) ; TODO
     'secretmanager (generic:init-api driver-id "secretmanager:v1" http-transport iam-api-operation-handler sm:patches) ; TODO
     'sql (generic:init-api driver-id "sqladmin:v1" http-transport compute-api-operation-handler sql:patches) ; TODO
     ))

  (define/contract (routing driver-spec config)
    (driver-cmd/c config/c . -> . driver-resp/c)
    (define subtype (string->symbol(car (string-split (driver-spec-api driver-spec) "." ))))
    (define api (hash-ref apis subtype))
    (api driver-spec config http-transport))
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
