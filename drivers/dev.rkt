#lang racket/base
(require racket/contract)
(require racket/string)

(require marv/core/config)
(require marv/drivers/types)
(require marv/log)

(provide init-dev-driver)

(define (init-dev-driver _)

  (define/contract (routing driver-spec)
    (driver-cmd/c . -> . driver-resp/c)
    (define api (driver-spec-api driver-spec))
    (define config (hash-ref driver-spec 'config))
    (log-marv-debug "dev-routing driver:~a config:~a" driver-spec config)
    (define resp (http-transport (format "https://local/~a" api) config))
    resp)

  routing)

(define (http-transport url res)
  (displayln (format "FAKE-HTTP ~a: ~a" url res))
  (hash-set res 'faked (format "~a" url)))
