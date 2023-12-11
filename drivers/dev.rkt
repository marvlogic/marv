#lang racket/base
(require racket/contract)
(require racket/string)

(require marv/core/config)
(require marv/drivers/types)
(require marv/log)

(provide init-dev-driver)

(define (init-dev-driver _)

  (define/contract (routing driver-spec config)
    (driver-cmd/c config/c . -> . driver-resp/c)
    (define api (driver-spec-api driver-spec))
    ; (define pre (driver-spec-pre-fn driver-spec))
    ; (define post (driver-spec-post-fn driver-spec))
    (log-marv-debug "dev-routing driver:~a config:~a" driver-spec config)
    ; (define xform (pre config))
    ; (log-marv-debug "xformed: ~a" xform)
    (define resp (http-transport api "https://local/host/" config))
    ; (define respxform (post resp))
    ; (log-marv-debug "xformed resp: ~a" respxform)
    resp)

  routing)

(define (http-transport method url res)
  (displayln (format "FAKE-HTTP ~a: ~a ~a" method url res))
  (hash-set res 'faked (format "~a:~a" method url)))
