#lang racket/base
(require racket/contract)
(require racket/string)

(require marv/core/config)
(require marv/drivers/utils)
(require marv/drivers/types)
(require marv/log)

(provide init-dev-driver)

(define (init-dev-driver interface-id)

  (define/contract (routing driver-spec config)
    (driver-spec/c config/c . -> . driver-resp/c)
    (define subtype (string->symbol(car (string-split (driver-spec-api driver-spec) "." ))))
    (define api (driver-spec-api driver-spec))
    (define pre (driver-spec-pre-fn driver-spec))
    (define post (driver-spec-post-fn driver-spec))
    (log-marv-debug "dev-routing driver:~a config:~a" driver-spec config)
    (define xform (pre config))
    (log-marv-debug "xformed: ~a" xform)
    (define resp (http-transport api "https://local/host/" xform))
    (define respxform (post resp))
    (log-marv-debug "xformed resp: ~a" respxform)
    respxform)

  routing)

(define (driver-spec-api ds) (hash-ref ds 'api-id))
(define (driver-spec-pre-fn ds) (hash-ref ds 'pre))
(define (driver-spec-post-fn ds) (hash-ref ds 'post))

(define (http-transport method url res)
  (displayln (format "FAKE-HTTP ~a: ~a ~a" method url res))
  (hash-set res 'faked (format "~a:~a" method url)))
