#lang racket/base

(require racket/contract)
(require marv/log)
(require marv/core/globals)
(require marv/core/config)
(require marv/drivers/types)
(require marv/drivers/dev)
(require marv/drivers/gcp/api)
(require marv/log)

(provide send-to-driver
         driver-cmd/c
         driver-resp/c
         display-docs)

(define/contract (send-to-driver driver-id driver-cmd config)
  (driver-id/c driver-cmd/c config/c . -> . driver-resp/c)
  (log-marv-debug "send-to-driver: ~a ~a ~a" driver-id driver-cmd config)
  (define driver-fn (hash-ref (DRIVERS) driver-id))
  (define pre (driver-spec-pre-fn driver-cmd))
  (define post (driver-spec-post-fn driver-cmd))
  (post (driver-fn driver-cmd (pre config))))

(define (std-drivers)
  (if
   (getenv "MARV_DEV_DRIVER")
   (hash
    'dev (init-dev-driver 'dev2)
    'gcp (init-dev-driver 'dev))
   (hash
    'dev (init-dev-driver 'dev)
    'gcp (init-gcp 'gcp (gcp-http-transport (getenv-or-raise "GCP_ACCESS_TOKEN"))))))

(define DRIVERS (make-parameter (std-drivers)))

; TODO41 -docs
(define (display-docs type)
  (raise "todo"))