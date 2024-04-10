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

(define/contract (send-to-driver driver-id driver-cmd)
  (driver-id/c driver-cmd/c . -> . driver-resp/c)
  (log-marv-debug "send-to-driver: ~a ~a" driver-id driver-cmd)
  (define driver-fn (hash-ref (DRIVERS) driver-id))
  (driver-fn driver-cmd))

(define (std-drivers)
  (if
   (getenv "MARV_DEV_DRIVER")
   (hash
    'dev (init-dev-driver 'dev2)
    'gcp (init-dev-driver 'dev))
   (hash
    'dev (init-dev-driver 'dev)
    'gcp (init-gcp (gcp-http-transport (getenv-or-raise "GCP_ACCESS_TOKEN"))))))

(define DRIVERS (make-parameter (std-drivers)))

; TODO41 -docs
(define (display-docs type)
  (raise "todo"))