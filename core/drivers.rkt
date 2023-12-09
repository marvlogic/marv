#lang racket/base

(require racket/contract)
(require racket/list)
(require racket/string)
(require marv/log)
(require marv/core/globals)
(require marv/core/config)
(require marv/drivers/types)
(require marv/drivers/dev)
(require marv/drivers/gcp/api)
(require marv/drivers/gcp/transformers)
(require marv/core/resources)
(require marv/log)

(provide send-to-driver
         driver-spec/c
         driver-resp/c
         display-docs)

; TODO41 - contract and type for spec
(define/contract (send-to-driver driver-id driver-spec config)
  (driver-id/c driver-spec/c config/c . -> . driver-resp/c)
  (log-marv-debug "send-to-driver: ~a ~a ~a" driver-id driver-spec config)
  (define driver-fn (hash-ref (DRIVERS) driver-id))
  (driver-fn driver-spec config))

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