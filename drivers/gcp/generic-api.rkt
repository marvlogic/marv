#lang racket/base

(require racket/contract)
(require marv/log)
(require marv/core/config)
(require marv/drivers/utils)
(require marv/drivers/gcp/crud)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/generic-request-handler)
(require marv/core/globals)

(provide init-api)

(define (init-api  http OPERATION-HANDLER)
  (mk-request-handler OPERATION-HANDLER))
