
#lang racket/base

(require racket/function)
(require marv/drivers/gcp/transformers)
(require marv/utils/hash)
(require marv/utils/base64)

(provide request-transformers)

; TODO23
(define (request-transformers)

  (define (secret-parent res) (hash-set res 'parent (format "projects/~a" (hash-ref res 'project))))
  (register-transformers (transformer 'secretmanager.projects.secrets.create secret-parent identity)))
