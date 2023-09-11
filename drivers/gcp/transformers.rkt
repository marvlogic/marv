#lang racket/base

(require marv/log)
(provide register-request-transformer
         apply-request-transformer
         (struct-out transformer))

(struct transformer (api-id fn) #:transparent)

(define request-transformers (make-parameter (hash)))

(define (register-request-transformer t)
  (log-marv-info "Registering request-transformer: ~a" t)
  (request-transformers (hash-set (request-transformers) (transformer-api-id t) (transformer-fn t))))

(define (apply-request-transformer type-op resource)
  ((hash-ref (request-transformers) type-op (lambda() (lambda(r)resource))) resource))