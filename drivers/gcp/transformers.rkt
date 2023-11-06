#lang racket/base

(require marv/log)
(provide register-request-transformer
         register-response-transformer
         apply-request-transformer
         apply-response-transformer
         (struct-out transformer))

(struct transformer (api-id fn) #:transparent)

(define request-transformers (make-parameter (hash)))
(define response-transformers (make-parameter (hash)))

(define (register-request-transformer t)
  (log-marv-info "Registering request-transformer: ~a" t)
  (request-transformers (hash-set (request-transformers) (transformer-api-id t) (transformer-fn t))))

(define (register-response-transformer t)
  (log-marv-info "Registering response-transformer: ~a" t)
  (response-transformers (hash-set (response-transformers) (transformer-api-id t) (transformer-fn t))))

(define (apply-request-transformer type-op resource)
  (log-marv-debug "apply-request-transformer: ~a ~a" type-op resource)
  ((hash-ref (request-transformers) type-op (lambda() (lambda(r)resource))) resource))

(define (apply-response-transformer type-op resource)
  (log-marv-debug "apply-response-transformer: ~a ~a" type-op resource)
  ((hash-ref (response-transformers) type-op (lambda() (lambda(r)resource))) resource))