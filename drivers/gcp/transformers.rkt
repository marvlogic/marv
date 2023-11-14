#lang racket/base

(require marv/log)
(provide register-transformers
         apply-request-transformer
         apply-response-transformer
         (struct-out transformer))

(struct transformer (api-id req-fn resp-fn) #:transparent)

(define request-transformers (make-parameter (hash)))
(define response-transformers (make-parameter (hash)))

(define (register-transformers t)
  (log-marv-info "Registering transformer: ~a" t)
  (request-transformers (hash-set (request-transformers) (transformer-api-id t) (transformer-req-fn t)))
  (response-transformers (hash-set (response-transformers) (transformer-api-id t) (transformer-resp-fn t))))

(define (no-op r)(lambda()(lambda(_)r)))

(define (apply-request-transformer type-op resource)
  (log-marv-debug "apply-request-transformer: ~a ~a" type-op resource)
  ((hash-ref (request-transformers) type-op (no-op resource)) resource))

(define (apply-response-transformer type-op resource)
  (log-marv-debug "apply-response-transformer: ~a ~a" type-op resource)
  ((hash-ref (response-transformers) type-op (no-op resource)) resource))