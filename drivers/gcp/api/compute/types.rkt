#lang racket/base

(require racket/contract)
(require marv/drivers/gcp/crud)
(require marv/log)

(provide compute-type-map register-type)

(define (compute-type-map type) (hash-ref (type-map) type))

(define/contract (register-type type crud)
  (symbol? crud? . -> . void)
  (log-marv-info "Registering compute type: ~a ~a" type crud)
  (type-map (hash-set (type-map) type crud)))

(define type-map (make-parameter (hash)))