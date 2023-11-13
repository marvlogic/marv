#lang racket/base

(require racket/contract)
(require marv/drivers/gcp/crud)
(require marv/log)

(provide iam-type-map register-type)

; Returns id of resource operation e.g. compute.instance.get
(define (iam-type-map type) (hash-ref (type-map) type))

(define/contract (register-type type crud)
  (symbol? crud? . -> . void)
  (log-marv-info "Registering type: ~a ~a" type crud)
  (type-map (hash-set (type-map) type crud)))

(define type-map (make-parameter (hash)))