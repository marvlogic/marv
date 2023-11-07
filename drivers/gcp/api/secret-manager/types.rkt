#lang racket/base

(require racket/contract)
(require marv/drivers/gcp/crud)
(require marv/log)

(provide secret-type-map register-type)

; Returns id of resource operation e.g. compute.instance.get
(define (secret-type-map type) (crud-for-type (type-map) type #:create "create"))

(define/contract (register-type type crud)
  (symbol? crud? . -> . void)
  (log-marv-info "Registering type: ~a ~a" type crud)
  (type-map (hash-set (type-map) type crud)))

(define base-type-map
  (make-immutable-hasheq '()))
;  `((secretmanager.project.secret . secretmanager.projects.secrets))))

(define type-map (make-parameter base-type-map))