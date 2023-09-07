#lang racket/base

(provide (struct-out crud)
         type-to-crud-op)

(struct crud (create read update delete) #:prefab)

(define (std-crud resid)
  (define (op opid) (string->symbol (format "compute.~a.~a" (symbol->string resid) opid)))
  (crud (op "insert") (op "get") (op "patch") (op "delete")))

(define (type-to-crud-op type-map type op)
  (define gcp-type (hash-ref type-map type))
  (op (cond [(symbol? gcp-type) (std-crud gcp-type)]
            [else gcp-type])))
