#lang racket/base

(provide (struct-out crud)
         crud-for-type)

(struct crud (create read update delete) #:prefab)

(define (std-crud resid #:create create)
  (define (op opid) (string->symbol (format "~a.~a" resid opid)))
  (crud (op create) (op "get") (op "patch") (op "delete")))

(define (crud-for-type type-map type #:create (create "insert"))
  (define gcp-type (hash-ref type-map type))
  (cond [(symbol? gcp-type) (std-crud gcp-type #:create create)]
        [(crud? gcp-type) gcp-type]
        [else raise (format "~a does not have correct CRUD struct" gcp-type)]))
