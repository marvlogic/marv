#lang racket/base

(require racket/contract)
(require marv/drivers/types)

(provide gcp-type)

(define (raise-unsupported op res) (raise (format "Unsupported message/operation: ~a = ~a" op res)))

(define (gcp-type r) (hash-ref r '$type))
