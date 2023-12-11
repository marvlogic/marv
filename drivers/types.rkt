#lang racket/base

(require racket/contract)
(provide (all-defined-out))

(define driver-id/c symbol?)
(define driver/c procedure?)
(define driver-set/c (hash/c driver-id/c driver/c))

; TODO41 rename to driver-cmd
(define driver-cmd/c hash?)
(define driver-resp/c hash?)

(define (driver-spec-api ds) (hash-ref ds 'api-id))
(define (driver-spec-pre-fn ds) (hash-ref ds 'pre))
(define (driver-spec-post-fn ds) (hash-ref ds 'post))
