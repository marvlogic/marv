#lang racket/base

(require racket/contract)
(require marv/core/resource-types)
(provide (all-defined-out))

(define crud/c symbol?)
(define crudfn/c (crud/c . -> . resource/c))
(define driver-id/c symbol?)
(define driver/c procedure?)
(define driver-set/c (hash/c driver-id/c driver/c))
