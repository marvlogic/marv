#lang racket/base

(require racket/contract)
(require marv/core/config)
(provide (all-defined-out))

(define msg-id/c symbol?)
(define crudfn/c (msg-id/c config/c . -> . config/c))
(define driver-id/c symbol?)
(define driver/c procedure?)
(define driver-set/c (hash/c driver-id/c driver/c))
