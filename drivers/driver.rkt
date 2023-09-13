#lang racket/base

(require racket/contract)

(require marv/core/resources)

(provide make-mk-resource-fn make-driver-crud-fn)

(define crud/c symbol?)
(define crudfn/c (crud/c . -> . resource/c))

(define driver-id/c symbol?)

(define (make-driver-crud-fn create readr update delete)
  (define/contract (crud op)
    crudfn/c
    (define fn
      (case op
        ['create create]
        ['read readr]
        ['update update]
        ['delete delete]
        [else (raise "unsupported crud-fn op")]))
    (fn))
  crud)

; TODO - procedure parameters contract
(define driver/c procedure?)

(define/contract (make-mk-resource-fn drivers)
  ((hash/c driver-id/c driver/c) . -> . driver/c)

  (define/contract (mk-resource driver-id config)
    (driver-id/c config/c . -> . resource/c)
    (define mkres (hash-ref drivers driver-id))
    (mkres config))

  mk-resource)
