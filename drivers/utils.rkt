#lang racket/base

(require racket/contract)
(require marv/drivers/types)

(provide make-driver-crud-fn gcp-type)

(define (raise-unsupported op res) (raise (format "Unsupported message/operation: ~a = ~a" op res)))

(define (gcp-type r) (hash-ref r '$type))

(define (make-driver-crud-fn validate create readr update delete (pass-thru-fn raise-unsupported))
  (define/contract (crud op res)
    crudfn/c
    (define fn
      (case op
        ['validate validate]
        ['create create]
        ['read readr]
        ['update update]
        ['delete delete]
        [else (pass-thru-fn op res)]))
    (fn res))
  crud)