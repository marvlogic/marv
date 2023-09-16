#lang racket/base

(require racket/contract)
(require marv/drivers/types)

(provide make-driver-for-set
         make-driver-crud-fn)

; TODO - it's not crudfn anymore; more generic

(define (raise-unsupported op res) (raise (format "Unsupported message/operation: ~a = ~a" op res)))

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

(define/contract (make-driver-for-set drivers)
  (driver-set/c . -> . driver/c)

  (define/contract (crudfn-for driver-id op msg)
    (driver-id/c msg-id/c any/c . -> . any/c)
    (define crud (hash-ref drivers driver-id))
    (crud op msg))

  crudfn-for)