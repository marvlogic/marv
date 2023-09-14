#lang racket/base

(require racket/contract)
(require marv/core/config)
(require marv/drivers/types)

(provide make-driver-for-set
         make-driver-crud-fn
         ;  (struct-out driver)
         ;  resource-create
         ;  resource-read
         ;  resource-update
         ;  resource-delete)
         )

; (struct driver (mk-resource crudfn))

(define (make-driver-crud-fn validate create readr update delete)
  (define/contract (crud op res)
    crudfn/c
    (define fn
      (case op
        ['validate validate]
        ['create create]
        ['read readr]
        ['update update]
        ['delete delete]
        [else (raise "unsupported crud-fn op")]))
    (fn res))
  crud)

; TODO - procedure parameters contract

(define/contract (make-driver-for-set drivers)
  (driver-set/c . -> . driver/c)

  ; (define/contract (mk-resource driver-id config)
  ;   (driver-id/c config/c . -> . resource/c)
  ;   (define mkres (driver-mk-resource (hash-ref drivers driver-id)))
  ;   (mkres driver-id config))

  (define/contract (crudfn-for driver-id op msg)
    (driver-id/c msg-id/c any/c . -> . any/c)
    (define crud (hash-ref drivers driver-id))
    (crud op msg))

  crudfn-for)