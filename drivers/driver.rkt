#lang racket/base

(require racket/contract)

(require marv/core/resources)

(provide make-driver-for-set
         make-driver-crud-fn
         (struct-out driver)
         ;  resource-create
         ;  resource-read
         ;  resource-update
         ;  resource-delete)
         )

(struct driver (mk-resource crudfn))

(define crud/c symbol?)
(define crudfn/c (crud/c resource/c . -> . resource/c))
(define driver-id/c symbol?)
(define driver/c driver?)
(define driver-set/c (hash/c driver-id/c driver/c))


; (define (resource-create r) (displayln "create")((resource-crudfn r) 'create))
; (define (resource-read r) ((resource-crudfn r) 'read))
; (define (resource-update r) ((resource-crudfn r) 'update))
; (define (resource-delete r) ((resource-crudfn r) 'delete))

(define (make-driver-crud-fn create readr update delete)
  (define/contract (crud op res)
    crudfn/c
    (define fn
      (case op
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

  (define/contract (mk-resource driver-id config)
    (driver-id/c config/c . -> . resource/c)
    (define mkres (driver-mk-resource (hash-ref drivers driver-id)))
    (mkres driver-id config))

  (define/contract (crudfn-for op resource)
    (crud/c resource/c . -> . resource/c)
    (define driver-id (resource-driver-id resource))
    (define crud (driver-crudfn (hash-ref drivers driver-id)))
    (crud op resource))

  (driver mk-resource crudfn-for))