#lang racket/base
(require racket/contract)

(require marv/core/resources)
(require marv/core/config)
(require marv/drivers/driver)
(require marv/drivers/types)

(provide init-dev-driver)

(define (init-dev-driver interface-id)

  (define/contract (create resource)
    (config/c . -> . config/c)
    (displayln ". DONE")
    (hash-set* resource
               'selfLink (format "dev:~a:~a" (hash-ref resource '$type)
                                 (hash-ref resource 'name 'no-name))))

  (define/contract (readr resource-state)
    (config/c . -> . config/c)
    resource-state)

  (define/contract (update resource-state)
    (config/c . -> . config/c)
    (displayln ". DONE")
    resource-state)

  (define/contract (delete resource-state)
    (config/c . -> . config/c)
    (displayln ". DONE")
    resource-state)

  (define/contract (validate res)
    (config/c . -> . config/c)
    res)

  (define (http-transport method url res)(displayln (format "~a: ~a\n~a" method url res)))

  (make-driver-crud-fn validate create readr update delete))