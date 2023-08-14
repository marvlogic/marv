#lang racket/base
(require racket/contract)

(require "resources.rkt")
(require "driver.rkt")
(require "state.rkt")

(provide init-dev-driver)

(define (init-dev-driver interface-id)

  (define driver-conf (hash))

  (define/contract (create resource)
    (config/c . -> . config/c)
    (hash-set* resource
               'selfLink (format "dev:~a:~a" (hash-ref resource '$type) (hash-ref resource 'name)))
    ; 'project "https://shouldnt-see-this"))
    )

  (define/contract (readr resource-state)
    (config/c . -> . config/c)
    resource-state)

  (define/contract (update resource-state)
    (config/c . -> . config/c)
    resource-state)

  (define/contract (delete resource-state)
    (config/c . -> . config/c)
    resource-state)

  (define/contract (validate res)
    (config/c . -> . config/c)
    res)

  (define (http-transport method url res)(displayln (format "~a: ~a\n~a" method url res)))

  (driver validate create readr update delete driver-conf))