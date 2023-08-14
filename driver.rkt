#lang racket/base

(require racket/contract)

(require "resources.rkt")
(require "values.rkt")

(provide (struct-out driver)
         init-drivers
         driver-config
         make-master-driver)

(struct driver (mkres create read update delete config) #:prefab)

(define DRIVERS (make-parameter #f))

(define/contract (init-drivers drivers)
  ((hash/c symbol? driver?) . -> . (hash/c any/c driver?))
  (DRIVERS drivers)
  (DRIVERS))

(define driver-id/c symbol?)

(define/contract (make-master-driver drivers)
  ((hash/c symbol? driver?) . -> . driver?)

  (define (apply-crudfn crud-fn r)
    (define drv (hash-ref drivers (resource-driver r)))
    (resource (resource-driver r)
              ((crud-fn drv) (resource-config r))))

  (define/contract (drv-create resource)
    (resource/c . -> . resource/c)
    (apply-crudfn driver-create resource))

  (define/contract (drv-read resource)
    (resource/c . -> . resource/c)
    (apply-crudfn driver-read resource))

  (define/contract (drv-update resource)
    (resource/c . -> . resource/c)
    (apply-crudfn driver-update resource))

  (define/contract (drv-delete resource)
    (resource/c . -> . resource/c )
    (apply-crudfn driver-delete resource))

  (define/contract (drv-mk-resource driver-id config)
    (driver-id/c config/c . -> . resource/c)
    (resource driver-id ((driver-mkres (hash-ref drivers driver-id)) config)))

  (driver drv-mk-resource drv-create drv-read drv-update drv-delete (hash)))