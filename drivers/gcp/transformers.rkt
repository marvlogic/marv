#lang racket/base

(require marv/utils/hash)
(provide apply-request-transformer)

(define (mig-create-instances res)
  (hash-set (hash-keep res '(name project zone instanceGroupManager))
            'instances (list (hash-nremove res '(zone instanceGroupManager project)))))

(define (mig-delete-instances res)
  (hash-set (hash-keep res '(name project zone instanceGroupManager))
            ; TODO - this is better done in a response-transformer
            'instances (list (format "zones/~a/instances/~a" (hash-ref res 'zone) (hash-ref res 'name)))))

(define request-transformers
  (hash
   'compute.instanceGroupManagers.createInstances mig-create-instances
   'compute.instanceGroupManagers.deleteInstances mig-delete-instances
   ))

(define (apply-request-transformer type-op resource)
  ((hash-ref request-transformers type-op (lambda() (lambda(r)resource))) resource))

(define inst (hash
              'name "example-kbh"
              'project 'abc
              'size "123"
              'instanceGroupManager "name"
              'zone  "eu1"
              ))

(apply-request-transformer 'compute.instanceGroupManagers.createInstances inst)
(apply-request-transformer 'compute.instanceGroupManagers.deleteInstances inst)