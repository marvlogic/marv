#lang racket/base

(require marv/utils/hash)
(require marv/log)
(provide register-request-transformer apply-request-transformer
         (struct-out transformer))

(struct transformer (api-id fn) #:transparent)

(define (mig-create-instances res)
  (hash-set (hash-take res '(name project zone instanceGroupManager))
            'instances (list (hash-take res '(name preservedState)))))

(define (mig-delete-instances res)
  (hash-set (hash-take res '(name project zone instanceGroupManager))
            ; TODO - this is better done in a response-transformer
            'instances (list (format "zones/~a/instances/~a" (hash-ref res 'zone) (hash-ref res 'name)))))

(define request-transformers
  (make-parameter
   (hash
    ; 'compute.instanceGroupManagers.createInstances mig-create-instances
    ; 'compute.instanceGroupManagers.deleteInstances mig-delete-instances
    )))

(define (register-request-transformer t)
  (log-marv-info "Registering transformer: ~a" t)
  (request-transformers
   (hash-set (request-transformers) (transformer-api-id t) (transformer-fn t))))

(define (apply-request-transformer type-op resource)
  ((hash-ref (request-transformers) type-op (lambda() (lambda(r)resource))) resource))

; (define inst (hash
;               'name "example-kbh"
;               'project 'abc
;               'size "123"
;               'instanceGroupManager "name"
;               'zone  "eu1"
;               ))

; (apply-request-transformer 'compute.instanceGroupManagers.createInstances inst)
; (apply-request-transformer 'compute.instanceGroupManagers.deleteInstances inst)