
#lang racket/base

; (require "struct-hash.rkt")
(require racket/contract)

(provide hash->attribute
         flat-attributes
         mk-rmodule
         res-id/c
         resource
         resource/c
         resource-set/c
         config/c
         rmodule/c
         (struct-out resource)
         (struct-out rmodule)
         (struct-out attribute))

(struct attribute (name value) #:prefab)
(struct resource (driver config) #:prefab)

; TODO - sort out circular dep if we use driver-id/c instead of 'symbol?'
(define resource/c (struct/c resource symbol? hash?))

; rmodule because module is a keyword :/
; TODO - own module file, and rename to imodule
(struct rmodule (drivers resources))
(define rmodule/c rmodule?)

(define config/c hash?)
(define res-id/c symbol?)

(define resource-set/c (hash/c res-id/c resource/c))

(define/contract (mk-rmodule drivers resources)
  (hash? resource-set/c . -> . rmodule/c)
  (rmodule drivers resources))

(define/contract (hash->attribute h)
  (hash? . -> . (listof attribute?))

  (define (mk-attr k v)
    (cond [(hash? v) (attribute k (hash-map v mk-attr))]
          [else (attribute k v)]))

  (hash-map h mk-attr))

(define/contract (flat-attributes attrs)
  ((listof attribute?) . -> . (listof any/c))

  (define (flat a acc)
    (cond [((listof attribute?) (attribute-value a))
           (foldl flat acc (attribute-value a) )]
          [else (cons a acc)]))

  (foldl flat '() attrs))