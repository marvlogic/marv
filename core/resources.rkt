
#lang racket/base

(require racket/contract)
(require racket/string)
(require marv/core/config)
(require marv/drivers/types)

(provide hash->attribute
         flat-attributes
         mk-rmodule
         res-id/c prefix-id list->id id->list
         resource
         resource/c
         resource-set/c
         rmodule/c
         (struct-out resource)
         (struct-out rmodule)
         (struct-out attribute))

(struct attribute (name value) #:prefab)
(struct resource (driver-id config) #:prefab)

(define res-id/c symbol?)

(define/contract (prefix-id prf id)
  (res-id/c res-id/c . -> . res-id/c )
  (string->symbol (format "~a.~a" prf id)))

(define/contract (id->list id)
  (res-id/c . -> . (listof symbol?))
  (map string->symbol (string-split (symbol->string id) ".")))

(define/contract (list->id lst)
  ((listof symbol?) . -> . res-id/c)
  (string->symbol (string-join (map symbol->string lst) ".")))

(define resource/c (struct/c resource driver-id/c config/c))

(struct rmodule (drivers resources))

(define rmodule/c rmodule?)

(define resource-set/c (hash/c res-id/c resource/c))

(define/contract (mk-rmodule drivers resources)
  (driver-set/c resource-set/c . -> . rmodule/c)
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