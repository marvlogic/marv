
#lang racket/base

(require racket/contract)
(require racket/string)
(require marv/core/config)

(provide hash->attribute
         flat-attributes
         res-id/c prefix-id list->id id->list
         resource
         resource/c
         resource-set/c
         (struct-out resource)
         (struct-out attribute))

(struct attribute (name value) #:prefab)
(struct resource (type-fn config) #:prefab)

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

(define resource/c (struct/c resource procedure? config/c))

(define resource-set/c (hash/c res-id/c resource/c))

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