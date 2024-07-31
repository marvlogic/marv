
#lang racket/base

(require racket/contract)
(require racket/pretty)
(require racket/string)
(require marv/core/config)
(require marv/utils/hash)
(require marv/core/values)

(provide hash->attribute
         flat-attributes
         res-id/c prefix-id list->id id->list
         resource
         resource-call
         resource-origin
         resource/c
         origin/c
         resource-set/c
         resource-type-fn
         config-resolve
         (struct-out resource)
         (struct-out attribute))

(struct attribute (name value) #:prefab)

(struct resource (gid type deps config) #:prefab)

(define res-id/c symbol?)

(define origin/c hash?)

; TODO45 - resource-type stuff/names and consolidate (vs lifecycle)
(define/contract (resource-type-fn res)
  (resource? . -> . any/c)
  (lambda(verb . args)(apply (hash-ref (resource-type res) verb) args)))

(define/contract (resource-call verb res)
  (symbol? resource? . -> . any/c)
  ((hash-ref (resource-type res) verb) (resource-config res)))

(define/contract (resource-origin res)
  (resource? . -> . origin/c)
  (resource-call 'origin res))

(define/contract (prefix-id prf id)
  (res-id/c res-id/c . -> . res-id/c )
  (string->symbol (format "~a.~a" prf id)))

(define/contract (id->list id)
  (res-id/c . -> . (listof symbol?))
  (map string->symbol (string-split (symbol->string id) ".")))

(define/contract (list->id lst)
  ((listof symbol?) . -> . res-id/c)
  (string->symbol (string-join (map symbol->string lst) ".")))

; TODO45
(define type? hash?)

(define resource/c (struct/c resource symbol? type? list? config/c))

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

(define (resolve-ref r get-by-gid)
  ; (log-marv-debug "-> attempting to resolve: ~a" r)
  (if (ref? r)
      (hash-nref (get-by-gid (ref-gid r)) (id->list (ref-path r)) (lambda()(raise "arg")))
      r))

(define (resolve-deferred d get-by-gid)

  ; (log-marv-debug "Resolving: ~a:~a:~a" op term1 term2)

  (define (handle-term t)
    (define tr (resolve-ref t get-by-gid))
    (if (deferred? tr) (resolve-deferred tr get-by-gid) tr))

  ; (displayln (format "===> DTs: ~a" (pretty-format (deferred-terms d))))
  (define resolved (map handle-term (deferred-terms d)))
  (when (memf deferred? resolved) (raise "aiiiieee"))
  (apply (deferred-op d) resolved))

(define (config-resolve cfg get-by-gid)
  (define (process _ v)
    ; (displayln (format "===> LOOKING at (~a) ~a " (deferred? v) (pretty-format v)))
    (if (deferred? v) (resolve-deferred v get-by-gid) (resolve-ref v get-by-gid)))
  (hash-apply cfg process))
