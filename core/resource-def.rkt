#lang racket/base

(require racket/contract)
(require racket/match)
(require racket/pretty)
(require racket/set)
(require racket/list)
(require racket/hash)
(require racket/string)
(require racket/path)

(require marv/drivers/driver)
(require marv/core/resources)
(require marv/core/values)
(require marv/utils/hash)

(provide init-module get-module
         resource-keys resource-ref
         module-ref
         mod-ref-driver?
         get-resource-params validate-params
         resource-refs
         unpack-value
         update-val
         unwrap-values
         (struct-out ref)
         (struct-out value)
         ref->id ref->pid
         mod-id->id
         ival ival?
         iref iref?
         match-resource-attr?
         deref-resource
         (struct-out params))

(struct params (required accepted))

(define RESOURCES (make-parameter (lambda oo (list))))
(define DRIVERS (make-parameter (lambda() (hash))))

(define/contract (init-module f purge?)
  (string? boolean? . -> . void?)
  (define rel-mod (find-relative-path (current-directory) f) )
  (DRIVERS (dynamic-require rel-mod 'drivers))
  (unless purge?
    (RESOURCES (dynamic-require rel-mod 'main)))
  (void))

(define/contract (get-module params)
  ((hash/c symbol? string?) . -> . rmodule/c)
  ; (validate-params params)
  (define driver (make-driver-for-set ((DRIVERS))))
  (define (mk-resource driver-id config) (resource driver-id (driver driver-id 'validate config)))
  (define resource-list ((RESOURCES) 'main mk-resource params))
  (mk-rmodule ((DRIVERS)) (resource-list->hash resource-list)))

(define/contract (resource-list->hash resources)
  ((listof (cons/c res-id/c resource/c)) . -> . (hash/c res-id/c resource/c))

  (define (check-dups acc-hs res-id res)
    (when (hash-has-key? acc-hs res-id) (raise (format "duplicate resource id: ~a" res-id)))
    (hash-set acc-hs res-id res))

  (for/fold ([hs (hash)])
            ([r (in-list resources)])
    (check-dups hs (car r) (cdr r))))


(define (make-keyword-params params)
  ; TODO: test for check that #t (sorting) works
  (hash-map params (lambda(k v) (cons (string->keyword k) v)) #t))

(define/contract (validate-params input-params)
  ((hash/c string? string?) . -> . void?)
  (define invalids (set-subtract (hash-keys input-params)
                                 (params-accepted (get-resource-params))))
  (when (not (empty? invalids))
    (raise (format "Invalid parameters: ~a\n(Valid are: ~a)"
                   invalids (params-accepted (get-resource-params)))))
  (define missing (set-subtract (params-required (get-resource-params))
                                (hash-keys input-params)))
  (when (not (empty? missing))
    (raise (format "Missing required parameters ~a" missing))))

(define (get-resource-params)
  (define-values (required accepted) (procedure-keywords (RESOURCES)))
  (params (map keyword->string required) (map keyword->string accepted))
  )

; TODO - tighten the contracts
(define/contract (resource-keys rs)
  (any/c . -> . (listof any/c))
  (hash-keys (rmodule-resources rs)))

(define/contract (resource-ref mod k)
  (rmodule/c res-id/c . -> . resource/c)
  (hash-ref (rmodule-resources mod) k))

; module-ref returns the resource, or the driver-config if a driver
(define/contract (module-ref mod k)
  (any/c any/c . -> . any/c)
  (define ref-spec (map string->symbol (string-split (symbol->string k) ".")))
  (hash-nref mod ref-spec))

(define (mod-ref-driver? ref)
  (define ref-spec (map string->symbol (string-split (symbol->string ref) ".")))
  (eq? '$drivers (car ref-spec)))

(define/contract (resource-refs res)
  (resource/c . -> . (listof (cons/c any/c ref?)))
  (filter (compose1 ref? cdr)
          (map (lambda (kv) (cons (car kv) (unpack-value(cdr kv))))
               (hash->flatlist (resource-config res)))))

(define (match-resource-attr? res matchf) (hash-match? res matchf))

(define/contract (unwrap-values res)
  (resource/c . -> . resource/c)

  (define (unwrap k v)  (unpack-value v))

  (resource (resource-driver-id res) (hash-apply (resource-config res) unwrap)))

; TODO - NB, the $resources/$drivers definition stuff has been left in for now,
; not sure if it will be needed in future.

(define (mod-id->id mid)
  (string->symbol
   (match (string-split (symbol->string mid) ".")
     [(list "$resources" id _ ...) id]
     [(list "$drivers" id _ ...) id]
     [(list id _ ...) id]
     [else (raise (format "~a: Bad reference format" (ref-path ref)))])))

(define (ref->id ref)
  (string->symbol
   (match (string-split (symbol->string (ref-path (unpack-value ref))) ".")
     [(list "$resources" id _ ..1) id]
     [(list "$drivers" id _ ...) id]
     [(list id _ ...) id]
     [else (raise (format "~a: Bad reference format" (ref-path ref)))])))

(define (ref->pid ref)
  (match (string-split (symbol->string (ref-path (unpack-value ref))) ".")
    [(list "$resources" id _ ...) (string->symbol (format "$resources.~a" id))]
    [(list "$drivers" id _ ...) (string->symbol (format "$drivers.~a" id))]
    [(list id _ ...) (string->symbol (format "$resources.~a" id))]
    [else (raise (format "~a: Bad reference format" (ref-path ref)))]))

(define (get-ref mod ref)
  (define ref-spec (ref->list ref))
  (hash-nref (resource-config (resource-ref mod (car ref-spec))) (drop ref-spec 1)))

(define/contract (deref-resource mod r)
  (rmodule/c resource/c . -> . resource/c)
  (define (deref-attr _ a)
    (update-val a (lambda (v) (if (ref? v) (unpack-value(get-ref mod v)) v))))
  (resource (resource-driver-id r) (hash-apply (resource-config r) deref-attr)))