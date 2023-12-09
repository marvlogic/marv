#lang racket/base

(require racket/contract)
(require racket/match)
(require racket/pretty)
(require racket/set)
(require racket/list)
(require racket/hash)
(require racket/string)
(require racket/path)

(require marv/core/drivers)
(require marv/core/resources)
(require marv/core/values)
(require marv/utils/hash)
(require marv/log)

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
         mod-id->id
         ival ival?
         iref iref?
         match-resource-attr?
         deref-resource
         (struct-out params))

(struct params (required accepted))

; TODO - using parameters for this isn't necessary
(define RESOURCES (make-parameter (lambda take-any-params (hash))))

(define/contract (init-module f)
  (string? . -> . void?)
  (define rel-mod (find-relative-path (current-directory) f) )
  (RESOURCES (dynamic-require rel-mod 'main))
  (void))

(define/contract (get-module params purge?)
  ((hash/c symbol? string?) boolean? . -> . resource-set/c)
  ; (validate-params params)
  (define resources ((RESOURCES) 'main params))
  (displayln (RESOURCES))
  (if purge? (hash) resources))

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
  (hash-keys rs))

(define/contract (resource-ref mod k)
  (resource-set/c res-id/c . -> . resource/c)
  (hash-ref mod k))

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
  ; TODO41 - refactor to resource-update-config-fn
  (resource (resource-type-fn res) (hash-apply (resource-config res) unwrap)))

; TODO - NB, the $resources/$drivers definition stuff has been left in for now,
; not sure if it will be needed in future.

(define (mod-id->id mid)
  (string->symbol
   (match (string-split (symbol->string mid) ".")
     [(list "$resources" id _ ...) id]
     [(list "$drivers" id _ ...) id]
     [(list id _ ...) (format "~a" mid)]
     [else (raise (format "~a: Bad reference format" (ref-path ref)))])))

(define/contract (deref-resource mod r)
  (resource-set/c resource/c . -> . resource/c)

  (define (get-ref ref)
    (define-values (res-id attr) (ref-split ref))
    (unpack-value (hash-nref (resource-config (resource-ref mod res-id)) (id->list attr))))

  (define (deref-attr _ a)
    (update-val a (lambda (v) (if (ref? v) (get-ref v) v))))

  ; TODO41 - refactor to resource-update-config-fn
  (resource (resource-type-fn r) (hash-apply (resource-config r) deref-attr)))