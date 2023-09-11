#lang racket/base

(require json)
(require racket/contract)
(require racket/pretty)
(require racket/list)
(require racket/string)
(require racket/dict)
(require marv/utils/hash)
(require marv/core/globals)

(provide load-discovery
         api-for-type
         api-for-type-op
         api-http-method
         api-resource-url
         api-required-params
         api-resource
         api-response-type)

(struct disc-doc (root))
(struct disc-api (root type-api))

(define ROOT-DISCOVERY "https://discovery.googleapis.com/discovery/v1/apis")

(define/contract (load-discovery int-id disc-id)
  (string? string? . -> . disc-doc?)
  (define disc-url (dict-ref (get-root-discovery int-id) disc-id))
  (with-workspace-file int-id disc-id
    #:thunk (lambda() (disc-doc(read-json))) #:url disc-url))

(define (get-root-discovery int-id)
  (with-workspace-file int-id "discovery-index.json"
    #:thunk (lambda ()
              (map
               (lambda (disc-ent) (cons (dict-ref disc-ent 'id) (dict-ref disc-ent 'discoveryRestUrl)))
               (dict-ref (read-json) 'items)))
    #:url ROOT-DISCOVERY))

(define/contract (api-for-type discovery type op)
  (disc-doc? symbol? symbol? . -> . disc-api?)
  (disc-api (disc-doc-root discovery)
            (hash-ref (hash-nref (disc-doc-root discovery) (list 'resources type 'methods)) op)))

; TODO - better name for type-op stuff?
(define/contract (api-for-type-op discovery type-op)
  (disc-doc? symbol? . -> . disc-api?)
  ; todo - cache this, or store in disc struct
  (hash-ref (api-types discovery) type-op))

; TODO - is append a code smell?
(define (api-types discovery)
  (make-immutable-hasheq
   (for/fold ([acc '()])
             ([res (hash-values (hash-ref (disc-doc-root discovery) 'resources))])
     (append acc
             (for/fold ([ac2 '()])
                       ([meth (hash-values (hash-ref res 'methods))])
               (cons (cons (string->symbol (hash-ref meth 'id))
                           (disc-api (disc-doc-root discovery) meth)) ac2))))))

; (define/contract (api-by-type-op discovery type-op)
;   (disc-doc? symbol? . -> . disc-api?)
;   (disc-api (disc-doc-root discovery)


(define/contract (api-http-method api)
  (disc-api? . -> . symbol?)
  (string->symbol(hash-ref (disc-api-type-api api) 'httpMethod)))

(define (ref-type api)

  ; TODO - bit of a hack to rely on parameterOrder like this?

  ; Warning! on method='insert, request.$ref has initial Upper case character,
  ; but it's not used in an insert request hence it 'works'.  parameterOrder is
  ; used on get and delete.

  (string->symbol
   (hash-nref (disc-api-type-api api) '(request $ref)
              (last (hash-ref (disc-api-type-api api) 'parameterOrder)))))

(define/contract (api-resource-url api resource)
  (disc-api? hash? . -> . string?)
  (define aliased-resource
    (make-immutable-caseless-string-hash
     (hash-set resource
               (ref-type api) (hash-ref resource 'name))))
  (define url
    (format "~a~a" (hash-ref (disc-api-root api) 'baseUrl)
            (flat-path api)))
  (dict-format-string aliased-resource url))

(define/contract (api-required-params api)
  (disc-api? . -> . list?)
  (define params (hash-ref (disc-api-type-api api) 'parameters))
  (filter (lambda (k) (hash-nref params (list k 'required) #f)) (hash-keys params)))

(define (flat-path api)
  (define type-api (disc-api-type-api api))
  (define (construct-flat-path)
    (format "~a?~a" (hash-ref type-api 'path) (type-api-query-params type-api)))
  (dict-ref type-api 'flatPath construct-flat-path))

(define (type-api-query-params api)
  (define params (hash-ref api 'parameters))
  (string-join
   (map
    (lambda (param) (format "~a={~a}" param param))
    (hash-keys
     (hash-filter
      params
      (lambda (_ v) (and (hash-ref v 'required #f) (equal? "query" (hash-ref v 'location)))))))
   "&"))

(define/contract (api-resource api resource)
  (disc-api? hash? . -> . hash?)
  (define path-parameters
    (hash-filter (hash-ref (disc-api-type-api api) 'parameters)
                 (lambda (k v) (equal? "path" (hash-ref v 'location)))))
  (hash-drop resource (cons '$type (hash-keys path-parameters))))

(define/contract (api-response-type api)
  (disc-api? . -> . (or/c #f string?))
  (hash-nref (disc-api-type-api api) '(response $ref) #f))

(define (api-resource-keys doc)
  (pretty-print
   (make-immutable-hash (map (lambda(k) (cons k k))
                             (hash-keys (hash-ref (disc-doc-root doc) 'resources))))))

(define comp (load-discovery "gcp" "compute:beta"))