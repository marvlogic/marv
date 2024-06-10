#lang racket/base

(require json)
(require racket/contract)
(require racket/pretty)
(require racket/list)
(require racket/set)
(require racket/match)
(require racket/string)
(require racket/dict)
(require uri-template)
(require scribble/text/wrap)

(require marv/log)
(require marv/utils/hash)
(require marv/core/globals)
(require marv/core/config)

(provide load-discovery
         get-discovery-resources
         api-by-resource-path
         api-http-method
         api-resource-url-base
         api-parameters
         api-required-params
         api-resource
         api-response-type
         api-request-type
         disc-schemas
         get-disc-schema
         api-schema
         disc-doc?
         api-display-docs
         api-resource-keys
         disc-api?)

(define (raise-exn fstr . vs) (apply error 'discovery fstr vs))

(struct disc-doc (root type-op-patches) #:transparent)
(struct disc-api (root type-api) #:transparent)

(define ROOT-DISCOVERY "https://discovery.googleapis.com/discovery/v1/apis")

(define/contract (load-discovery int-id disc-id [type-op-patches (hash)])
  ((string? string?) (hash?) . ->* . disc-doc?)
  (define disc-url (dict-ref (get-root-discovery int-id) disc-id))
  (with-workspace-file int-id disc-id
    #:thunk (lambda() (disc-doc(read-json) type-op-patches))
    #:url disc-url))

(define (get-root-discovery int-id)
  (with-workspace-file int-id "discovery-index.json"
    #:thunk (lambda ()
              (map
               (lambda (disc-ent) (cons (dict-ref disc-ent 'id) (dict-ref disc-ent 'discoveryRestUrl)))
               (dict-ref (read-json) 'items)))
    #:url ROOT-DISCOVERY))

(define/contract (get-discovery-resources doc)
  (disc-doc?  . -> . (listof string?))

  (define (extract hs path)
    (for/fold ([found-names '()])
              ([(key value) (in-hash hs)])
      (cond
        [(hash? value)
         (define keypath (format "~a/~a" path key))
         (define next-found-names (if (hash-has-key? value 'methods) (cons keypath found-names) found-names))
         (append next-found-names (extract value keypath))]
        [else found-names])))
  (extract (disc-doc-root doc) ""))

(define/contract (api-by-resource-path disc res-path method)
  (disc-doc? string? string? . -> . (or/c boolean? disc-api?))

  (define (descend hs ks)
    (match ks
      [(list k) (hash-ref hs k #f)]
      [(list k kss ...)(descend (hash-ref hs k) kss)]))

  (define foundit (descend (disc-doc-root disc)
                           (map string->symbol (append (string-split res-path "/") (list "methods" method)))))
  (if foundit (disc-api (disc-doc-root disc) foundit) #f))

(define/contract (api-http-method api)
  (disc-api? . -> . symbol?)
  (string->symbol(hash-ref (disc-api-type-api api) 'httpMethod)))

(define/contract (api-resource-url-base api)
  (disc-api? . -> . string?)

  (define reqd-params (api-required-params api))

  ; for apis (secret-manager, storage) that have required query parameters, but
  ; don't have the template variables for them in the 'path' setting:

  (define query-params-str
    (match (set-intersect reqd-params (api-query-parameters api))
      [(list qps ..1) (string-join
                       (for/list ([q qps]) (format "~a={~a}" q q))
                       "&" #:before-first "?")]
      [else ""]))

  ; TODO - should be doing this per-api as this is assuming the last thing in
  ; the URL inside {} is actually supposed to be {name}
  (define base-path (regexp-replace #rx"{[a-zA-Z0-9]*}$" (hash-ref (disc-api-type-api api) 'path) "{name}"))
  (define path (format "~a~a" base-path query-params-str))
  (define api-root (disc-api-root api))
  (define url
    (format "~a~a~a"
            (hash-ref api-root 'rootUrl)
            (hash-ref api-root 'servicePath)
            path))
  (log-marv-debug "url: ~v" url)
  url)

(define/contract (api-all-params api)
  (disc-api? . -> . list?)
  (hash-keys (hash-ref (disc-api-type-api api) 'parameters)))

(define/contract (api-parameters api [fltr (lambda(_)#t)])
  ((disc-api?) (procedure?) .  ->* . hash?)
  (hash-filter (hash-ref (disc-api-type-api api) 'parameters)
               (lambda (k v) (fltr v))))

(define/contract (api-required-params api)
  (disc-api? . -> . (listof symbol?))
  (define (required? v) (hash-ref v 'required #f))
  (hash-keys(api-parameters api required?)))

(define/contract (api-query-parameters api)
  (disc-api? . -> . (listof symbol?))
  (define (qry? p) (equal? "query" (hash-ref p 'location)))
  (hash-keys(api-parameters api qry?)))

(define/contract (api-resource api config)
  (disc-api? config/c . -> . config/c)
  (define req-type (api-request-type api))
  (cond [req-type
         (define schema (api-schema api req-type))
         (hash-take config (hash-keys schema))]
        [else (hash)]))

(define/contract (api-response-type api)
  (disc-api? . -> . (or/c #f string?))
  (hash-nref (disc-api-type-api api) '(response $ref) #f))

(define/contract (api-request-type api)
  (disc-api? . -> . (or/c #f string?))
  (hash-nref (disc-api-type-api api) '(request $ref) #f))

(define/contract (disc-schemas disc)
  (disc-doc? . -> . (listof symbol?))
  (hash-keys (hash-ref (disc-doc-root disc) 'schemas)))

(define/contract (get-disc-schema disc type)
  (disc-doc? symbol? . -> . hash?)
  (hash-nref (disc-doc-root disc) (list 'schemas type)))

(define/contract (api-schema api type)
  (disc-api? string? . -> . hash?)
  (hash-nref (disc-api-root api) (list 'schemas (string->symbol type) 'properties)))

; Utility for getting resource IDs from a discovery-document
(define (api-resource-keys doc [prefix ""])
  (map (lambda(k) (string->symbol(format "~a~a" prefix k))) (hash-keys (hash-ref (disc-doc-root doc) 'resources))))

(define (api-display-docs api type [subtype #f])

  ; TODO - descend into nested property structures
  (define (handle-type item)
    (define type
      (hash-ref item 'type
                (lambda()
                  (hash-ref item '$ref
                            (string-join
                             (map symbol->string (hash-keys item)) ",")))))
    (case (string->symbol type)
      ['array (format "[ ~a ]" (handle-type (hash-ref item 'items)))]
      ['object
       (format "{ ~a }"
               (handle-type
                (hash-ref item 'properties
                          (lambda()(hash-ref item 'additionalProperties)))))]
      [else type]))

  (define (display-attr name stuff)
    (displayln (format " ~a = ~a" name (handle-type stuff)))
    (for-each
     (lambda(s)(displayln (format " # ~a" s)))
     (wrap-line (hash-ref stuff 'description)))
    (displayln ""))

  (define (display-root)
    (displayln (format "~a {" type))
    (hash-for-each (api-parameters api) display-attr)
    (hash-for-each  (api-schema api type) display-attr)
    (displayln "}"))

  (define (display-nested)
    (define schema-spec (api-schema api type))
    (displayln (format "~a {" subtype))
    (hash-for-each (hash-nref schema-spec (list subtype 'properties)) display-attr)
    (displayln "}"))

  (displayln #<<EOF

# WARNING: This information is auto-generated from discovery-document fields and
# may not be 100% compatible with marv. However it should be a good starting
# point for most GCP APIs.

EOF
             )

  (if subtype (display-nested) (display-root)))