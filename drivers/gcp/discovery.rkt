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
         api-for-type-op
         api-http-method
         api-resource-url
         api-parameters
         api-required-params
         api-resource
         api-response-type
         api-request-type
         api-schema
         disc-doc?
         api-display-docs
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

; TODO - better name for type-op stuff?
(define/contract (api-for-type-op discovery type-op)
  (disc-doc? symbol? . -> . disc-api?)

  (define (consult-discovery-doc)
    (define (find-api res-tree type-path)
      (define hs (hash-ref res-tree 'resources))
      (match type-path
        [(list t op) (hash-nref hs (list t 'methods op))]
        [(list t ts ...) (find-api (hash-ref hs t) ts)]))

    ; TODO14 - exception handling
    (define res-tree (disc-doc-root discovery))
    (define path (cdr (split-symbol type-op)))
    (define api (find-api res-tree path))

    ; This check covers the assumption that the path through the discovery
    ; document should match up with the id of the API. e.g the IAM discovery
    ; document path "resources.projects.resources.serviceAccounts.methods.get"
    ; should have an API with id = "iam.projects.serviceAccount.get"

    (define api-id (string->symbol (hash-ref api 'id)))
    (when (not (eq? api-id type-op))
      (raise-exn "API for ~v did not match in API's id field (~v)" type-op api-id))
    api)

  (define patches (disc-doc-type-op-patches discovery))
  (disc-api (disc-doc-root discovery) (hash-ref patches type-op consult-discovery-doc)))

(define/contract (api-http-method api)
  (disc-api? . -> . symbol?)
  (string->symbol(hash-ref (disc-api-type-api api) 'httpMethod)))

(define/contract (api-resource-url api config)
  (disc-api? config/c . -> . string?)

  ; NB this area is bound to give problems in the future, because assumptions are
  ; made for all APIs that we can provide a missing required-parameter by using
  ; the 'name' attribute from a resource config

  (define reqd-params (api-required-params api))
  (define config-params (hash-take config reqd-params))
  (define missing-params (set-subtract reqd-params (hash-keys config-params) ))

  (define alias-cfg
    (case (set-count missing-params)
      [(0) config-params]
      [(1)
       (define missing (set-first missing-params))
       (define name (hash-ref config 'name))
       (log-marv-warn "missing parameter: ~v, assuming ~v is ok" missing name)
       (hash-set config-params missing name)]
      [else (raise-exn "missing too many path parameters: ~v, need: ~v" missing-params reqd-params)]))

  ; uri-template library needs an 'equal?' hash, using strings as keys
  (define path-parameters
    (hash-map/copy
     (make-immutable-hash (hash->list alias-cfg))
     (lambda(k v) (values (symbol->string k) v))))

  ; for apis (secret-manager, storage) that have required query parameters, but
  ; don't have the template variables for them in the 'path' setting:

  (define query-params-str
    (match (set-intersect reqd-params (api-query-parameters api))
      [(list qps ..1) (string-join
                       (for/list ([q qps]) (format "~a={~a}" q q))
                       "&" #:before-first "?")]
      [else ""]))

  (define path (format "~a~a" (hash-ref (disc-api-type-api api) 'path) query-params-str))
  (log-marv-debug "path: ~v, imm: ~v" path path-parameters)
  (log-marv-debug "exp: ~v" (expand-template path path-parameters))
  (define api-root (disc-api-root api))
  (define url
    (format "~a~a~a"
            (hash-ref api-root 'rootUrl)
            (hash-ref api-root 'servicePath)
            (expand-template path path-parameters)))
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

(define/contract (api-schema api type)
  (disc-api? string? . -> . hash?)
  (hash-nref (disc-api-root api) (list 'schemas (string->symbol type) 'properties)))

; Utility for printing out resource IDs from a discovery-document
(define (api-resource-keys doc)
  (pretty-print
   (make-immutable-hash (map (lambda(k) (cons k k))
                             (hash-keys (hash-ref (disc-doc-root doc) 'resources))))))

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