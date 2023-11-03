#lang racket/base

(require json)
(require racket/contract)
(require racket/pretty)
(require racket/list)
(require racket/set)
(require racket/match)
(require racket/string)
(require uri-template)
(require racket/dict)
(require marv/log)
(require marv/utils/hash)
(require marv/core/globals)
(require marv/core/config)

(provide load-discovery
         api-for-type-op
         api-http-method
         api-resource-url
         api-required-params
         api-resource
         api-response-type
         disc-api?)

(define (raise-exn fstr . vs) (apply error 'discovery fstr vs))

(struct disc-doc (root type-op-patches) #:transparent)
(struct disc-api (root type-api) #:transparent)

(define ROOT-DISCOVERY "https://discovery.googleapis.com/discovery/v1/apis")

(define/contract (load-discovery int-id disc-id [type-op-patches (hash)])
  ((string? string?) (hash?) . ->* . disc-doc?)
  (define disc-url (dict-ref (get-root-discovery int-id) disc-id))
  (with-workspace-file int-id disc-id
    #:thunk (lambda() (disc-doc(read-json) type-op-patches)) #:url disc-url))

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

  ; uri-template library needs an 'equal?' hash
  (define path-parameters
    (hash-map/copy
     (make-immutable-hash (hash->list alias-cfg))
     (lambda(k v) (values (symbol->string k) v))))

  (define api-root (disc-api-root api))
  (define path (format "~a" (hash-ref (disc-api-type-api api) 'path)))
  (log-marv-debug "path: ~v, imm: ~v" path path-parameters)
  (log-marv-debug "exp: ~v" (expand-template path path-parameters))
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

(define/contract (api-required-params api)
  (disc-api? . -> . list?)
  (define params (hash-ref (disc-api-type-api api) 'parameters))
  (filter (lambda (k) (hash-nref params (list k 'required) #f)) (hash-keys params)))

(define/contract (api-path-parameters api)
  (disc-api? .  -> . hash?)
  (hash-filter (hash-ref (disc-api-type-api api) 'parameters)
               (lambda (k v) (equal? "path" (hash-ref v 'location)))))

(define/contract (api-query-params api)
  (disc-api? . -> . list?)
  (define params (hash-ref (disc-api-type-api api) 'parameters))
  (hash-keys
   (hash-filter
    params
    (lambda (_ v) (equal? "query" (hash-ref v 'location))))))

(define/contract (api-query-params-str api)
  (disc-api? . -> . string?)
  (define qps
    (map
     (lambda (param) (format "~a={~a}" param param)) (api-query-params api) ))
  (string-join qps "&"))

(define/contract (api-resource api config)
  (disc-api? config/c . -> . config/c)
  (define req-type (api-request-type api))
  (cond [req-type
         (define api-root (disc-api-root api))
         (define schema (api-schema api-root req-type))
         (hash-take config (hash-keys schema))]
        [else (hash)]))

(define/contract (api-response-type api)
  (disc-api? . -> . (or/c #f string?))
  (hash-nref (disc-api-type-api api) '(response $ref) #f))

(define/contract (api-request-type api)
  (disc-api? . -> . (or/c #f string?))
  (hash-nref (disc-api-type-api api) '(request $ref) #f))

(define/contract (api-schema disc type)
  (hash? string? . -> . hash?)
  (hash-nref disc (list 'schemas (string->symbol type) 'properties)))

(define (api-resource-keys doc)
  (pretty-print
   (make-immutable-hash (map (lambda(k) (cons k k))
                             (hash-keys (hash-ref (disc-doc-root doc) 'resources))))))

; (define comp (load-discovery "gcp" "compute:beta"))
(define secret (load-discovery "gcp" "secretmanager:v1"))