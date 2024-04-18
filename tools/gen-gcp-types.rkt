#lang racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/list)
(require marv/drivers/gcp/discovery)
(require racket/match)
(require marv/utils/hash)
(require marv/core/globals)

(define compute  (load-discovery "gcp" "compute:beta"))
(define iam  (load-discovery "gcp" "iam:v1"))
(define storage  (load-discovery "gcp" "storage:v1"))
(define secretmanager  (load-discovery "gcp" "secretmanager:v1"))
(define sql  (load-discovery "gcp" "sqladmin:v1"))

; (get-discovery-resources secretmanager)
; (api-by-resource-path secretmanager "/resources/projects/resources/secrets" "create")

(define (gen-all)

  (define (gen-auto-file disc api-name create read update delete)
    (define (schemas) (gen-schemas disc))
    (define (gen) (gen-types disc api-name create read update delete))
    (define (shim) (gen-shim disc api-name))
    (with-output-to-file #:exists 'replace (format "types/gcp/_auto/~a-schema.mrv" api-name) schemas)
    (with-output-to-file #:exists 'replace (format "types/gcp/_auto/~a.mrv" api-name) gen)
    (with-output-to-file #:exists 'replace (format "types/gcp/~a-shim.mrv" api-name) shim))

  (gen-auto-file storage "storage" "insert" "get" "patch" "delete")
  (gen-auto-file compute "compute" "insert" "get" "patch" "delete")
  (gen-auto-file iam "iam" "create" "get" "patch" "delete")
  (gen-auto-file secretmanager "secretmanager" "create" "get" "patch" "delete")
  (gen-auto-file sql "sql" "insert" "get" "patch" "delete")
  )

(define (gen-schemas disc)
  (displayln #<<EOF
#lang marv
## AUTO-GENERATED FILE - DO NOT EDIT!

EOF
             )
  (for ([type (disc-schemas disc)])
    (define schema (get-disc-schema disc type))
    (define properties (hash-keys (hash-ref schema 'properties)))
    (displayln
     (format #<<EOF

# Schema for ~a
~a(c)=c << [~a]
export ~a
EOF
             type type  (symbols->string properties) type )))
  (displayln "schemas={")
  (for ([type (disc-schemas disc)])
    (displayln (format "  ~a=~a" type type)))
  (displayln "}\nexport schemas"))


(define res-spec-verbs
  (hash
   "/resources/projects/resources/secrets/resources/versions"
   '("/resources/projects/resources/secrets:addVersion"
     ":access"
     "/resources/projects/resources/secrets:addVersion"
     ":destroy")
   ))

(define (gen-types disc api-id api-create api-read api-update api-delete)
  (define types (get-discovery-resources disc))
  (displayln
   (format #<<EOF
#lang marv
## AUTO-GENERATED FILE - DO NOT EDIT!
import types/gcp/_auto/~a-schema
API-ID="~a"
EOF
           api-id api-id))
  (for ([res-path (in-list types)])
    ; TODO41- filter out empty apis

    (define (parse-verb->api v)
      (match (string-split v ":")
        [(list path method) (api-by-resource-path disc path method)]
        [(list method) (api-by-resource-path disc res-path method)]))

    (define (api-spec verb)
      (define api (parse-verb->api verb))
      (define (request-clause)
        (let ((request-type (api-request-type api)))
          (if (string? request-type) (format "request-body=~a(cfg)|" request-type) "" )))

      (define spec
        (cond
          [(disc-api? api)
           (format "api-id=API-ID|~aresponse-type=\"~a.schemas.~a\"|method=\"~a\"|url=\"~a\"|required=[\"~a\"]"
                   (request-clause) api-id (api-response-type api) (api-http-method api)
                   (api-resource-url-base api)
                   (map symbol->string (api-required-params api)))]
          [else ""]))
      (string-replace spec "|" "\n   "))

    (define type (last (string-split res-path "/")))
    (define-values (create read update delete)
      (apply values (hash-ref res-spec-verbs res-path (list api-create api-read api-update api-delete))))
    (displayln
     (format
      #<<EOF
# ~a
type ~a = {
 # TODO41 - destructors
 origin(cfg)= cfg <- {
  driver="gcp"
 }
 create(cfg)={
  config=cfg
  api={
   ~a
  }
 }
 read(cfg)={
  config=cfg
  api={
   ~a
  }
 }
 update(cfg)={
  config=cfg
  api={
   ~a
  }
 }
 delete(cfg)={
  config=cfg
  api={
   ~a
  }
 }
}
export ~a

EOF
      res-path type  (api-spec create) (api-spec read) (api-spec update) (api-spec delete) type
      ))))

(define (singularise str)
  (cond [(string-suffix? str "ies") (string-append (string-trim str "ies") "y")]
        [else (string-trim #:left? #f str "s")]))

(define (gen-shim disc api-name)
  (define types (get-discovery-resources disc))
  (displayln (format #<<EOF
#lang marv

# WARNING: This shim file is incomplete. You need to define a '_base' type
# relevant to the API being imported, and then remove this warning.

import types/gcp/_auto/~a as _auto
EOF
                     api-name))
  (for ([t (in-list types)])
    (define type(last (string-split t "/")))
    (define sgl (singularise type))

    (displayln (format #<<EOF
type ~a =  _base | _auto:~a
export ~a

EOF
                       sgl type sgl))))

