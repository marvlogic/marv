#lang racket/base

(provide (all-defined-out))

(require racket/string)
(require marv/drivers/gcp/discovery)

(define compute  (load-discovery "gcp" "compute:beta"))
(define iam  (load-discovery "gcp" "iam:v1"))
(define storage  (load-discovery "gcp" "storage:v1"))
(define secretmanager  (load-discovery "gcp" "secretmanager:v1"))
(define sql  (load-discovery "gcp" "sqladmin:v1"))


(define (gen-types disc prefix create read update delete)
  ; (define types '("regionDisks"));(api-resource-keys disc))
  (define types (api-resource-keys disc))
  (displayln #<<EOF
#lang marv
## AUTO-GENERATED FILE - DO NOT EDIT!
EOF
             )
  (for ([t (in-list types)])
    (define (api-spec verb)
      (define api-id (string->symbol(format "~a.~a.~a" prefix t verb)))
      (define api (api-for-type-op disc api-id))
      (define spec
        (cond
          [(disc-api? api)
           (format "api-id=\"~a\"|request-type=\"~a\"|response-type=\"~a\"|method=\"~a\"|url=\"~a\"|required=[\"~a\"]"
                   prefix
                   (api-request-type api) (api-response-type api) (api-http-method api)
                   (api-resource-url-base api)
                   (map symbol->string (api-required-params api)))]
          [else ""]))
      (string-replace spec "|" "\n   "))

    (displayln
     (format
      #<<EOF
type ~a = {
 # TODO41 - destructors
 origin(cfg)= cfg <- {
  driver="gcp"
  api={
   ~a
  }
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
      t (api-spec create) (api-spec create) (api-spec read) (api-spec update) (api-spec delete) t
      ))))

(define (singularise str)
  (cond [(string-suffix? str "ies") (string-append (string-trim str "ies") "y")]
        [else (string-trim #:left? #f str "s")]))

(define (gen-shim api-name types)
  (displayln (format #<<EOF
#lang marv

# WARNING: This shim file is incomplete. You need to define a '_base' type
# relevant to the API being imported, and then remove this warning.

import types/gcp/_auto/~a as _auto
EOF
                     api-name))
  (for ([t (in-list types)])
    (define sgl (singularise (symbol->string t)))

    (displayln (format #<<EOF
type ~a =  _base | _auto:~a
export ~a

EOF
                       sgl t sgl))))


(define (gen-all)

  (define (gen-auto-file disc api-name create read update delete)
    (define (gen) (gen-types disc api-name create read update delete))
    (define (shim) (gen-shim api-name (api-resource-keys disc)))
    (with-output-to-file #:exists 'replace (format "types/gcp/_auto/~a.mrv" api-name) gen)
    (with-output-to-file #:exists 'replace (format "types/gcp/~a-shim.mrv" api-name) shim))

  ; (gen-auto-file storage "storage" "insert" "get" "patch" "delete")
  (gen-auto-file compute "compute" "insert" "get" "patch" "delete")
  ; (gen-auto-file iam "iam" "create" "get" "patch" "delete")
  ; (gen-auto-file secretmanager "secretmanager" "create" "get" "patch" "delete")
  ; (gen-auto-file sql "sql" "insert" "get" "patch" "delete")
  )

; (gen-types compute "compute" "insert" "get" "patch" "delete")