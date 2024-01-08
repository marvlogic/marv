#lang racket/base

(provide (all-defined-out))

(require racket/string)
(require marv/drivers/gcp/discovery)

(define compute  (load-discovery "gcp" "compute:beta"))
(define iam  (load-discovery "gcp" "iam:v1"))
(define storage  (load-discovery "gcp" "storage:v1"))
(define secretmanager  (load-discovery "gcp" "secretmanager:v1"))
(define sql  (load-discovery "gcp" "sqladmin:v1"))


(define (gen-types types prefix create read update delete)
  (displayln #<<EOF
#lang marv
## AUTO-GENERATED FILE - DO NOT EDIT!
EOF
             )
  (for ([t (in-list types)])
    (define (api-id verb) (format "~a.~a.~a" prefix t verb))
    (displayln
     (format
      #<<EOF
type ~a = {
  origin(cfg)= cfg <- { driver="gcp" api="~a" }
  create(cfg)={ api-id="~a" config=cfg }
  read(cfg)={ api-id="~a" config=cfg }
  update(cfg)={ api-id="~a" config=cfg}
  delete(cfg)={ api-id="~a" config={ name=cfg.name } }
}
export ~a

EOF
      t (api-id create) (api-id create) (api-id read) (api-id update) (api-id delete) t
      ))))

(define (singularise str)
  (cond [(string-suffix? str "ies") (string-append (string-trim str "ies") "y")]
        [else (string-trim #:left? #f str "s")]))

(define (gen-shim api-name types)
  (displayln (format #<<EOF
#lang marv

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

  (define (gen-auto-file api api-name create read update delete)
    (define (gen) (gen-types (api-resource-keys api) api-name create read update delete))
    (define (shim) (gen-shim api-name (api-resource-keys api)))
    (with-output-to-file #:exists 'replace (format "types/gcp/_auto/~a.mrv" api-name) gen)
    (with-output-to-file #:exists 'replace (format "types/gcp/~a-shim.mrv" api-name) shim))

  (gen-auto-file storage "storage" "insert" "get" "patch" "delete")
  (gen-auto-file compute "compute" "insert" "get" "patch" "delete")
  (gen-auto-file iam "iam" "create" "get" "patch" "delete")
  (gen-auto-file secretmanager "secretmanager" "create" "get" "patch" "delete")
  (gen-auto-file sql "sql" "insert" "get" "patch" "delete")
  )
