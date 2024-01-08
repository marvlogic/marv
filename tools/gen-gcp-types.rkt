#lang racket/base

(provide (all-defined-out))

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
  origin(cfg)= cfg <- { driver="gcp" api="~a" name=cfg.name }

  create(cfg)={ api-id="~a" config=cfg }
  read(cfg)={ api-id="~a" config=cfg }
  update(cfg)={ api-id="~a" config=cfg}
  delete(cfg)={ api-id="~a" config={ name=cfg.name } }
}
export ~a

EOF
      t (api-id create) (api-id create) (api-id read) (api-id update) (api-id delete) t
      ))))


;(with-output-to-file )

(define (gen-storage) (gen-types (api-resource-keys storage) "storage" "insert" "get" "patch" "delete"))
