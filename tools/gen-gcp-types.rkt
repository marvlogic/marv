#lang racket/base

(provide (all-defined-out))

(require marv/drivers/gcp/discovery)

(define compute  (load-discovery "gcp" "compute:beta"))
(define iam  (load-discovery "gcp" "iam:v1"))
(define storage  (load-discovery "gcp" "storage:v1"))
(define secretmanager  (load-discovery "gcp" "secretmanager:v1"))
(define sql  (load-discovery "gcp" "sqladmin:v1"))

(define (gen-types base-type-map)
  (displayln #<<EOF
#lang marv
import "../common.mrv"

EOF
             )
  (for ([kv (in-list (hash->list base-type-map))])
    (define-values (typ at) (values (car kv) (cdr kv)))
    (displayln
     (format
      #<<EOF
type gcp:~a = {
  create = ~a.insert { identity identity }
  read = ~a.get { identity identity }
  update = ~a.patch { identity identity }
  delete = ~a.delete { identity identity }
}

EOF
      typ at at at at
      ))))


;(with-output-to-file )