#lang racket/base

(provide (all-defined-out))

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
